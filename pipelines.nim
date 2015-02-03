import algorithm
import macros
import strutils

import optionals

#===============================================================================
# Expression Referencing/Dereferencing

var exprNodes {.compileTime.} = newSeq[PNimrodNode]()

proc refExpr(exprNode: PNimrodNode): string {.compileTime.} =
  exprNodes.add exprNode
  "expr" & $(exprNodes.len - 1)

proc derefExpr(exprRef: string): PNimrodNode {.compileTime.} =
  exprNodes[parseInt(exprRef[4 .. -1])]

#===============================================================================
# AST Manipulation

proc newBracketExpr(args: varargs[PNimrodNode]): PNimrodNode {.compileTime.} =
  newNimNode(nnkBracketExpr).add(args)

#===============================================================================
# Tuple Operations

template len(t: tuple): int =
  template getLen(i: static[int]): expr {.genSym.} =
    when compiles(t[i]): getLen(i + 1) else: i
  getLen 0

template readAll(t: tuple): expr =
  macro buildResult(tExpr: tuple): expr {.genSym.} =
    result = newPar()
    for i in 0 .. <t.len:
      result.add(newColonExpr(
        ident("field" & $i),
        newCall(ident("read"), newBracketExpr(tExpr, newLit(i)))))
  buildResult t

template allHaveValue(t: tuple): expr =
  let tVal = t; const tHigh = tVal.len - 1
  template level(i: static[int]): expr {.genSym.} =
    when i == tHigh + 1: true
    else: tVal[tHigh - i].hasValue and level(i + 1)
  level 0

template values(t: tuple): expr =
  macro buildResult(tExpr: tuple): expr {.genSym.} =
    result = newPar()
    for i in 0 .. <t.len:
      result.add(newColonExpr(
        ident("field" & $i),
        newDotExpr(
          newBracketExpr(tExpr, newLit(i)),
          ident("value"))))
  buildResult t

#===============================================================================
# Type Classes

type Source = generic S
  var s: S; compiles(s.typeClassTag_Source)

type Sink = generic S
  var s: S; compiles(s.typeClassTag_Sink)

template Element*(S: typedesc[Source]): typedesc =
  var source {.noInit.}: S
  type(source.read.value)

template Element*(source: Source): typedesc =
  var mSource {.noInit.}: type(source)
  type(mSource.read.value)

#===============================================================================
# Sequence-Based Sources/Sinks

type SeqSource* {.shallow.} [E] = object
  buffer: seq[E]
  first, last: ptr E
  index, len: int
  typeClassTag_Source: type(())

type SeqSink* {.shallow.} [E] = object
  buffer: seq[E]
  first, last: ptr E
  index, len: int
  typeClassTag_Sink: type(())

proc newSeqSource*[E](s: openarray[E]): SeqSource[E] =
  result.buffer = @s
  if result.buffer.len > 0:
    result.first = addr result.buffer[0]
    result.last = addr result.buffer[s.len - 1]
  result.index = 0
  result.len = s.len

proc newSeqSink*[E](s: var seq[E]): SeqSource[E] =
  result.buffer.shallowCopy s
  if result.buffer.len > 0:
    result.first = addr result.buffer[0]
    result.last = addr result.buffer[s.len - 1]
  result.index = 0
  result.len = s.len

converter toSource*[R, E](s: array[R, E]): SeqSource[E] =
  newSeqSource s

converter toSource*[E](s: seq[E]): SeqSource[E] =
  newSeqSource s

converter toSink*[E](s: seq[E]): SeqSink[E] =
  result.buffer.shallowCopy s
  result.index = 0
  result.len = s.len

proc read*[E](source: var SeqSource[E]): Optional[E] {.inline, noInit.} =
  if source.index < source.len:
    cast[ptr bool](addr(result))[] = true
    result.value = source.buffer[source.index]
    source.index += 1
  else:
    cast[ptr bool](addr(result))[] = false

proc write*[E](sink: var SeqSink[E], element: E) =
  sink.buffer[sink.index] = element
  sink.index += 1

#===============================================================================
# Generic Source/Sink Operations

iterator items*(source: Source): any =
  when compiles(type(system.items(source))):
    for e in system.items(source):
      yield e
  else:
    var mSource = source
    while true:
      let e = mSource.read
      if e.hasValue: yield e.value
      else: break

proc collect*[Source](source: Source): any =
  result = newSeq[source.items.type]()
  for e in source:
    result.add e

proc readInto*(source: Source, sink: Sink) =
  var mSink = sink
  for e in source:
    mSink.write e

template fold*(source: Source, op: expr): expr =
  var mSource = source
  let e0 = mSource.read
  let e1 = mSource.read
  assert e0.hasValue and e1.hasValue
  var result = op(e0.value, e1.value)
  for e in mSource:
    result = op(result, e)
  result

template fold*(source: Source, op, init: expr): expr =
  var mSource = source
  var result = init
  for e in mSource:
    result = op(result, e)
  result

type Taken*[S: Source] = object
  source: S
  n: int
  typeClassTag_Source: type(())

proc take*(source: Source, n: int): auto =
  Taken[type(source)](source: source, n: n)

proc read*(s: var Taken): auto {.noInit.} =
  if s.n > 0:
    result = s.source.read
    assert result.hasValue
    s.n -= 1
  else:
    result = nothing(type(s.source.read.value))

type Mapped*[S: Source; op: static[string]] = object
  source: S
  typeClassTag_Source: type(())

proc mapProc(source: Source, op: static[string]): auto =
  var result: Mapped[type(source), op]
  result.source = source
  result

macro map*(source: Source, op: expr): expr =
  newCall(bindSym"mapProc", source, newLit(refExpr(op)))

proc read*(source: var Mapped): auto {.noInit.} =
  macro getOp: expr =
    derefExpr source.op
  let op = getOp()
  let e = source.source.read
  var result {.noInit.}: Optional[type(op(e.value))]
  if e.hasValue:
    cast[ptr bool](addr(result))[] = true
    result.value = getOp()(e.value)
  else:
    cast[ptr bool](addr(result))[] = false
  result

type Filtered*[S; op: static[string]] = object
  source: S
  typeClassTag_Source: type(())

proc filterProc(source: Source, op: static[string]): auto =
  var result: Filtered[type(source), op]
  result.source = source
  result

macro filter*(source: Source, op: expr): expr =
  newCall(bindSym"filterProc", source, newLit(refExpr(op)))

proc read*(source: var Filtered): auto =
  macro getOp: expr =
    derefExpr source.op
  while true:
    let e = source.source.read
    if not e.hasValue:
      return e
    elif getOp()(e.value):
      return e

type Joined*[E; S: Source] = object
  frontSource: Optional[E]
  backSources: S
  typeClassTag_Source: type(())

proc join*[S: Source](sources: S): auto =
  var result: Joined[Element(type(sources)), type(sources)]
  result.backSources = sources
  result.frontSource = result.backSources.read
  result

proc `&`*(source0, source1: Source): auto =
  join([source0, source1])

proc read*(sources: var Joined): auto =
  while true:
    if not sources.frontSource.hasValue:
      return nothing(type(sources.frontSource.value.read.value))
    else:
      let e = sources.frontSource.value.read
      if e.hasValue:
        return e
      else:
        sources.frontSource = sources.backSources.read

type Zipped*[Sources: tuple] = object
  sources: Sources
  typeClassTag_Source: type(())

proc zipProc(sources: tuple): auto =
  Zipped[type(sources)](sources: sources)

macro zip*(source0: Source, otherSources: varargs[expr]): expr =
  let args = newPar(newColonExpr(ident"field0", source0))
  for i in 0 .. <otherSources.len:
    args.add(newColonExpr(ident("field" & $(i + 1)), otherSources[i]))
  newCall(bindSym"zipProc", args)

proc read*(sources: var Zipped): auto {.noInit.} =
  let readResults = readAll sources.sources
  var result {.noInit.}: Optional[type(readResults.values)]
  if readResults.allHaveValue:
    cast[ptr bool](addr(result))[] = true
    result.value = readResults.values
  else:
    cast[ptr bool](addr(result))[] = false
  result

#===============================================================================
# Tests

when isMainModule:
  import future

template test(name: expr, action: stmt): stmt =
  when isMainModule and not defined(release):
      try:
        block: action
        echo "Test succeeded: \"", name, "\"."
      except AssertionError:
        echo "Test failed: \"", name, "\"."
        stderr.write(getCurrentException().getStackTrace())

test "source.read":
  block:
    var source = newSeq[int]().toSource
    assert source.read == nothing(int)
  block:
    var source = ["0", "1"].toSource
    assert source.read == "0"
    assert source.read == "1"
    assert source.read == nothing(string)
  block:
    var source = [0, 1, 2].toSource
    assert source.read == 0
    assert source.read == 1
    assert source.read == 2
    assert source.read == nothing(int)

test "source.items":
  block:
    var i = 0
    for e in newSeq[int]().toSource:
      i += 1
    assert i == 0
  block:
    var i = 0
    for e in ["0", "1"].toSource:
      assert e == $i
      i += 1
    assert i == 2
  block:
    var i = 0
    for e in [0, 1, 2].toSource:
      assert e == i
      i += 1
    assert i == 3

test "source.collect":
  assert(newSeq[int]().collect == newSeq[int]())
  assert(["0", "1"].collect == @["0", "1"])
  assert([0, 1, 2].collect == @[0, 1, 2])

test "source.readInto(sink)":
  block:
    let a = newSeq[int]()
    var b = newSeq[int]()
    a.readInto b
    assert b == newSeq[int]()
  block:
    let a = @[0, 1, 2]
    var b = @[0, 0, 0]
    a.readInto b
    assert b == @[0, 1, 2]
  block:
    let a = @["0", "1"]
    var b = @["", "", ""]
    a.readInto b
    assert b == @["0", "1", ""]

test "source.fold(op)":
  assert([0, 1].fold(`+`) == 1)
  assert([0, 1, 2].fold(`+`) == 3)
  assert([0, 1].fold(`-`) == -1)
  assert([0, 1, 2].fold(`-`) == -3)
  assert(["0", "1"].fold(`&`) == "01")
  assert(["0", "1", "2"].fold(`&`) == "012")

test "source.fold(op, init)":
  assert(newSeq[int]().fold(`+`, 0) == 0)
  assert([1].fold(`+`, 0) == 1)
  assert([1, 2].fold(`+`, 0) == 3)
  assert(newSeq[int]().fold(`-`, 0) == 0)
  assert([1].fold(`-`, 0) == -1)
  assert([1, 2].fold(`-`, 0) == -3)
  assert(newSeq[string]().fold(`&`, "0") == "0")
  assert(["1"].fold(`&`, "0") == "01")
  assert(["1", "2"].fold(`&`, "0") == "012")

test "source.take(n)":
  assert(newSeq[int]().take(0).collect == newSeq[int]())
  assert(["0", "1"].take(0).collect == newSeq[string]())
  assert(["0", "1"].take(1).collect == @["0"])
  assert(["0", "1"].take(2).collect == @["0", "1"])
  assert([0, 1, 2].take(0).collect == newSeq[int]())
  assert([0, 1, 2].take(1).collect == @[0])
  assert([0, 1, 2].take(2).collect == @[0, 1])
  assert([0, 1, 2].take(3).collect == @[0, 1, 2])

test "source.map(op)":
  assert(newSeq[int]().map((e: int) => e + 1).collect == newSeq[int](0))
  assert(["0", "1"].map((e: string) => e & e).collect == @["00", "11"])
  assert([0, 1, 2].map((e: int) => e + 1).collect == @[1, 2, 3])

test "source.filter(op)":
  assert(newSeq[int]().filter((e: int) => e > 0).collect == newSeq[int]())
  assert(["0", "1"].filter((e: string) => e == "0").collect == @["0"])
  assert([0, 1, 2].filter((e: int) => e > 0).collect == @[1, 2])

test "sources.join":
  block:
    assert newSeq[SeqSource[int]]().join.collect == newSeq[int]()
  block:
    let a = @[0, 1].toSource
    assert([a].join.collect == @[0, 1])
  block:
    let a = @[0, 1].toSource
    let b = newSeq[int]().toSource
    assert([a, b].join.collect == @[0, 1])
  block:
    let a = @["0", "1"].toSource
    let b = newSeq[string]().toSource
    let c = @["2"].toSource
    assert([a, b, c].join.collect == @["0", "1", "2"])

test "source0 & source1":
  block:
    let a = @[0, 1].toSource
    let b = newSeq[int]().toSource
    assert(collect(a & b) == @[0, 1])
  block:
    let a = @["0", "1"].toSource
    let b = @["2"].toSource
    assert(collect(a & b) == @["0", "1", "2"])

test "zip(source0)":
  block:
    let a = newSeq[int]().toSource
    assert zip(a).collect == newSeq[tuple[field0: int]]()
  block:
    let a = @[0, 1].toSource
    assert zip(a).collect == @[(field0: 0), (field0: 1)]
  block:
    let a = @[0, 1, 2].toSource
    assert zip(a).collect == @[(field0: 0), (field0: 1), (field0: 2)]

test "zip(source0, source1)":
  block:
    let a = newSeq[int]().toSource
    let b = newSeq[float]().toSource
    assert zip(a, b).collect == newSeq[tuple[field0: int, field1: float]]()
  block:
    let a = @[0, 1].toSource
    let b = @[0.0, 1.0].toSource
    assert zip(a, b).collect == @[(0, 0.0), (1, 1.0)]
  block:
    let a = @[0, 1, 2].toSource
    let b = @[0.0, 1.0].toSource
    assert zip(a, b).collect == @[(0, 0.0), (1, 1.0)]

test "zip(source0, source1, source2)":
  block:
    let a = newSeq[int]().toSource
    let b = newSeq[float]().toSource
    let c = newSeq[string]().toSource
    assert zip(a, b, c).collect ==
           newSeq[tuple[field0: int, field1: float, field2: string]]()
  block:
    let a = @[0, 1].toSource
    let b = @[0.0, 1.0].toSource
    let c = @["0", "1"].toSource
    assert zip(a, b, c).collect == @[(0, 0.0, "0"), (1, 1.0, "1")]
  block:
    let a = @[0, 1, 2].toSource
    let b = @[0.0, 1.0].toSource
    let c = @["0"].toSource
    assert zip(a, b, c).collect == @[(0, 0.0, "0")]

#===============================================================================
# Benchmarks

when isMainModule:
  import times

template benchmark(action: stmt): stmt =
  when isMainModule and defined(release):
    block: action

template measure(name: expr, action: stmt): stmt =
  when isMainModule and defined(release):
    let t0 = cpuTime()
    for i in 0 .. <1000:
      action
    let t1 = cpuTime()
    echo "Average duration for \"", name, "\": ", t1 - t0, "ms."

benchmark:
  var a = newSeq[int](1_000_000)
  var b = newSeq[int](1_000_000)
  var c = newSeq[int](1_000_000)
  var aSnk = a.toSink
  var bSrc = b.toSource
  var cSrc = c.toSource
  measure "copy - loop":
    for i in 0 .. <a.len:
      a[i] = b[i]
  measure "copy - pipeline":
    bSrc.readInto aSnk
  measure "add 1 - loop":
    for i in 0 .. <a.len:
      a[i] = b[i] + 1
  measure "add 1 - pipeline":
    bSrc.map((e: int) => e + 1).readInto aSnk
  measure "multiply - loop":
    for i in 0 .. <a.len:
      a[i] = b[i] * c[i]
  measure "multiply - pipeline":
    zip(bSrc, cSrc).map((e: tuple[field0:int,field1:int]) =>
      e[0] * e[1]).readInto(aSnk)
