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
  exprNodes[parseInt(exprRef[4 .. ^1])]

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

type Source* = concept s
  compiles(s.typeClassTag_Source)

type Sink* = concept s
  compiles(s.typeClassTag_Sink)

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
  typeClassTag_Source*: byte

type SeqSink* {.shallow.} [E] = object
  buffer: seq[E]
  first, last: ptr E
  index, len: int
  typeClassTag_Sink*: byte

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
  result = newSeq[type(source.items)]()
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
  var res = op(e0.value, e1.value)
  for e in mSource:
    res = op(res, e)
  res

template fold*(source: Source, op, init: expr): expr =
  var mSource = source
  var res = init
  for e in mSource:
    res = op(res, e)
  res

type Taken*[S: Source] = object
  source: S
  n: int
  typeClassTag_Source*: byte

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
  typeClassTag_Source*: byte

proc mapProc(source: Source, op: static[string]): auto =
  var res: Mapped[type(source), op]
  res.source = source
  res

macro map*(source: Source, op: expr): expr =
  newCall(bindSym"mapProc", source, newLit(refExpr(op)))

proc read*(source: var Mapped): auto {.noInit.} =
  macro getOp: expr =
    derefExpr source.op
  let op = getOp()
  let e = source.source.read
  var res {.noInit.}: Optional[type(op(e.value))]
  if e.hasValue:
    cast[ptr bool](addr(res))[] = true
    res.value = getOp()(e.value)
  else:
    cast[ptr bool](addr(res))[] = false
  res

type Filtered*[S; op: static[string]] = object
  source: S
  typeClassTag_Source*: byte

proc filterProc(source: Source, op: static[string]): auto =
  var res: Filtered[type(source), op]
  res.source = source
  res

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
  typeClassTag_Source*: byte

proc join*[S: Source](sources: S): auto =
  var res: Joined[Element(type(sources)), type(sources)]
  res.backSources = sources
  res.frontSource = res.backSources.read
  res

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
  typeClassTag_Source*: byte

proc zipProc(sources: tuple): auto =
  Zipped[type(sources)](sources: sources)

macro zip*(source0: Source, otherSources: varargs[expr]): expr =
  let args = newPar(newColonExpr(ident"field0", source0))
  for i in 0 .. <otherSources.len:
    args.add(newColonExpr(ident("field" & $(i + 1)), otherSources[i]))
  newCall(bindSym"zipProc", args)

proc read*(sources: var Zipped): auto {.noInit.} =
  let readress = readAll sources.sources
  var res {.noInit.}: Optional[type(readress.values)]
  if readress.allHaveValue:
    cast[ptr bool](addr(res))[] = true
    res.value = readress.values
  else:
    cast[ptr bool](addr(res))[] = false
  res
