import future
import times
import pipelines

template benchmark(name: expr, action: stmt): stmt =
  when isMainModule and defined(release):
    let t0 = cpuTime()
    for i in 0 .. <1000:
      action
    let t1 = cpuTime()
    echo "Average duration for \"", name, "\": ", t1 - t0, "ms."

var a = newSeq[int](1_000_000)
var b = newSeq[int](1_000_000)
var c = newSeq[int](1_000_000)
var aSnk = a.toSink
var bSrc = b.toSource
var cSrc = c.toSource

benchmark "copy - loop":
  for i in 0 .. <a.len:
    a[i] = b[i]

benchmark "copy - pipeline":
  bSrc.readInto aSnk

benchmark "add 1 - loop":
  for i in 0 .. <a.len:
    a[i] = b[i] + 1

benchmark "add 1 - pipeline":
  bSrc.map((e: int) => e + 1).readInto aSnk

benchmark "multiply - loop":
  for i in 0 .. <a.len:
    a[i] = b[i] * c[i]

benchmark "multiply - pipeline":
  zip(bSrc, cSrc).map((e: tuple[field0:int,field1:int]) =>
    e[0] * e[1]).readInto(aSnk)
