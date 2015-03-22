import future
import unittest
import optionals
import pipelines

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
