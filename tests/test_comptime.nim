when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import lispnim, nuance/comptime

load(toNim(parseLisp(staticRead("./module.lispnim"), "tests/module.lispnim")))

test "compiletime works":
  check foo(3, 7) == 24

import macros
from strutils import endsWith

test "info is kept":
  macro fooFilename(): string =
    newLit(getImpl(bindSym"foo").lineInfoObj.filename)
  check fooFilename().endsWith("module.lispnim")
