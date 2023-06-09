when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import nuance/tosexp, lispnim

const backendName =
  when defined(nimscript):
    "nims"
  elif defined(js):
    "js"
  else:
    "c"

when defined(js):
  template read(a) =
    bridge.readFile(a)
  template write(a, b) =
    bridge.writeFile(a, b)
else:
  template read(a) =
    readFile(a)
  template write(a, b) =
    writeFile(a, b)

test "generate files":
  const outputFilename = "tests/generated/output_" & backendName & ".tmp"
  when defined(nimscript):
    {.pragma: bugWorkaround, compileTime.}
  else:
    {.pragma: bugWorkaround.}
  let lisp {.bugWorkaround.} = parseLisp(read("tests/module.lispnim"), "tests/module.lispnim")
  write(outputFilename, toSexp(toNim(lisp)))
  write("tests/generated/module_" & backendName & "_generated.nim", """
import nuance/[fromsexp, comptime]

load(parseSexp(staticRead("../../""" & outputFilename & """")))
""")
  write("tests/generated/test_" & backendName & "_generated.nim", """
when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import module_""" & backendName & """_generated

test "generated module works":
  check foo(3, 7) == 24

import macros
from strutils import endsWith

test "info is kept":
  macro fooFilename(): string =
    newLit(getImpl(bindSym"foo").lineInfoObj.filename)
  check fooFilename().endsWith("module.lispnim")
""")
