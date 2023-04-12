# Package

version       = "0.1.0"
author        = "metagn"
description   = "lisp version of nim"
license       = "MIT"
srcDir        = "src"
installExt    = @["nim"]


# Dependencies

requires "nim >= 1.6.12"
requires "nuance"

when (compiles do: import nimbleutils):
  import nimbleutils

task docs, "build docs for all modules":
  when declared(buildDocs):
    buildDocs(gitUrl = "https://github.com/metagn/lispnim")
  else:
    echo "docs task not implemented, need nimbleutils"

task tests, "run tests for multiple backends":
  when declared(runTests):
    runTests(backends = {c, js, nims})
    runTests("tests/generated", backends = {c, js, nims})
  else:
    echo "tests task not implemented, need nimbleutils"
