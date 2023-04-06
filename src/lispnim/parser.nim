import types, strutils, nuance/node

type ParseState* = object
  pos*: int
  fn*: string
  preservePos*: bool
  ln*, cl*: int

proc currentInfo*(p: ParseState): LineInfo =
  LineInfo(filename: p.fn, line: p.ln, column: p.cl)

proc advance*(str: string, p: var ParseState) =
  if p.preservePos:
    p.preservePos = false
  else:
    inc p.pos
    if (p.pos < str.len and str[p.pos] == '\n') or
      (p.pos + 1 < str.len and str[p.pos] == '\r' and str[p.pos + 1] == '\n'):
      inc p.ln
      p.cl = 0
    else:
      inc p.cl

proc parseSingleLisp*(str: string, p: var ParseState): Lisp

proc parseList*(str: string, p: var ParseState): Lisp =
  result = Lisp(info: p.currentInfo, kind: List, children: @[])
  assert p.pos < str.len and str[p.pos] == '('
  advance(str, p)
  while p.pos < str.len:
    let c = str[p.pos]
    case c
    of Whitespace: discard
    of ')': return
    else: result.children.add(parseSingleLisp(str, p))
    advance(str, p)

proc parseAtomString*(str: string, p: var ParseState): string =
  while p.pos < str.len:
    let c = str[p.pos]
    case c
    of Whitespace, ')':
      p.preservePos = true
      return
    else: result.add(c)
    advance(str, p)

proc parseQuotedString*(str: string, p: var ParseState): string =
  assert p.pos < str.len and str[p.pos] in {'\'', '"', '`'}
  let quote = str[p.pos]
  result.add('"')
  advance(str, p)
  var escaped = false
  while p.pos < str.len:
    let c = str[p.pos]
    if escaped:
      escaped = false
      if not (quote != '"' and c == quote):
        result.add('\\')
      result.add(c)
    elif c == '\\':
      escaped = true
    elif c == quote:
      result.add('"')
      return
    else:
      result.add(c)
    advance(str, p)

proc parseSingleLisp*(str: string, p: var ParseState): Lisp =
  while p.pos < str.len:
    let c = str[p.pos]
    case c
    of Whitespace: discard
    of '(': return parseList(str, p)
    of '0'..'9': return Lisp(info: p.currentInfo, kind: Number, num: parseAtomString(str, p))
    of '"', '\'':
      let savedInfo = p.currentInfo
      let s = parseQuotedString(str, p)
      let us = unescape(s)
      if c == '\'' and us.len == 1:
        return Lisp(info: savedInfo, kind: Character, ch: us[0])
      else:
        return Lisp(info: savedInfo, kind: String, str: us)
    of '`':
      let savedInfo = p.currentInfo
      let s = parseQuotedString(str, p)
      let us = unescape(s)
      return Lisp(info: savedInfo, kind: Atom, atom: us)
    else:
      let savedInfo = p.currentInfo
      let isNum = c == '-' and p.pos + 1 < str.len and str[p.pos + 1] in '0'..'9'
      let s = parseAtomString(str, p)
      if isNum:
        return Lisp(info: savedInfo, kind: Number, num: s)
      else:
        return Lisp(info: savedInfo, kind: Atom, atom: s)
    advance(str, p)

proc parseLisp*(str: string, p: var ParseState): seq[Lisp] =
  while p.pos < str.len:
    let c = str[p.pos]
    case c
    of Whitespace: discard
    else: result.add(parseLisp(str, p))
    advance(str, p)

proc init*(str: string, p: var ParseState) =
  p.pos = -1
  p.ln = 1
  p.cl = 0
  advance(str, p)

proc parseLisp*(str: string, filename = ""): seq[Lisp] =
  var p = ParseState(fn: filename)
  init(str, p)
  result = parseLisp(str, p)
