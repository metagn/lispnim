import nuance/node

type
  LispKind* = enum
    List, Atom, Number, String, Character
  Lisp* = object
    info*: LineInfo
    case kind*: LispKind
    of List: children*: seq[Lisp]
    of Atom: atom*: string
    of Number: num*: string
    of String: str*: string
    of Character: ch*: char
