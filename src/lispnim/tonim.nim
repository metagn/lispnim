import types, nuance/node, strutils

proc node(kind: UntypedNodeKind, info: LineInfo, val: BiggestInt): UntypedNode =
  result = UntypedNode(kind: kind, info: info)
  assert kind in intValNodeKinds
  result.intVal = val
proc node(kind: UntypedNodeKind, info: LineInfo, val: BiggestUint): UntypedNode =
  result = UntypedNode(kind: kind, info: info)
  assert kind in uintValNodeKinds
  result.uintVal = val
proc node(kind: UntypedNodeKind, info: LineInfo, val: BiggestFloat): UntypedNode =
  result = UntypedNode(kind: kind, info: info)
  assert kind in floatValNodeKinds
  result.floatVal = val
proc node(kind: UntypedNodeKind, info: LineInfo, val: string): UntypedNode =
  result = UntypedNode(kind: kind, info: info)
  assert kind in strValNodeKinds
  result.strVal = val
proc node(kind: UntypedNodeKind, info: LineInfo, val: sink seq[UntypedNode]): UntypedNode =
  result = UntypedNode(kind: kind, info: info)
  assert kind notin intValNodeKinds + uintValNodeKinds + floatValNodeKinds + strValNodeKinds
  result.children = val
proc node(kind: UntypedNodeKind, info: LineInfo, val: varargs[UntypedNode]): UntypedNode =
  node(kind, info, @val)

proc toNim*(lisp: Lisp): UntypedNode =
  let topInfo = lisp.info
  case lisp.kind
  of Number:
    let n = lisp.num
    if '.' in n:
      result = node(nnkFloatLit, topInfo, parseFloat(n))
    elif n[0] == '-':
      result = node(nnkIntLit, topInfo, parseBiggestInt(n))
    else:
      var u = parseBiggestUInt(n)
      if u > high(BiggestInt).BiggestUint:
        result = node(nnkUIntLit, topInfo, u)
      else:
        result = node(nnkIntLit, topInfo, u.BiggestInt)
  of String:
    result = node(nnkStrLit, topInfo, lisp.str)
  of Character:
    result = node(nnkCharLit, topInfo, lisp.ch.int)
  of Atom:
    let a = lisp.atom
    case a
    of "nil":
      result = node(nnkNilLit, topInfo)
    else:
      result = node(nnkIdent, topInfo, a)
  of List:
    proc map(a: openarray[Lisp]): seq[UntypedNode] =
      result = newSeq[UntypedNode](a.len)
      for i in 0 ..< a.len:
        result[i] = toNim(a[i])
    let cn = lisp.children
    if cn.len == 0:
      result = node(nnkTupleConstr, topInfo)
    elif cn[0].kind == Atom:
      let a = cn[0].atom
      template args: untyped = cn.toOpenArray(1, cn.len - 1)
      case a
      of "": # ``
        result = node(nnkAccQuoted, topInfo, map(args))
      of "do":
        result = node(nnkStmtList, topInfo, map(args))
      of "tuple!":
        result = node(nnkTupleConstr, topInfo, map(args))
      of "object!":
        var nodes = newSeq[UntypedNode](lisp.children.len - 1)
        for i in 1 ..< lisp.children.len:
          let lc = lisp.children[i]
          if i != 1 and lc.kind == List and lc.children.len == 2:
            nodes[i - 1] = node(nnkExprColonExpr, lc.info, toNim(lc.children[0]), toNim(lc.children[1]))
          else:
            nodes[i - 1] = toNim(lc)
        result = node(nnkObjConstr, topInfo, nodes)
      of "array!":
        result = node(nnkBracket, topInfo, map(args))
      of "seq!":
        result = node(nnkPrefix, topInfo, node(nnkIdent, topInfo, "@"), node(nnkBracket, topInfo, map(args)))
      of "set!":
        result = node(nnkCurly, topInfo, map(args))
      of "comment":
        if cn.len == 2 and cn[1].kind == String:
          result = node(nnkCommentStmt, topInfo, cn[1].str)
      of "cast":
        if cn.len == 3:
          result = node(nnkCast, topInfo, toNim(cn[1]), toNim(cn[2]))
      of "[]":
        result = node(nnkBracketExpr, topInfo, map(args))
      of "deref":
        if cn.len == 2:
          result = node(nnkDerefExpr, topInfo, toNim(cn[1]))
      of "break":
        if cn.len == 1:
          result = node(nnkBreakStmt, topInfo, node(nnkEmpty, topInfo))
        elif cn.len == 2:
          result = node(nnkBreakStmt, topInfo, toNim(cn[1]))
      of "continue":
        if cn.len == 1:
          result = node(nnkContinueStmt, topInfo)
      of "defer":
        if cn.len == 2:
          result = node(nnkDefer, topInfo, toNim(cn[1]))
      of "block":
        if cn.len == 2:
          result = node(nnkBlockStmt, topInfo, node(nnkEmpty, topInfo), toNim(cn[1]))
        elif cn.len == 3:
          result = node(nnkBlockStmt, topInfo, toNim(cn[1]), toNim(cn[2]))
      of "raise":
        if cn.len == 1:
          result = node(nnkRaiseStmt, topInfo, node(nnkEmpty, topInfo))
        elif cn.len == 2:
          result = node(nnkRaiseStmt, topInfo, toNim(cn[1]))
      of "return":
        if cn.len == 1:
          result = node(nnkReturnStmt, topInfo, node(nnkEmpty, topInfo))
        elif cn.len == 2:
          result = node(nnkReturnStmt, topInfo, toNim(cn[1]))
      of "yield":
        if cn.len == 1:
          result = node(nnkYieldStmt, topInfo, node(nnkEmpty, topInfo))
        elif cn.len == 2:
          result = node(nnkYieldStmt, topInfo, toNim(cn[1]))
      of "discard":
        if cn.len == 1:
          result = node(nnkDiscardStmt, topInfo, node(nnkEmpty, topInfo))
        elif cn.len == 2:
          result = node(nnkDiscardStmt, topInfo, toNim(cn[1]))
      of "mixin":
        result = node(nnkMixinStmt, topInfo, map(args))
      of "bind":
        result = node(nnkBindStmt, topInfo, map(args))
      of "asm":
        result = node(nnkAsmStmt, topInfo, map(args))
      of "include":
        result = node(nnkIncludeStmt, topInfo, map(args))
      of "import":
        result = node(nnkImportStmt, topInfo, map(args))
      of "from-import":
        result = node(nnkFromStmt, topInfo, map(args))
      of "import-except":
        result = node(nnkImportExceptStmt, topInfo, map(args))
      of "export":
        result = node(nnkExportStmt, topInfo, map(args))
      of "ref":
        if cn.len < 3:
          result = node(nnkRefTy, topInfo, map(args))
      of "ptr":
        if cn.len < 3:
          result = node(nnkPtrTy, topInfo, map(args))
      of "out":
        if cn.len < 3:
          result = node(nnkMutableTy, topInfo, map(args))
      of "distinct":
        if cn.len < 3:
          result = node(nnkDistinctTy, topInfo, map(args))
      of "var-type":
        if cn.len < 3:
          result = node(nnkVarTy, topInfo, map(args))
      of "tuple":
        if cn.len == 1:
          result = node(nnkTupleClassTy, topInfo)
        elif cn.len == 2 and cn[1].kind == List and cn[1].children.len == 0:
          result = node(nnkTupleTy, topInfo)
        else:
          var nodes = newSeq[UntypedNode](cn.len - 1)
          for i in 1 ..< cn.len:
            let n = cn[i]
            if n.kind == List and n.children.len > 1:
              var defNodes = newSeq[UntypedNode](n.children.len + 1)
              for i in 0 ..< n.children.len:
                defNodes[i] = toNim(n.children[i])
              defNodes[n.children.len] = node(nnkEmpty, n.info)
              nodes.add(node(nnkIdentDefs, n.info, defNodes))
            else:
              nodes.add(toNim(n))
          result = node(nnkTupleTy, topInfo, nodes)
      of ":":
        if cn.len == 3:
          result = node(nnkExprColonExpr, topInfo, map(args))
      of ".":
        result = node(nnkDotExpr, topInfo, map(args))
    if result.kind == nnkNone:
      var nodes = newSeq[UntypedNode](cn.len)
      for i in 0 ..< cn.len:
        let n = cn[i]
        if i != 0 and n.kind == List and n.children.len == 3 and
          n.children[0].kind == Atom and n.children[0].atom == "=":
          nodes[i] = node(nnkExprEqExpr, n.info, toNim(n.children[1]), toNim(n.children[2]))
        else:
          nodes[i] = toNim(n)
      result = node(nnkCall, topInfo, nodes)

# node todo:
  # nnkAsgn
  # nnkVarSection, nnkLetSection, nnkConstSection
  # nnkVarTuple
  # nnkPragmaExpr in definitions
  # nnkProcDef, nnkFuncDef, nnkMethodDef, nnkConverterDef, nnkMacroDef, nnkTemplateDef, nnkIteratorDef
  # nnkGenericParams
  # nnkFormalParams
  # nnkPattern
  # nnkPragma
  # nnkLambda
  # nnkDo
  # nnkTypeSection
  # nnkTypeDef
  # nnkImportAs
  # nnkUsingStmt
  # nnkTypeClassTy concept
  # nnkProcTy, nnkIteratorTy
  # nnkObjectTy
    # nnkOfInherit
    # nnkRecList
    # nnkRecCase
    # nnkRecWhen
  # nnkEnumTy
    # nnkEnumFieldDef
  # nnkPragmaBlock
  # nnkPostfix
  # nnkIfStmt, nnkWhenStmt, nnkWhileStmt, nnkCaseStmt, nnkTryStmt
  # nnkOfBranch
  # nnkElifBranch
  # nnkElse
  # nnkExceptBranch
  # nnkFinally
  # nnkForStmt
  # nnkCommand, nnkInfix, nnkPrefix ? nnkDotCall
  # nnkStmtListType, nnkBlockType
