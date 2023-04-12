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
      when defined(js):
        result = node(nnkIntLit, topInfo, u.BiggestInt)
      else:
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
      if a[0] in IdentStartChars and '-' in a:
        # kebab case insensitive
        result = node(nnkIdent, topInfo, a.replace('-', '_'))
      else:
        result = node(nnkIdent, topInfo, a)
  of List:
    proc map(a: openarray[Lisp]): seq[UntypedNode] =
      result = newSeq[UntypedNode](a.len)
      for i in 0 ..< a.len:
        result[i] = toNim(a[i])
    let cn = lisp.children
    if cn.len == 0:
      result = node(nnkEmpty, topInfo)
    elif cn[0].kind == Atom:
      proc ident(a: Lisp, inFor: static bool = false): UntypedNode =
        proc asVarTuple(x: Lisp, inFor: static bool = false): UntypedNode =
          if x.kind == List:
            var nodes = newSeq[UntypedNode](x.children.len + 2 - ord(inFor))
            for i in 0 ..< x.children.len:
              nodes[i] = asVarTuple(x.children[i])
            nodes[x.children.len] = node(nnkEmpty, x.info)
            when not inFor:
              nodes[x.children.len + 1] = node(nnkEmpty, x.info)
            result = node(nnkVarTuple, x.info, nodes)
          else:
            result = toNim(x)
        if a.kind == List and a.children.len != 0:
          result = asVarTuple(a.children[0], inFor)
          for i in 1 ..< a.children.len:
            let x = a.children[i]
            if x.kind == Atom and x.atom == "*":
              result = node(nnkPostfix, a.info, node(nnkIdent, x.info, "*"), result)
            if x.kind == List and x.children.len != 0 and
              x.children[0].kind == Atom and x.children[0].atom == "pragma":
              result = node(nnkPragmaExpr, a.info, result, node(nnkPragma, x.info, map(x.children.toOpenArray(1, x.children.len - 1))))
        else: result = toNim(a)
      proc typeBiasedIdentDef(n: Lisp): UntypedNode =
        if n.kind == List:
          let len = n.children.len
          let hasDefault = n.children[len - 1].kind == List and
            n.children[len - 1].children.len == 2 and
            n.children[len - 1].children[0].kind == Atom and
            n.children[len - 1].children[0].atom == "="
          if hasDefault:
            var defNodes = newSeq[UntypedNode](len)
            for i in 0 ..< len - 2:
              defNodes[i] = ident(n.children[i])
            let typeNode = n.children[len - 2]
            if typeNode.kind == List and typeNode.children.len == 0:
              defNodes[len - 2] = node(nnkEmpty, typeNode.info)
            else:
              defNodes[len - 2] = toNim(typeNode)
            defNodes[len - 1] = toNim(n.children[len - 1].children[1])
            result = node(nnkIdentDefs, n.info, defNodes)
          else:
            var defNodes = newSeq[UntypedNode](len + 1)
            for i in 0 ..< len - 1:
              defNodes[i] = ident(n.children[i])
            let typeNode = n.children[len - 1]
            if typeNode.kind == List and typeNode.children.len == 0:
              defNodes[len - 1] = node(nnkEmpty, typeNode.info)
            else:
              defNodes[len - 1] = toNim(typeNode)
            defNodes[len] = node(nnkEmpty, n.info)
            result = node(nnkIdentDefs, n.info, defNodes)
        else:
          result = node(nnkIdentDefs, n.info, ident(n), node(nnkEmpty, n.info), node(nnkEmpty, n.info))
      proc procParamList(n: Lisp): UntypedNode =
        if n.kind == List and (let pc = n.children;
            pc.len != 0):
          var paramNodes = newSeq[UntypedNode](pc.len)
          paramNodes[0] = toNim(pc[^1])
          for i in 1 ..< pc.len:
            paramNodes[i] = typeBiasedIdentDef(pc[i - 1])
          node(nnkFormalParams, n.info, paramNodes)
        else:
          # just return type
          node(nnkFormalParams, n.info, toNim(n))
      proc doProc(kind: UntypedNodeKind, info: LineInfo, ns: openarray[Lisp]): UntypedNode =
        if ns.len > 1:
          var name = ident(ns[0])
          var i = 1
          var pattern = node(nnkEmpty, info)
          if i + 1 < ns.len and ns[i].kind == List and (let pnc = ns[i].children;
            pnc.len != 0 and pnc[0].kind == Atom and pnc[0].atom == "pattern"):
            var patternNodes = newSeq[UntypedNode](pnc.len - 1)
            for i in 1 ..< pnc.len:
              patternNodes[i - 1] = typeBiasedIdentDef(pnc[i])
            pattern = node(nnkPattern, ns[i].info, patternNodes)
            inc i
          var generics = node(nnkEmpty, info)
          if i + 1 < ns.len and ns[i].kind == List and (let gc = ns[i].children;
            gc.len != 0 and gc[0].kind == Atom and gc[0].atom == "generics"):
            var genericParamNodes = newSeq[UntypedNode](gc.len - 1)
            for i in 1 ..< gc.len:
              genericParamNodes[i - 1] = typeBiasedIdentDef(gc[i])
            generics = node(nnkGenericParams, ns[i].info, genericParamNodes)
            inc i
          let params = procParamList(ns[i])
          inc i
          var pragma = node(nnkEmpty, info)
          if name.kind == nnkPragmaExpr:
            pragma = name.children[1]
            name = name.children[0]
          elif i < ns.len and ns[i].kind == List and (let pgc = ns[i].children;
            pgc.len != 0 and pgc[0].kind == Atom and pgc[0].atom == "pragma"):
            var pragmaNodes = newSeq[UntypedNode](pgc.len - 1)
            for i in 1 ..< pgc.len:
              pragmaNodes[i - 1] = toNim(pgc[i])
            pragma = node(nnkPragma, ns[i].info, pragmaNodes)
            inc i
          var body = node(nnkEmpty, info)
          if i < ns.len: body = toNim(ns[i])
          result = node(kind, info, name, pattern, generics, params, pragma, node(nnkEmpty, info), body)
      proc procType(kind: UntypedNodeKind, info: LineInfo, ns: openarray[Lisp]): UntypedNode =
        case ns.len
        of 0: result = node(kind, topInfo)
        of 1:
          if ns[0].kind == List and (let pgc = ns[0].children;
            pgc.len != 0 and pgc[0].kind == Atom and pgc[0].atom == "pragma"):
            var pragmaNodes = newSeq[UntypedNode](pgc.len - 1)
            for i in 1 ..< pgc.len:
              pragmaNodes[i - 1] = toNim(pgc[i])
            result = node(kind, topInfo, node(nnkEmpty, topInfo),
              node(nnkPragma, ns[0].info, pragmaNodes))
          else:
            result = node(kind, topInfo, procParamList(ns[0]))
        elif ns[1].kind == List and (let pgc = ns[1].children;
          pgc.len != 0 and pgc[0].kind == Atom and pgc[0].atom == "pragma"):
          let params = procParamList(ns[0])
          var pragmaNodes = newSeq[UntypedNode](pgc.len - 1)
          for i in 1 ..< pgc.len:
            pragmaNodes[i - 1] = toNim(pgc[i])
          result = node(kind, topInfo, params,
            node(nnkPragma, ns[1].info, pragmaNodes))
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
          result = node(when compiles(nnkMutableTy): nnkMutableTy else: nnkOutTy, topInfo, map(args))
      of "distinct":
        if cn.len < 3:
          result = node(nnkDistinctTy, topInfo, map(args))
      of "var-type":
        if cn.len < 3:
          result = node(nnkVarTy, topInfo, map(args))
      of "tuple", "tuple-type":
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
      of "=":
        if cn.len == 2:
          result = node(nnkAsgn, topInfo, map(args))
      of "var", "let", "const":
        var kind, defKind: UntypedNodeKind
        case a
        of "var": kind = nnkVarSection; defKind = nnkIdentDefs
        of "let": kind = nnkLetSection; defKind = nnkIdentDefs
        of "const": kind = nnkConstSection; defKind = nnkConstDef
        block sectioner:
          if kind == nnkVarSection and cn.len == 2 and cn[0].kind != List:
            result = node(nnkVarTy, topInfo, toNim(cn[0]))
            break sectioner
          var nodes = newSeq[UntypedNode](cn.len - 1)
          for i in 1 ..< cn.len:
            let n = cn[i]
            if n.kind == List and n.children.len > 1:
              if n.children.len == 2:
                nodes[i - 1] = node(defKind, n.info, ident(n.children[0]), node(nnkEmpty, n.info), toNim(n.children[1]))
              else:
                var defNodes = newSeq[UntypedNode](n.children.len)
                for i in 0 ..< n.children.len - 2:
                  defNodes[i] = ident(n.children[i])
                defNodes[^2] = toNim(n.children[^2])
                defNodes[^1] = toNim(n.children[^1])
                nodes[i - 1] = node(defKind, n.info, defNodes)
            else:
              break sectioner
          result = node(kind, topInfo, nodes)
      of "enum":
        if cn.len == 1:
          result = node(nnkEnumTy, topInfo)
        else:
          var nodes = newSeq[UntypedNode](lisp.children.len)
          nodes[0] = node(nnkEmpty, lisp.info)
          for i in 1 ..< lisp.children.len:
            let c = lisp.children[i]
            if c.kind == List and c.children.len == 3 and
              c.children[0].kind == Atom and c.children[0].atom == "=":
              nodes[i] = node(nnkEnumFieldDef, c.info, ident(c.children[1]), toNim(c.children[2]))
            else:
              nodes[i] = ident(c)
          result = node(nnkEnumTy, lisp.info, nodes)
      of "object":
        if cn.len == 1:
          result = node(nnkObjectTy, topInfo)
        else:
          var nodes = newSeq[UntypedNode](3)
          nodes[0] = node(nnkEmpty, topInfo)
          var start = 1
          if cn[start].kind == List and cn[start].children.len == 2 and
            cn[start].children[0].kind == Atom and
            cn[start].children[0].atom == "of":
            nodes[1] = node(nnkOfInherit, cn[start].info,
              toNim(cn[start].children[1]))
            inc start
          else:
            nodes[1] = node(nnkEmpty, topInfo)
          proc parseRecList(x: Lisp): UntypedNode
          proc parseRec(x: Lisp): UntypedNode =
            if x.kind == List and x.children.len != 0 and x.children[0].kind == Atom:
              let a = x.children[0].atom
              if a == "case":
                var nodes = newSeq[UntypedNode](x.children.len - 1)
                for i in 1 ..< x.children.len - 2:
                  let y = x.children[i]
                  if y.kind == List and y.children.len != 0 and y.children[0].kind == Atom:
                    let b = y.children[0].atom
                    if b == "of":
                      var nodes2 = newSeq[UntypedNode](y.children.len - 1)
                      for i in 1 ..< y.children.len - 2:
                        nodes2[i - 1] = toNim(y.children[i])
                      nodes2[y.children.len - 2] = parseRecList(y.children[y.children.len - 1])
                      nodes[i - 1] = node(nnkOfBranch, y.info, nodes2)
                    elif b == "else" and y.children.len == 2:
                      nodes[i - 1] = node(nnkElse, y.info, parseRecList(y.children[2]))
                    elif b == "elif" and y.children.len == 3:
                      nodes[i - 1] = node(nnkElifBranch, y.info, toNim(y.children[1]), parseRecList(y.children[2]))
                    else:
                      nodes[i - 1] = toNim(y)
                  else:
                    nodes[i - 1] = toNim(y)
                nodes[^1] = parseRecList(x.children[^1])
                result = node(nnkRecCase, x.info, map(x.children.toOpenArray(1, x.children.len - 1)))
              elif a == "when":
                var nodes = newSeq[UntypedNode](x.children.len - 1)
                for i in 1 ..< x.children.len - 2:
                  let y = x.children[i]
                  if y.kind == List and y.children.len != 0 and y.children[0].kind == Atom:
                    let b = y.children[0].atom
                    if b == "else" and y.children.len == 2:
                      nodes[i - 1] = node(nnkElse, y.info, parseRecList(y.children[2]))
                    elif b == "elif" and y.children.len == 3:
                      nodes[i - 1] = node(nnkElifBranch, y.info, toNim(y.children[1]), parseRecList(y.children[2]))
                    else:
                      nodes[i - 1] = toNim(y)
                  else:
                    nodes[i - 1] = toNim(y)
                nodes[^1] = parseRecList(x.children[^1])
                result = node(nnkRecCase, x.info, map(x.children.toOpenArray(1, x.children.len - 1)))
              else:
                result = typeBiasedIdentDef(x)
            else:
              result = typeBiasedIdentDef(x)
          proc parseRecList(x: Lisp): UntypedNode =
            if x.kind == List:
              var nodes = newSeq[UntypedNode](x.children.len)
              for i in 0 ..< x.children.len:
                nodes[i] = parseRec(x.children[i])
              result = node(nnkRecList, x.info, nodes)
            else:
              result = node(nnkRecList, x.info, parseRec(x))
          nodes[2] = parseRecList(cn[start])
      of "type":
        block sectioner:
          var nodes = newSeq[UntypedNode](cn.len - 1)
          for i in 1 ..< cn.len:
            let n = cn[i]
            if n.kind == List and n.children.len > 1:
              if n.children.len == 2:
                nodes[i - 1] = node(nnkTypeDef, n.info, ident(n.children[0]), node(nnkEmpty, n.info), toNim(n.children[1]))
              elif n.children.len == 3:
                let name = ident(n.children[0])
                var genericParamNodes: seq[UntypedNode]
                let generic = n.children[1]
                if generic.kind == List:
                  genericParamNodes = newSeq[UntypedNode](generic.children.len)
                  for i in 0 ..< generic.children.len:
                    genericParamNodes[i] = typeBiasedIdentDef(generic.children[i])
                else: break sectioner
                nodes[i - 1] = node(nnkTypeDef, n.info,
                  name,
                  node(nnkGenericParams, generic.info, genericParamNodes),
                  toNim(n.children[2]))
              else:
                break sectioner
            else:
              break sectioner
          result = node(nnkTypeSection, topInfo, nodes)
      of "pragma-block":
        if cn.len > 1:
          var pragmaNodes = newSeq[UntypedNode](cn.len - 2)
          for i in 1 ..< cn.len - 1:
            pragmaNodes[i - 1] = toNim(cn[i])
          result = node(nnkPragmaBlock, topInfo, node(nnkPragma, topInfo, pragmaNodes), toNim(cn[^1]))
      of "pragma-expr":
        if cn.len > 1:
          let expr = toNim(cn[1])
          var pragmaNodes = newSeq[UntypedNode](cn.len - 2)
          for i in 2 ..< cn.len:
            pragmaNodes[i - 2] = toNim(cn[i])
          result = node(nnkPragmaExpr, topInfo, expr, node(nnkPragma, topInfo, pragmaNodes))
      of "of?":
        if cn.len == 3:
          result = node(nnkInfix, topInfo, node(nnkIdent, cn[0].info, "of"), toNim(cn[1]), toNim(cn[2]))
      of "of":
        # needed to be separate from of? for post expr blocks
        result = node(nnkOfBranch, topInfo, map(args))
      of "except":
        result = node(nnkExceptBranch, topInfo, map(args))
      of "elif":
        if cn.len == 3:
          result = node(nnkElifBranch, topInfo, map(args))
      of "else":
        if cn.len == 2:
          result = node(nnkElse, topInfo, map(args))
      of "finally":
        if cn.len == 2:
          result = node(nnkFinally, topInfo, map(args))
      of "while":
        if cn.len == 3:
          result = node(nnkWhileStmt, topInfo, map(args))
      of "if", "when":
        if cn.len > 2:
          let kind = if a == "if": nnkIfStmt else: nnkWhenStmt
          var nodes = newSeq[UntypedNode](cn.len - 2)
          nodes[0] = node(nnkElifBranch, cn[1].info, toNim(cn[1]), toNim(cn[2]))
          for i in 3 ..< cn.len:
            nodes[i - 2] = toNim(cn[i])
          result = node(kind, topInfo, nodes)
      of "try":
        if cn.len > 2:
          result = node(nnkTryStmt, topInfo, map(args))
      of "case":
        if cn.len > 2:
          result = node(nnkCaseStmt, topInfo, map(args))
      of "using":
        var nodes = newSeq[UntypedNode](cn.len - 1)
        for i in 1 ..< cn.len:
          nodes[i - 1] = typeBiasedIdentDef(cn[i])
        result = node(nnkUsingStmt, topInfo, nodes)
      of "for":
        if cn.len > 2:
          var nodes = newSeq[UntypedNode](cn.len - 1)
          for i in 1 ..< cn.len - 2:
            nodes[i - 1] = ident(cn[i], true)
          nodes[cn.len - 2] = toNim(cn[cn.len - 2])
          nodes[cn.len - 1] = toNim(cn[cn.len - 1])
          result = node(nnkForStmt, topInfo, nodes)
      of "proc": result = doProc(nnkProcDef, topInfo, args)
      of "func": result = doProc(nnkFuncDef, topInfo, args)
      of "method": result = doProc(nnkMethodDef, topInfo, args)
      of "converter": result = doProc(nnkConverterDef, topInfo, args)
      of "macro": result = doProc(nnkMacroDef, topInfo, args)
      of "template": result = doProc(nnkTemplateDef, topInfo, args)
      of "iterator": result = doProc(nnkIteratorDef, topInfo, args)
      of "proc-type": result = procType(nnkProcTy, topInfo, args)
      of "iterator-type": result = procType(nnkIteratorTy, topInfo, args)
      of "pragma": result = node(nnkPragma, topInfo, map(args))
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

proc toNim*(lisps: openarray[Lisp]): seq[UntypedNode] =
  result = newSeq[UntypedNode](lisps.len)
  for i in 0 ..< lisps.len: result[i] = toNim(lisps[i])

# nodes left:
  # nnkDo
  # nnkTypeClassTy concept
