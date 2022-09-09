let rec replace = fun (e, x, replaceWith) -> match e with
  | Identifier(x)
  | Number(n) -> e
  | Parenthesized(e) -> Parenthesized(replace(e, varName, replaceWith))
  | BinaryOp(l, op, r) -> BinaryOp(
    replace(l, varName, replaceWith),
    op,
    replace(r, varName, replaceWith)
  )
in replace (
  BinaryOp(
    Identifier("x"),
    Plus,
    Parenthesized(
      BinaryOp(
        Identifier("x"), Minus, Number(2)
      )
    )
  )
)