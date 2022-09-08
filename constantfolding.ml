type bop = 
  | Plus
  | Minus
  | Multiply;;

type expr = 
  | Identifier of string
  | Number of int
  | BinaryOp of expr * bop * expr
  | Parenthesized of expr;;

type rec constfold = fun e -> match e with
  | Identifier(x) -> e
  | Number(n) -> e
  | Parenthesized(e) -> Parenthesized(constfold(e))
  | BinaryOp(l, op, r) -> match (constfold l, constfold r) with
    | (Number(a), Number(b)) -> (match op with
      | Plus -> Number (a + b)
      | Multiply -> Number(a * b)
      | Minus -> Number(a - b)
    )
    | (a, b) -> BinaryOp(a, op, b)
in constfold (
  BinaryOp(Identifier("x"), Plus, Parenthesized(BinaryOp(Number 2, Minus, Number 1)))
);;