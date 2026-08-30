open Expr_view

(* An expression can be substituted verbatim only when it binds tighter than
   whatever it lands next to. Everything else needs parentheses: splicing
   [a + b] into [f _ 1] without them silently reassociates the result.

   Negative literals are the subtle case. [-1] spliced after an operator reads
   as a subtraction, so every numeric constant below is atomic only when it is
   non-negative. *)
let atomic (expression : Expr_view.t) =
  match expression.desc with
  | Ident _ -> true
  | Const (String _ | Char _) -> true
  | Const (Int n) -> n >= 0
  | Const (Int32 n) -> Int32.compare n 0l >= 0
  | Const (Int64 n) -> Int64.compare n 0L >= 0
  | Const (Nativeint n) -> Nativeint.compare n 0n >= 0
  | Const (Float literal) -> not (String.starts_with ~prefix:"-" literal)
  | Construct { args = []; _ } -> true
  | _ -> false

let source expression =
  if atomic expression then Rule.Source expression.loc
  else Rule.Parenthesized_source expression.loc
