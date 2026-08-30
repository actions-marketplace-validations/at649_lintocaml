open Lintocaml_engine
open Expr_view

let docs =
  {|Integer division and remainder operations raise `Division_by_zero` when
their divisor is zero. A literal zero divisor is always a bug; it cannot be
made safe by input or control flow elsewhere.

Use the intended non-zero divisor, or validate a dynamic divisor before the
operation. Float division is deliberately excluded because IEEE float division
by zero produces infinities or NaN instead of raising this exception.|}

let operators =
  [
    "Stdlib./";
    "Stdlib.mod";
    "Stdlib.Int.div";
    "Stdlib.Int.rem";
    "Stdlib.Int32.div";
    "Stdlib.Int32.rem";
    "Stdlib.Int64.div";
    "Stdlib.Int64.rem";
    "Stdlib.Nativeint.div";
    "Stdlib.Nativeint.rem";
  ]

let is_zero = function
  | { desc = Const (Int 0); _ } -> true
  | { desc = Const (Int32 0l); _ } -> true
  | { desc = Const (Int64 0L); _ } -> true
  | { desc = Const (Nativeint 0n); _ } -> true
  | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = [ _dividend; divisor ] }
    when path_is callee operators && is_zero divisor ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"use a non-zero divisor or validate it before this operation"
          "integer division or remainder by zero always raises Division_by_zero";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "division-by-zero";
    title = "Literal integer division by zero";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
