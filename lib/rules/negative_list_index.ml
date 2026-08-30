open Lintml_engine
open Expr_view

let docs =
  {|`List.nth` raises `Invalid_argument` on a negative index, and
`List.nth_opt` raises rather than returning `None`. A negative literal index is
therefore never reachable code: it always raises.

This usually appears when an index is computed by subtraction and the guard is
missing, or when someone reaches for the Python convention where -1 means the
last element. If end-relative access is common, use an array or maintain the
value while traversing the list.|}

let indexers = [ "Stdlib.List.nth"; "Stdlib.List.nth_opt" ]

let is_negative expression =
  match expression.desc with Const (Int n) -> n < 0 | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = [ _; index ] }
    when path_is callee indexers && is_negative index ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"use a non-negative index; List.nth has no end-relative form"
          "a negative list index always raises Invalid_argument";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "negative-list-index";
    title = "Negative literal index passed to List.nth";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
