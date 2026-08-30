open Lintml_engine
open Expr_view

let docs =
  {|List append traverses and copies its left operand. In (first @ second) @
third, the spine of first is copied twice: once for the inner append and again
as part of the outer append. Longer left-nested chains amplify that repeated
work.

Reassociate the expression to the right or collect the lists and concatenate
once. A right-nested append is not reported because each spine is then copied
only once.|}

let append_functions = [ "Stdlib.@"; "Stdlib.List.append" ]

let is_append = function
  | { desc = Apply { callee = Some callee; args = [ _; _ ] }; _ } ->
      path_is callee append_functions
  | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = [ left; _right ] }
    when path_is callee append_functions && is_append left ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"reassociate appends to the right or use List.concat"
          "left-nested list append recopies an intermediate list";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "left-nested-list-append";
    title = "Left-nested list append";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
