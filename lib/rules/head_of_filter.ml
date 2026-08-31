open Lintocaml_engine
open Expr_view

let docs =
  {|`List.hd (List.filter p l)` filters the entire list, allocating every match,
and then reads the first one. `List.find p l` stops at the first match and
allocates nothing.

On a list where an early element matches, the difference is the whole traversal.
`List.find` raises `Not_found` where `List.hd` raises `Failure`, so if the empty
case matters use `List.find_opt`, which is what most callers want anyway. Both
alternatives stop early; retain the filter when effects on later elements are
part of the behavior.|}

let heads = [ "Stdlib.List.hd" ]
let filters = [ "Stdlib.List.filter"; "Stdlib.List.find_all" ]

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some outer; args = [ inner ] }
    when path_is outer heads && callee_is inner filters ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:
            "use List.find to preserve the return type, or List.find_opt to handle \
             absence"
          "this filters the whole list to read only its first match";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "head-of-filter";
    title = "First element of a filtered list";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
