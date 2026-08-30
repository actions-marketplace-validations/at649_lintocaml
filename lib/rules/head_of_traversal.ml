open Lintocaml_engine
open Expr_view

let docs =
  {|Taking the head of a sorted or reversed list builds the entire intermediate
list to read one element from it.

`List.hd (List.sort c l)` is the minimum under `c`, obtainable in one pass with
a fold and no allocation. `List.hd (List.rev l)` is the last element, likewise
one pass. Sorting to find a minimum is the more expensive of the two: it turns
O(n) into O(n log n) and allocates the sorted list as well.|}

let heads = [ "Stdlib.List.hd" ]
let sorts = [ "Stdlib.List.sort"; "Stdlib.List.stable_sort"; "Stdlib.List.fast_sort" ]
let reverses = [ "Stdlib.List.rev" ]

let describe inner =
  if path_is inner sorts then
    Some
      ( "sorting a list to read its first element does O(n log n) work for an O(n) \
         question",
        "fold over the list to find the minimum" )
  else if path_is inner reverses then
    Some
      ( "reversing a list to read its last element allocates the whole reversed list",
        "fold over the list to find the last element" )
  else None

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply
      { callee = Some outer; args = [ { desc = Apply { callee = Some inner; _ }; _ } ] }
    when path_is outer heads -> (
      match describe inner with
      | Some (message, suggestion) ->
          [ Rule.finding ~loc:expression.loc ~suggestion message ]
      | None -> [])
  | _ -> []

let rule : Rule.t =
  {
    id = "head-of-traversal";
    title = "Head taken of a sorted or reversed list";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
