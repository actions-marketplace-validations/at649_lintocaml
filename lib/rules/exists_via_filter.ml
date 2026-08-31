open Lintocaml_engine
open Expr_view

let docs =
  {|`List.length (List.filter p l) > 0` builds a filtered list, counts it, and
throws both away to answer a yes-or-no question. `List.exists p l` answers the
same question and stops at the first match.

The difference is not only allocation: on a list whose first element satisfies
the predicate, `exists` inspects one element where the filter inspects all of
them. This rewrite short-circuits, so keep the original traversal when later
predicate effects are intentional.|}

let is_filter expression =
  callee_is expression [ "Stdlib.List.filter"; "Stdlib.List.find_all" ]

let is_length_of_filter expression =
  match expression.desc with
  | Apply { callee = Some callee; args = [ inner ] }
    when path_is callee [ "Stdlib.List.length" ] ->
      is_filter inner
  | _ -> false

let suggestion operator ~length_on_left =
  match (strip_stdlib operator, length_on_left) with
  | "=", _ -> Some "use not (List.exists predicate list)"
  | "<>", _ -> Some "use List.exists predicate list"
  | ">", true | "<", false -> Some "use List.exists predicate list"
  | _ -> None

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some operator; args = [ left; right ] }
    when path_is operator [ "Stdlib.>"; "Stdlib.<"; "Stdlib.="; "Stdlib.<>" ] -> (
      let length_on_left =
        if is_length_of_filter left && is_int_const 0 right then Some true
        else if is_int_const 0 left && is_length_of_filter right then Some false
        else None
      in
      match
        Option.bind length_on_left (fun length_on_left ->
            suggestion operator ~length_on_left)
      with
      | None -> []
      | Some suggestion ->
          [
            Rule.finding ~loc:expression.loc ~suggestion
              "this filters and counts an entire list to answer a yes-or-no question";
          ])
  | _ -> []

let rule : Rule.t =
  {
    id = "exists-via-filter";
    title = "Filter and count used as an existence test";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
