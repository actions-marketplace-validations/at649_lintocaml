open Lintocaml_engine
open Expr_view

let docs =
  {|`List.filter predicate list = []` builds the entire filtered list, then
compares it with the empty list to conclude that nothing matched. The filtered
allocation is thrown away immediately.

`not (List.exists predicate list)` answers the same question and stops at the
first match; for `<> []` drop the negation. The rule matches resolved
standard-library filter calls only, and requires the empty list to be the other
operand so ordinary filter uses are not reported.|}

let filter_functions = [ "Stdlib.List.filter"; "Stdlib.List.find_all" ]

let is_filter expression =
  match expression.desc with
  | Apply { callee = Some callee; args = _ :: _ :: _ } -> path_is callee filter_functions
  | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some operator; args = [ left; right ] }
    when path_is operator [ "Stdlib.="; "Stdlib.<>" ]
         && ((is_filter left && is_construct "[]" right)
            || (is_construct "[]" left && is_filter right)) ->
      let suggestion =
        if path_is operator [ "Stdlib.=" ] then
          "not (List.exists predicate list) tests absence without allocating"
        else "List.exists predicate list tests presence without allocating"
      in
      [
        Rule.finding ~loc:expression.loc ~suggestion
          "comparing a filtered list against [] builds the entire filtered list first";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "empty-filter-equality";
    title = "Filter result compared against the empty list";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
