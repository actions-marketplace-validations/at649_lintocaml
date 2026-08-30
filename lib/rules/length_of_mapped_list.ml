open Lintml_engine
open Expr_view

let docs =
  {|`List.map` allocates a new list of exactly the same length as its input, so
`List.length (List.map f l)` builds a whole list in order to count something
already known: it is `List.length l`.

Beyond the wasted allocation, the mapped function is applied to every element
purely for its return value, which is then discarded. If `f` has side effects,
use `List.iter f l` before taking `List.length l`; this preserves the effects
without allocating the mapped list.|}

let length_preserving = [ "Stdlib.List.map"; "Stdlib.List.mapi"; "Stdlib.List.rev" ]

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply
      { callee = Some outer; args = [ { desc = Apply { callee = Some inner; _ }; _ } ] }
    when path_is outer [ "Stdlib.List.length" ] && path_is inner length_preserving ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:
            "take the original list's length; use List.iter first if the callback has \
             effects"
          "this traversal allocates a list only to count elements the input already had";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "length-of-mapped-list";
    title = "Length taken of a length-preserving traversal";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
