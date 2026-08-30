open Lintml_engine
open Expr_view

let docs =
  {|`List.length list op n` traverses the entire list even though a comparison
against a fixed bound can stop as soon as that bound is crossed. On unbounded
input this needlessly turns a bounded operation into a full traversal.

Use `List.compare_length_with list n`, then compare its result with zero. The
zero case has a clearer `list = []` rewrite and is handled by a separate rule.|}

let operators =
  [ "Stdlib.="; "Stdlib.<>"; "Stdlib.<"; "Stdlib.>"; "Stdlib.<="; "Stdlib.>=" ]

let is_length expression = callee_is expression [ "Stdlib.List.length" ]
let is_positive_int = function { desc = Const (Int value); _ } -> value > 0 | _ -> false

let check (e : Expr_view.t) =
  match e.desc with
  | Apply { callee = Some operator; args = [ left; right ] }
    when path_is operator operators
         && ((is_length left && is_positive_int right)
            || (is_positive_int left && is_length right)) ->
      [
        Rule.finding ~loc:e.loc
          ~suggestion:"compare List.compare_length_with list n against zero"
          "List.length traverses the whole list to compare it with a fixed bound";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "length-compare-n";
    title = "List.length compared against a fixed bound";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
