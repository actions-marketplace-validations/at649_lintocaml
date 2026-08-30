open Lintml_engine
open Expr_view

let docs =
  {|`List.length` on the result of `List.append` traverses the appended list a
second time to count elements both inputs already knew about.

`List.length (a @ b)` is `List.length a + List.length b`, which avoids building
the appended list at all when the result is not otherwise needed. The append
itself is O(length a), so the combined form is a real saving rather than a
rearrangement.|}

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some outer; args = [ inner ] }
    when path_is outer [ "Stdlib.List.length" ]
         && callee_is inner [ "Stdlib.List.append"; "Stdlib.@" ] ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"add the two lengths instead of appending first"
          "this appends two lists only to count the result";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "length-of-append";
    title = "Length taken of an appended list";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
