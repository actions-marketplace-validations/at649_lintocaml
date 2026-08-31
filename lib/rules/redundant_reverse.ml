open Lintocaml_engine
open Expr_view

let docs =
  {|Reversing a list twice reconstructs the original order while traversing
and allocating the list two times. This is occasionally left behind after a
pipeline is refactored.

Use the original list expression directly. The rewrite preserves structural
content and all evaluation of the list expression, but not the fresh list
allocation. Keep the reversals when a test is deliberately checking that
allocation behavior.|}

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply
      {
        callee = Some outer;
        args = [ { desc = Apply { callee = Some inner; args = [ _ ] }; _ } ];
      }
    when path_is outer [ "Stdlib.List.rev" ] && path_is inner [ "Stdlib.List.rev" ] ->
      [
        Rule.finding ~loc:expression.loc ~suggestion:"remove both List.rev calls"
          "reversing a list twice performs two unnecessary traversals";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "redundant-reverse";
    title = "List reversed twice";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
