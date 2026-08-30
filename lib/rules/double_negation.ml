open Lintocaml_engine
open Expr_view

let docs =
  {|`not (not condition)` has exactly the same value and effects as
`condition`. The double negation adds visual noise and can conceal a mistaken
extra negation during review.

Use the inner boolean expression directly.|}

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply
      {
        callee = Some outer;
        args = [ { desc = Apply { callee = Some inner; args = [ value ] }; _ } ];
      }
    when path_is outer [ "Stdlib.not" ] && path_is inner [ "Stdlib.not" ] ->
      [
        Rule.finding ~loc:expression.loc ~fix:(Fix.source value)
          ~suggestion:"remove both `not` applications"
          "double boolean negation is redundant";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "double-negation";
    title = "Double boolean negation";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
