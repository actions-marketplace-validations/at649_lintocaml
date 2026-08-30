open Lintocaml_engine
open Expr_view

let docs =
  {|Appending an empty list on the left returns the right-hand list unchanged.
The empty operand communicates no behavior and makes the expression harder to
scan.

Use the right-hand expression directly. The rule intentionally does not report
an empty list on the right: removing that append can change allocation identity,
even though code should rarely depend on it.|}

let append_functions = [ "Stdlib.@"; "Stdlib.List.append" ]

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = [ left; right ] }
    when path_is callee append_functions && is_construct "[]" left ->
      [
        Rule.finding ~loc:expression.loc ~fix:(Fix.source right)
          ~suggestion:"use the right-hand list directly"
          "appending an empty list on the left is redundant";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "redundant-list-append";
    title = "Empty list appended on the left";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
