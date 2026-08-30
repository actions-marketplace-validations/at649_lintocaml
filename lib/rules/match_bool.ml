open Lintml_engine
open Expr_view

let docs =
  {|A two-arm match on `true` and `false` is an indirect spelling of an
`if` expression. Using `if condition then ... else ...` makes the boolean
control flow immediately recognizable.

The rule requires exactly the two unguarded boolean constructor patterns.
Matches with guards, aliases, or additional patterns are left unchanged.|}

let check (expression : Expr_view.t) =
  match expression.desc with
  | Bool_match _ ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"replace the boolean match with an if expression"
          "matching true and false is clearer as an if expression";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "match-bool";
    title = "Boolean match instead of if";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
