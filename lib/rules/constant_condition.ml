open Lintocaml_engine
open Expr_view

let docs =
  {|An if expression whose condition is literally true or false has an
unreachable branch and obscures which code can execute. This is often residue
from debugging, generated configuration, or an incomplete edit.

Keep only the reachable expression. This rule does not attempt constant
folding, so named flags and computed conditions are left untouched.|}

let check (expression : Expr_view.t) =
  match expression.desc with
  | If { cond; then_; _ } when is_construct "true" cond ->
      [
        Rule.finding ~loc:expression.loc ~fix:(Fix.source then_)
          ~suggestion:"remove the unreachable branch"
          "this conditional has a constant boolean condition";
      ]
  | If { cond; else_ = Some else_; _ } when is_construct "false" cond ->
      [
        Rule.finding ~loc:expression.loc ~fix:(Fix.source else_)
          ~suggestion:"remove the unreachable branch"
          "this conditional has a constant boolean condition";
      ]
  | If { cond; else_ = None; _ } when is_construct "false" cond ->
      [
        Rule.finding ~loc:expression.loc ~fix:(Rule.Text "()")
          ~suggestion:"remove the unreachable branch"
          "this conditional has a constant boolean condition";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "constant-condition";
    title = "Conditional with a constant condition";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
