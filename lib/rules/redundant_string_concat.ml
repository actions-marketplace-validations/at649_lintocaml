open Lintocaml_engine
open Expr_view

let docs =
  {|Concatenating the empty string contributes no content, but `(^)` may still
allocate and copy the other operand. The result is clearer and no slower when
the empty operand is removed.

This rule reports only a literal empty string on either side. It does not guess
whether a computed string happens to be empty.|}

let is_empty_string = function { desc = Const (String ""); _ } -> true | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = [ left; right ] }
    when path_is callee [ "Stdlib.^" ] && (is_empty_string left || is_empty_string right)
    ->
      [
        Rule.finding ~loc:expression.loc ~suggestion:"remove the empty-string operand"
          "concatenating an empty string is redundant";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "redundant-string-concat";
    title = "Concatenation with an empty string";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
