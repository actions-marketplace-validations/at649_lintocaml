open Lintocaml_engine
open Expr_view

let docs =
  {|Comparing a boolean with `true` or `false` repeats information already
carried by the value. `condition = true` is just `condition`, while
`condition = false` is `not condition`.

Removing the comparison makes conditions shorter without changing evaluation
order or exception behavior. Comparisons between two literals are left alone
for the compiler's constant-expression tooling.|}

type literal = True | False

let bool_literal = function
  | expression when is_construct "true" expression -> Some True
  | expression when is_construct "false" expression -> Some False
  | _ -> None

let replacement operator literal =
  match (strip_stdlib operator, literal) with
  | "=", True | "<>", False -> "use the boolean expression directly"
  | "=", False | "<>", True -> "negate the boolean expression with `not`"
  | _ -> "simplify the boolean expression"

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some operator; args = [ left; right ] }
    when path_is operator [ "Stdlib.="; "Stdlib.<>" ] -> (
      match (bool_literal left, bool_literal right) with
      | Some _, Some _ | None, None -> []
      | Some literal, None | None, Some literal ->
          [
            Rule.finding ~loc:expression.loc
              ~suggestion:(replacement operator literal)
              "a comparison with a boolean literal is redundant";
          ])
  | _ -> []

let rule : Rule.t =
  {
    id = "boolean-comparison";
    title = "Boolean compared with true or false";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
