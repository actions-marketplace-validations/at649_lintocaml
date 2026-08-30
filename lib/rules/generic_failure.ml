open Lintml_engine
open Expr_view

let docs =
  {|`failwith` raises the generic `Failure` exception. Callers cannot handle
that failure precisely without inspecting a message string, and the exception
type communicates nothing about the failed invariant.

For library and application boundaries, define a descriptive exception or
return `result`. This rule is opt-in because small scripts and impossible
branches may reasonably use `failwith`.|}

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; _ } when path_is callee [ "Stdlib.failwith" ] ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"raise a descriptive exception or return a result"
          "failwith raises an undifferentiated Failure exception";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "generic-failure";
    title = "Generic failwith exception";
    category = Rule.Idiom;
    profile = Rule.Pedantic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
