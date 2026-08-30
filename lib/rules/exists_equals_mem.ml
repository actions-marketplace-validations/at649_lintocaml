open Lintocaml_engine
open Expr_view

let docs =
  {|`List.exists (fun item -> item = target) list` is `List.mem target list`
written out by hand. The named function says what is being asked and is one
less closure to read.

The rule requires a simple bound parameter compared for structural equality
with a stable value: an identifier, literal, or nullary constructor. Predicates
that transform the element or recompute the target for every item are left
alone because replacing them could change effects.|}

let stable_target expression =
  match expression.desc with
  | Ident _ | Const _ | Construct { args = []; _ } -> true
  | _ -> false

let member_target parameter body =
  let valid_target expression =
    stable_target expression && not (exists (ident_is parameter) expression)
  in
  match body.desc with
  | Apply { callee = Some operator; args = [ left; right ] }
    when path_is operator [ "Stdlib.=" ] ->
      if ident_is parameter left && valid_target right then Some right
      else if ident_is parameter right && valid_target left then Some left
      else None
  | _ -> None

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = callback :: _ }
    when path_is callee [ "Stdlib.List.exists" ] -> (
      match callback.desc with
      | Function { params = [ parameter ]; simple_params = true; body; _ } -> (
          match member_target parameter body with
          | Some _ ->
              [
                Rule.finding ~loc:expression.loc ~suggestion:"use List.mem target list"
                  "List.exists with an equality on the bound element reimplements \
                   List.mem";
              ]
          | None -> [])
      | _ -> [])
  | _ -> []

let rule : Rule.t =
  {
    id = "exists-equals-mem";
    title = "List.exists used as a hand-written List.mem";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
