open Lintocaml_engine
open Expr_view

let docs =
  {|`List.map (fun x -> x) l` allocates a structurally equal copy of the list and
returns it. The identity function is usually what remains after the real
transformation moved elsewhere.

If a copy is genuinely wanted, say so directly; if not, use the list. Note the
result is a distinct allocation, so code relying on physical equality with the
original would change behaviour.|}

let mappers = [ "Stdlib.List.map"; "Stdlib.Array.map"; "Stdlib.Option.map" ]

let is_identity expression =
  match expression.desc with
  | Function { params = [ parameter ]; simple_params = true; body; _ } ->
      ident_is parameter body
  | Ident name -> path_is name [ "Fun.id" ]
  | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = transform :: _ }
    when path_is callee mappers && is_identity transform ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:
            "use the original value, or copy it explicitly if that is the intent"
          "mapping the identity function allocates a copy and changes nothing";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "identity-map";
    title = "Identity function passed to map";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
