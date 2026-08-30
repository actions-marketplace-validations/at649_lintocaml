open Lintocaml_engine
open Expr_view

let docs =
  {|A function that only forwards each of its unlabelled parameters to another
function adds a name and call layer without adding behavior. For example,
`fun x -> parse x` can be replaced by `parse`.

The rule requires the forwarded arguments to be the same resolved identifiers,
in the same order. Labelled and optional arguments are excluded because
removing their wrapper can change the public function type, and recursive
bindings are excluded because `let rec f = g` does not compile.

One caveat the rule cannot check for you: eta-expansion is sometimes load
bearing. `let f = g` is subject to the value restriction where `let f x = g x`
is not, so if the right-hand side is a partial application the reduced form can
end up with a weak type variable that will not generalise. If the code stops
compiling after the rewrite, that is why, and the original was correct.|}

let arguments_are_parameters parameters arguments =
  parameters <> []
  && List.length parameters = List.length arguments
  && List.for_all2 ident_is parameters arguments

let check (expression : Expr_view.t) =
  match expression.desc with
  | Function
      {
        params;
        simple_params = true;
        (* Eta-reducing a recursive binding yields [let rec f = g], which the
           compiler rejects. *)
        recursive = false;
        body = { desc = Apply { callee = Some _; args }; _ };
      }
    when arguments_are_parameters params args ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"replace the wrapper with the function it calls"
          "this function only forwards its arguments unchanged";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "redundant-fun-wrapper";
    title = "Function that only forwards its arguments";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
