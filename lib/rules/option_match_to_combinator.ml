open Lintocaml_engine
open Expr_view

let docs =
  {|A match that maps Some through a named function and returns None unchanged
duplicates the definition of Option.map. Likewise, forwarding the unwrapped
value to an option-returning function duplicates Option.bind.

Using the combinator names the operation directly and removes boilerplate. The
rule requires an unguarded standard option match, a simple bound variable, and
a resolved function identifier. It deliberately avoids Option.value rewrites,
whose eager default can change effects and exception behavior.|}

let forwarded_call binding = function
  | { desc = Apply { callee = Some _; args = [ argument ] }; _ } as call
    when ident_is binding argument ->
      Some call
  | _ -> None

let check (expression : Expr_view.t) =
  match expression.desc with
  | Option_match
      {
        binding;
        when_some = { desc = Construct { name = "Some"; args = [ mapped ] }; _ };
        when_none;
        _;
      }
    when is_construct "None" when_none && Option.is_some (forwarded_call binding mapped)
    ->
      [
        Rule.finding ~loc:expression.loc ~suggestion:"replace the match with Option.map"
          "this match manually implements Option.map";
      ]
  | Option_match { binding; when_some; when_none; _ }
    when is_construct "None" when_none
         && type_name_is [ "Stdlib.option" ] when_some
         && Option.is_some (forwarded_call binding when_some) ->
      [
        Rule.finding ~loc:expression.loc ~suggestion:"replace the match with Option.bind"
          "this match manually implements Option.bind";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "option-match-to-combinator";
    title = "Manual option map or bind";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
