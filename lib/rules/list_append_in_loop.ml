open Lintocaml_engine
open Expr_view

let docs =
  {|`acc @ [item]` copies the complete accumulator on every fold iteration.
Building an n-element list this way performs quadratic work and allocates all
intermediate prefixes.

Prepend with `item :: acc` and call `List.rev` once after the fold. This rule
reports the classic inline `List.fold_left` shape only, where both the growing
accumulator and the singleton right-hand list are unambiguous.|}

let is_singleton_list = function
  | {
      desc =
        Construct
          { name = "::"; args = [ _; { desc = Construct { name = "[]"; _ }; _ } ] };
      _;
    } ->
      true
  | _ -> false

let appends_singleton accumulator expression =
  match expression.desc with
  | Apply { callee = Some callee; args = [ left; right ] } ->
      path_is callee [ "Stdlib.@" ]
      && ident_is accumulator left
      && is_singleton_list right
  | _ -> false

let check (e : Expr_view.t) =
  match e.desc with
  | Apply
      {
        callee = Some callee;
        args =
          {
            desc = Function { params = accumulator :: _; simple_params = true; body; _ };
            _;
          }
          :: _;
      }
    when path_is callee [ "Stdlib.List.fold_left" ]
         && exists (appends_singleton accumulator) body ->
      [
        Rule.finding ~loc:e.loc
          ~suggestion:"prepend with item :: acc, then reverse the result once"
          "appending a singleton to a growing list accumulator is quadratic";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "list-append-in-loop";
    title = "List append to a growing fold accumulator";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
