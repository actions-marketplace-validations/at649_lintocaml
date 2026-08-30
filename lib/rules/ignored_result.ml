open Lintocaml_engine
open Expr_view

let docs =
  {|A `result` value contains either the successful value or an error that must
be handled. Explicitly discarding it with `ignore` or `let _ =` loses failures
silently and makes the surrounding code appear to have completed successfully.

Pattern-match the result, propagate it with a result-aware combinator, or state
why failure is impossible. Ordinary sequence expressions are left to compiler
warning 10, so this rule does not duplicate an existing warning.|}

let explicitly_discarded = function
  | { desc = Discard value; _ } -> Some value
  | { desc = Apply { callee = Some callee; args = [ value ] }; _ }
    when path_is callee [ "Stdlib.ignore" ] ->
      Some value
  | _ -> None

let check e =
  match explicitly_discarded e with
  | Some value
    when type_name_is [ "Stdlib.result" ] value && not (is_construct "Ok" value) ->
      [
        Rule.finding ~loc:e.loc
          ~suggestion:"handle or propagate both the Ok and Error cases"
          "a result value is explicitly discarded, so an error can be lost";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "ignored-result";
    title = "Explicitly discarded result";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
