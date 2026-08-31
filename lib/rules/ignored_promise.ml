open Lintocaml_engine
open Expr_view

let docs =
  {|Discarding an `Lwt.t` or `Eio.Promise.t` value drops the caller's handle to
an asynchronous computation. Depending on the library and call, failures may go
unobserved and cancellation or structured-concurrency guarantees may be lost.

Await the computation or attach it to an explicit supervisor. This rule checks
only known promise type names and explicit `ignore` or `let _ =` sites.|}

let is_promise value =
  type_name_ends_with "Lwt.t" value || type_name_ends_with "Eio.Promise.t" value

let explicitly_discarded = function
  | { desc = Discard value; _ } -> Some value
  | { desc = Apply { callee = Some callee; args = [ value ] }; _ }
    when path_is callee [ "Stdlib.ignore" ] ->
      Some value
  | _ -> None

let check e =
  match explicitly_discarded e with
  | Some value when is_promise value ->
      [
        Rule.finding ~loc:e.loc
          ~suggestion:"await the promise or attach it to an explicit supervisor"
          "a promise is explicitly discarded, so its failure or cancellation can be lost";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "ignored-promise";
    title = "Explicitly discarded promise";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
