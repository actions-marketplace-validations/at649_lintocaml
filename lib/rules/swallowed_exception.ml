open Lintocaml_engine

let docs =
  {|`try ... with _ -> ...` catches every exception, including runtime failures
that ordinary recovery code should not consume: `Out_of_memory`,
`Stack_overflow`, and `Sys.Break` from Ctrl-C. It can also hide library-specific
control-flow exceptions that the handler never intended to intercept.

Match the exceptions you actually expect: `with Not_found -> ...`.

Two shapes are not reported. A catch-all that re-raises - `with e -> log e;
raise e` - inspects the exception without swallowing it. And a catch-all
preceded by an arm that re-raises the fatal exceptions:

    try ...
    with
    | (Out_of_memory | Stack_overflow | Sys.Break) as e -> raise e
    | e -> handle e

is the recommended shape, since the exceptions that must not be caught are
already back out of the way.|}

let check (e : Expr_view.t) =
  match e.desc with
  | Expr_view.Try { handlers; _ } ->
      (* [try ... with Fatal -> raise e | _ -> handle] is the recommended
         shape: the exceptions that must not be caught are re-raised by an
         earlier arm, so the catch-all after it is deliberate. *)
      let rec findings fatal_protected = function
        | [] -> []
        | (handler : Expr_view.handler) :: rest ->
            let finding =
              if
                handler.catches_all
                && (not handler.guarded)
                && (not handler.reraises)
                && not fatal_protected
              then
                [
                  Rule.finding ~loc:handler.h_loc
                    ~suggestion:"match the exceptions you expect, or re-raise the rest"
                    "catch-all handler also swallows Out_of_memory, Stack_overflow and \
                     Sys.Break";
                ]
              else []
            in
            finding @ findings (fatal_protected || handler.protects_fatal) rest
      in
      findings false handlers
  | _ -> []

let rule : Rule.t =
  {
    id = "swallowed-exception";
    title = "Catch-all exception handler";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
