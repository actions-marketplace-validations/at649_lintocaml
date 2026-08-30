open Lintml_engine
open Expr_view

let docs =
  {|`Printf.sprintf "%s" x` parses a format and allocates a result only to
return the same string contents. A direct conversion states the intent without
the formatting machinery.

Only conversions whose output is identical to the direct function are reported:

- `%s`: the argument itself
- `%d` or `%i`: `string_of_int`
- `%b`: `string_of_bool`

`%f` is deliberately absent. `sprintf "%f" 1.0` is "1.000000" while
`string_of_float 1.0` is "1.", so suggesting the swap would change the output.|}

let sprintf_paths = [ "Stdlib.Printf.sprintf"; "Stdlib.Format.sprintf" ]

(* Only conversions that are exactly equivalent to a direct call. Adding one
   here means claiming the two produce the same string for every input. *)
let replacement_for = function
  | "%s" -> Some "the argument itself"
  | "%d" | "%i" -> Some "string_of_int"
  | "%b" -> Some "string_of_bool"
  | _ -> None

(* The typechecker rewrites a format literal into
   [CamlinternalFormatBasics.Format (parsed, "%s")], so the plain string is not
   the argument itself. The constructor's second field is the original literal
   the compiler recorded, which is what we read. *)
let format_literal expression =
  match expression.desc with
  | Const (String literal) -> Some literal
  | Construct { name = "Format"; args = [ _; { desc = Const (String literal); _ } ] } ->
      Some literal
  | _ -> None

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = [ format; _argument ] }
    when path_is callee sprintf_paths -> (
      match Option.bind (format_literal format) replacement_for with
      | None -> []
      | Some direct ->
          [
            Rule.finding ~loc:expression.loc ~suggestion:(Fmt.str "use %s" direct)
              "this format string does no formatting";
          ])
  | _ -> []

let rule : Rule.t =
  {
    id = "trivial-sprintf";
    title = "Trivial sprintf conversion";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
