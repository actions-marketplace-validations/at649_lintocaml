open Lintocaml_engine
open Expr_view

let docs =
  {|`String.length s = 0` asks whether a string is empty by measuring it. The
measurement itself is constant time, so this is a readability issue rather
than a performance one: the reader has to reconstruct the intent from the
number.

Compare with the empty string instead: `s = ""` for emptiness and `s <> ""`
for non-emptiness. The rule reports only comparisons equivalent to one of
those tests; impossible comparisons such as `String.length s < 0` are left
alone rather than given a misleading rewrite.|}

let length_of_string expression =
  match expression.desc with
  | Apply { callee = Some callee; args = [ _ ] } ->
      path_is callee [ "Stdlib.String.length" ]
  | _ -> false

let replacement operator ~length_on_left =
  match (strip_stdlib operator, length_on_left) with
  | "=", _ -> Some "s = \"\""
  | "<>", _ -> Some "s <> \"\""
  | ">", true | "<", false -> Some "s <> \"\""
  | _ -> None

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some operator; args = [ left; right ] }
    when path_is operator [ "Stdlib.="; "Stdlib.<>"; "Stdlib.<"; "Stdlib.>" ] -> (
      let length_on_left =
        if length_of_string left && is_int_const 0 right then Some true
        else if is_int_const 0 left && length_of_string right then Some false
        else None
      in
      match
        Option.bind length_on_left (fun length_on_left ->
            replacement operator ~length_on_left)
      with
      | None -> []
      | Some replacement ->
          [
            Rule.finding ~loc:expression.loc
              ~suggestion:
                (Fmt.str "compare against the empty string instead: %s" replacement)
              "comparing String.length with 0 is an indirect emptiness test";
          ])
  | _ -> []

let rule : Rule.t =
  {
    id = "string-length-compare-empty";
    title = "String.length compared with a literal 0";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
