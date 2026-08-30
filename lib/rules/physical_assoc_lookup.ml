open Lintocaml_engine
open Expr_view

let docs =
  {|`List.assq`, `List.memq` and `List.assq_opt` compare keys with physical
equality. For a boxed key that means the lookup succeeds only when the caller
holds the very same allocation, so a structurally equal key built elsewhere -
parsed from input, read from a table, rebuilt after a round trip - silently
misses.

Use `List.assoc`, `List.mem` and `List.assoc_opt`, which compare structurally.
The physical variants are appropriate for interned or unique values, where the
identity is the point.|}

let physical_lookups =
  [
    "Stdlib.List.assq";
    "Stdlib.List.assq_opt";
    "Stdlib.List.memq";
    "Stdlib.List.remove_assq";
  ]

let structural_name callee =
  match strip_stdlib callee with
  | "List.assq" -> "List.assoc"
  | "List.assq_opt" -> "List.assoc_opt"
  | "List.memq" -> "List.mem"
  | "List.remove_assq" -> "List.remove_assoc"
  | other -> other

(* All four take the key first and the list second. Matching the pair exactly
   rather than [key :: _] matters because labelled arguments never reach a rule:
   on [List.assq ~key:k l] the list would slide into the key position, and a
   list is boxed, so the rule would report the list as the key. *)
let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = [ key; _ ] }
    when path_is callee physical_lookups && key.ty = Boxed ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:
            (Fmt.str "use %s, which compares structurally" (structural_name callee))
          (Fmt.str
             "%s compares keys by allocation, so an equal key built elsewhere will not \
              be found"
             (strip_stdlib callee));
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "physical-assoc-lookup";
    title = "Physical-equality lookup with a boxed key";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
