open Lintocaml_engine
open Expr_view

let docs =
  {|`if Hashtbl.mem table key then ... Hashtbl.find table key ...` hashes the
key, looks it up, throws the answer away, then hashes and looks the same key
up again. The second lookup repeats work that the first already paid for.

`Hashtbl.find_opt table key` answers membership and value with a single
lookup. The rule requires `Hashtbl.find` to be the direct then-branch and the
same resolved table and key identifiers in both calls. It does not look
through intervening code, which could mutate the table or defer the lookup.|}

let mem_fns = [ "Stdlib.Hashtbl.mem" ]
let find_fns = [ "Stdlib.Hashtbl.find" ]

let lookup names expression =
  match expression.desc with
  | Apply { callee = Some callee; args = table :: key :: _ } when path_is callee names ->
      Some (table, key)
  | _ -> None

let same_lookup (left_table, left_key) (right_table, right_key) =
  match (left_table.desc, right_table.desc, left_key.desc, right_key.desc) with
  | Ident left_table, Ident right_table, Ident left_key, Ident right_key ->
      String.equal left_table right_table && String.equal left_key right_key
  | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | If { cond; then_; _ } -> (
      match (lookup mem_fns cond, lookup find_fns then_) with
      | Some mem_args, Some find_args when same_lookup mem_args find_args ->
          [
            Rule.finding ~loc:expression.loc
              ~suggestion:"Hashtbl.find_opt answers membership and value with one lookup"
              "Hashtbl.mem followed by Hashtbl.find on the same key performs two lookups";
          ]
      | _ -> [])
  | _ -> []

let rule : Rule.t =
  {
    id = "hashtbl-mem-then-find";
    title = "Hashtbl.mem followed by Hashtbl.find on the same key";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
