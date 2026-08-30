type t = {
  rule_id : string;
  severity : Severity.t;
  loc : Loc.t;
  message : string;
  suggestion : string option;
  replacement : string option;
  expected_source : string option;
}

let make ~rule_id ~severity ~loc ~message ?suggestion ?replacement ?expected_source () =
  { rule_id; severity; loc; message; suggestion; replacement; expected_source }

(* Identical input must produce byte-identical output, so the ordering has to
   be total: two findings differing only in their suggested fix still need a
   stable relative order. *)
let compare a b =
  match Loc.compare a.loc b.loc with
  | 0 ->
      Stdlib.compare
        ( a.rule_id,
          Severity.rank a.severity,
          a.message,
          a.suggestion,
          a.replacement,
          a.expected_source )
        ( b.rule_id,
          Severity.rank b.severity,
          b.message,
          b.suggestion,
          b.replacement,
          b.expected_source )
  | ordering -> ordering
