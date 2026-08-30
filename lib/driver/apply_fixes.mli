(** Rewriting source files from the fixes rules attach to their findings. *)

type skip_reason = Overlapping | Stale_source | Invalid

type result = {
  applied : int;
  skipped : int;
  errors : string list;
  skipped_reasons : (skip_reason * int) list;
}

val skip_reason_to_string : skip_reason -> string

val run : Lintocaml_engine.Diagnostic.t list -> result
(** Applies every mechanically safe fix.

    Three guarantees, in order of importance:

    - A fix is skipped when the source no longer matches what the rule saw, so a stale
      compiler artifact cannot corrupt a file.
    - Overlapping fixes are skipped rather than applied in sequence, since applying one
      invalidates the offsets of the other.
    - Files are replaced atomically, so an interrupted run leaves either the original or
      the fully rewritten file, never a truncated one. *)
