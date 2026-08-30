(** Rule selection: which rules run, at what severity, over which paths.

    The parser accepts a documented subset of TOML. Everything below the selection API -
    tokenising, globbing, severity parsing - is private, so the file format can change
    without touching callers. *)

type profile_selection =
  | Profile_default  (** Bugs and near-certain mistakes only. *)
  | Profile_idiomatic  (** Adds style and idiom rules. *)
  | Profile_pedantic  (** Everything. *)

type t

val default : t
(** The default profile with no overrides. *)

val profile_of_string : string -> profile_selection option

val with_root_dir : string -> t -> t
(** Anchors relative path patterns. Overrides are written relative to the directory
    holding the config file, not to the working directory, so that a config means the same
    thing wherever lintocaml is invoked from. *)

val profile : t -> profile_selection

val with_profile : profile_selection -> t -> t
(** Overrides the profile, as [--profile] does over a config file. *)

val severity_for : t -> Rule.t -> Severity.t option
(** The severity a rule runs at, or [None] when the profile or an explicit override
    disables it. *)

val severity_for_path : t -> Rule.t -> string -> Severity.t option
(** As {!severity_for}, but applying any path overrides that match [path]. Later overrides
    win, so a config reads top to bottom. *)

val parse_string : known_rule_ids:string list -> string -> (t, string list) result
(** Parses a config, returning every error rather than only the first.

    An unrecognised rule id is an error, never a silent ignore: a typo would otherwise
    disable a rule invisibly for as long as nobody re-reads the config. *)

val glob_matches : string -> string -> bool
(** [glob_matches pattern path] where [*] and [?] match within one segment and [**] spans
    any number of segments. Exposed for testing: the matcher is memoised and its
    correctness is not obvious by inspection. *)
