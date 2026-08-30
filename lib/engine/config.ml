type profile_selection = Profile_default | Profile_idiomatic | Profile_pedantic
type override_profile = Use_profile of profile_selection | Disable_all

type path_override = {
  paths : string list;
  profile : override_profile option;
  rules : (string * Severity.t option) list;
}

type t = {
  profile : profile_selection;
  overrides : (string * Severity.t option) list;
  path_overrides : path_override list;
  root_dir : string option;
}

let profile_of_string = function
  | "default" -> Some Profile_default
  | "idiomatic" -> Some Profile_idiomatic
  | "pedantic" -> Some Profile_pedantic
  | _ -> None

let default =
  { profile = Profile_default; overrides = []; path_overrides = []; root_dir = None }

let profile cfg = cfg.profile
let with_profile profile cfg = { cfg with profile }
let with_root_dir root_dir cfg = { cfg with root_dir = Some root_dir }

let profile_severity profile (rule : Rule.t) =
  match (profile, rule.profile) with
  | Profile_pedantic, _ -> Some rule.default_severity
  | Profile_idiomatic, (Rule.Default | Rule.Idiomatic) -> Some rule.default_severity
  | Profile_default, Rule.Default -> Some rule.default_severity
  | (Profile_default | Profile_idiomatic), Rule.Pedantic | Profile_default, Rule.Idiomatic
    ->
      None

let severity_for cfg (rule : Rule.t) =
  match List.assoc_opt rule.id cfg.overrides with
  | Some severity -> severity
  | None -> profile_severity cfg.profile rule

let normalise_path path =
  let path = String.map (function '\\' -> '/' | c -> c) path in
  let rec drop_dot_prefix path =
    if String.starts_with ~prefix:"./" path then
      drop_dot_prefix (String.sub path 2 (String.length path - 2))
    else path
  in
  drop_dot_prefix path

let path_relative_to ~root path =
  let root = normalise_path root in
  let path = normalise_path path in
  let prefix = if String.ends_with ~suffix:"/" root then root else root ^ "/" in
  if String.starts_with ~prefix path then
    String.sub path (String.length prefix) (String.length path - String.length prefix)
  else path

let split_path path =
  normalise_path path |> String.split_on_char '/' |> List.filter (( <> ) "")

(* Matches one path segment against a pattern containing [*] (any run of
   characters) and [?] (exactly one). Memoised on (pattern_pos, value_pos)
   because the naive recursion is exponential on patterns like [a*a*a*b]. *)
let segment_matches pattern value =
  let pattern_length = String.length pattern in
  let value_length = String.length value in
  let memo = Hashtbl.create 16 in
  let rec loop pattern_pos value_pos =
    match Hashtbl.find_opt memo (pattern_pos, value_pos) with
    | Some result -> result
    | None ->
        let result =
          if pattern_pos = pattern_length then value_pos = value_length
          else
            match pattern.[pattern_pos] with
            | '*' ->
                loop (pattern_pos + 1) value_pos
                || (value_pos < value_length && loop pattern_pos (value_pos + 1))
            | '?' -> value_pos < value_length && loop (pattern_pos + 1) (value_pos + 1)
            | character ->
                value_pos < value_length
                && Char.equal character value.[value_pos]
                && loop (pattern_pos + 1) (value_pos + 1)
        in
        Hashtbl.add memo (pattern_pos, value_pos) result;
        result
  in
  (* A pattern with no metacharacter is the overwhelmingly common case and does
     not need the table. *)
  if String.exists (fun c -> c = '*' || c = '?') pattern then loop 0 0
  else String.equal pattern value

(* Matches a whole path, where [**] spans any number of segments.

   Both sides are arrays so a recursive position is just an index. The
   recursion only advances, so (pattern_index, path_index) identifies a unique
   pair of suffixes and is a sound memo key. *)
let glob_matches pattern path =
  let pattern = Array.of_list (split_path pattern) in
  let path = Array.of_list (split_path path) in
  let pattern_length = Array.length pattern in
  let path_length = Array.length path in
  let memo = Hashtbl.create 16 in
  let rec loop pattern_index path_index =
    match Hashtbl.find_opt memo (pattern_index, path_index) with
    | Some result -> result
    | None ->
        let result =
          if pattern_index = pattern_length then path_index = path_length
          else if String.equal pattern.(pattern_index) "**" then
            (* Consume no segments, or consume one and stay on the [**]. *)
            loop (pattern_index + 1) path_index
            || (path_index < path_length && loop pattern_index (path_index + 1))
          else
            path_index < path_length
            && segment_matches pattern.(pattern_index) path.(path_index)
            && loop (pattern_index + 1) (path_index + 1)
        in
        Hashtbl.add memo (pattern_index, path_index) result;
        result
  in
  loop 0 0

let override_matches cfg path path_override =
  let path =
    match cfg.root_dir with
    | Some root -> path_relative_to ~root path
    | None -> normalise_path path
  in
  List.exists (fun pattern -> glob_matches pattern path) path_override.paths

let severity_for_path cfg (rule : Rule.t) path =
  let apply severity path_override =
    if override_matches cfg path path_override then
      let severity =
        match path_override.profile with
        | None -> severity
        | Some Disable_all -> None
        | Some (Use_profile profile) -> profile_severity profile rule
      in
      match List.assoc_opt rule.id path_override.rules with
      | Some rule_severity -> rule_severity
      | None -> severity
    else severity
  in
  List.fold_left apply (severity_for cfg rule) cfg.path_overrides

let strip_comment line =
  let rec loop quoted escaped index =
    if index = String.length line then line
    else
      match line.[index] with
      | '#' when not quoted -> String.sub line 0 index
      | '"' when not escaped -> loop (not quoted) false (index + 1)
      | '\\' when quoted -> loop quoted (not escaped) (index + 1)
      | _ -> loop quoted false (index + 1)
  in
  loop false false 0

let split_setting line =
  String.index_opt line '='
  |> Option.map (fun index ->
      ( String.trim (String.sub line 0 index),
        String.trim (String.sub line (index + 1) (String.length line - index - 1)) ))

let unquote value =
  let length = String.length value in
  if length >= 2 && value.[0] = '"' && value.[length - 1] = '"' then
    Some (String.sub value 1 (length - 2))
  else if String.contains value '"' then None
  else Some value

let parse_string_array value =
  let length = String.length value in
  if length < 2 || value.[0] <> '[' || value.[length - 1] <> ']' then None
  else
    let contents = String.sub value 1 (length - 2) in
    let contents_length = String.length contents in
    let rec skip_space index =
      if index < contents_length && (contents.[index] = ' ' || contents.[index] = '\t')
      then skip_space (index + 1)
      else index
    in
    let read_string index =
      let buffer = Buffer.create 16 in
      let rec loop escaped index =
        if index = contents_length then None
        else
          match (escaped, contents.[index]) with
          | true, (('"' | '\\') as character) ->
              Buffer.add_char buffer character;
              loop false (index + 1)
          | true, _ -> None
          | false, '\\' -> loop true (index + 1)
          | false, '"' -> Some (Buffer.contents buffer, index + 1)
          | false, character ->
              Buffer.add_char buffer character;
              loop false (index + 1)
      in
      loop false index
    in
    let rec loop index acc =
      let index = skip_space index in
      if index = contents_length then Some (List.rev acc)
      else if contents.[index] <> '"' then None
      else
        match read_string (index + 1) with
        | None -> None
        | Some (item, index) ->
            let index = skip_space index in
            if index = contents_length then Some (List.rev (item :: acc))
            else if contents.[index] <> ',' then None
            else
              let index = skip_space (index + 1) in
              if index = contents_length then Some (List.rev (item :: acc))
              else loop index (item :: acc)
    in
    loop 0 []

let parse_severity value =
  match unquote value with
  | Some "off" -> Ok None
  | Some value -> (
      match Severity.of_string value with
      | Some severity -> Ok (Some severity)
      | None -> Error (Fmt.str "unknown severity %S" value))
  | None -> Error (Fmt.str "invalid string %S" value)

let parse_profile value =
  match unquote value with
  | Some value -> (
      match profile_of_string value with
      | Some profile -> Ok profile
      | None -> Error (Fmt.str "unknown profile %S" value))
  | None -> Error (Fmt.str "invalid string %S" value)

let parse_rule ~known_rule_ids (key, value) =
  if List.mem key known_rule_ids then
    Result.map (fun severity -> (key, severity)) (parse_severity value)
  else Error (Fmt.str "unknown rule id %S" key)

type section = Root | Rules | Override | Override_rules

let parse_string ~known_rule_ids source =
  let finish_override cfg current errors lineno =
    match current with
    | None -> (cfg, errors)
    | Some path_override when path_override.paths = [] ->
        (cfg, Fmt.str "line %d: override has no paths" lineno :: errors)
    | Some path_override ->
        ({ cfg with path_overrides = path_override :: cfg.path_overrides }, errors)
  in
  let step (cfg, section, current, errors) (lineno, raw_line) =
    let add_error message =
      (cfg, section, current, Fmt.str "line %d: %s" lineno message :: errors)
    in
    match String.trim (strip_comment raw_line) with
    | "" -> (cfg, section, current, errors)
    | "[[overrides]]" ->
        let cfg, errors = finish_override cfg current errors (lineno - 1) in
        let path_override = { paths = []; profile = None; rules = [] } in
        (cfg, Override, Some path_override, errors)
    | "[rules]" ->
        let cfg, errors = finish_override cfg current errors (lineno - 1) in
        (cfg, Rules, None, errors)
    | "[overrides.rules]" -> (
        match current with
        | Some _ -> (cfg, Override_rules, current, errors)
        | None -> add_error "[overrides.rules] requires a preceding [[overrides]]")
    | line when String.starts_with ~prefix:"[" line ->
        add_error (Fmt.str "unsupported section %S" line)
    | line -> (
        match split_setting line with
        | None -> add_error "expected 'key = value'"
        | Some (key, value) -> (
            match (section, current, key) with
            | Root, _, "profile" -> (
                match parse_profile value with
                | Ok profile -> ({ cfg with profile }, section, current, errors)
                | Error error -> add_error error)
            | Rules, _, "profile" -> add_error "profile belongs before [rules]"
            | (Root | Rules), _, _ -> (
                match parse_rule ~known_rule_ids (key, value) with
                | Ok rule ->
                    ( { cfg with overrides = rule :: cfg.overrides },
                      section,
                      current,
                      errors )
                | Error error -> add_error error)
            | Override, Some path_override, "paths" -> (
                match parse_string_array value with
                | Some (_ :: _ as paths) ->
                    (cfg, section, Some { path_override with paths }, errors)
                | Some [] -> add_error "paths must not be empty"
                | None -> add_error "paths must be an array of strings")
            | Override, Some path_override, "profile" -> (
                match unquote value with
                | Some "off" ->
                    ( cfg,
                      section,
                      Some { path_override with profile = Some Disable_all },
                      errors )
                | _ -> (
                    match parse_profile value with
                    | Ok profile ->
                        ( cfg,
                          section,
                          Some { path_override with profile = Some (Use_profile profile) },
                          errors )
                    | Error error -> add_error error))
            | Override, Some _, _ -> add_error (Fmt.str "unknown override setting %S" key)
            | Override_rules, Some path_override, _ -> (
                match parse_rule ~known_rule_ids (key, value) with
                | Ok rule ->
                    ( cfg,
                      section,
                      Some { path_override with rules = rule :: path_override.rules },
                      errors )
                | Error error -> add_error error)
            | (Override | Override_rules), None, _ ->
                add_error "override setting requires [[overrides]]"))
  in
  let numbered =
    List.mapi (fun index line -> (index + 1, line)) (String.split_on_char '\n' source)
  in
  let cfg, _, current, errors = List.fold_left step (default, Root, None, []) numbered in
  let cfg, errors = finish_override cfg current errors (List.length numbered) in
  match errors with
  | [] -> Ok { cfg with path_overrides = List.rev cfg.path_overrides }
  | _ -> Error (List.rev errors)
