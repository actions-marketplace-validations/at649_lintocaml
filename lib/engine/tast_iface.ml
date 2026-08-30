(* The only module that touches compiler-libs.

   compiler-libs has no stable API, so a new compiler release breaks this file
   and nothing else. Do not open Typedtree anywhere but here. *)

open Expr_view

let convert_location (location : Location.t) =
  let start = location.loc_start in
  let end_ = location.loc_end in
  { Loc.file = start.pos_fname;
    line = start.pos_lnum;
    col = start.pos_cnum - start.pos_bol;
    end_line = end_.pos_lnum;
    end_col = end_.pos_cnum - end_.pos_bol }

let immediate_paths =
  [ Predef.path_int; Predef.path_bool; Predef.path_char; Predef.path_unit ]

let boxed_paths =
  [ Predef.path_string; Predef.path_bytes; Predef.path_float;
    Predef.path_list; Predef.path_array; Predef.path_option; Predef.path_nativeint;
    Predef.path_int32; Predef.path_int64; Predef.path_floatarray ]

let constructor_is_boxed (constructor : Types.constructor_declaration) =
  match constructor.cd_args with
  | Types.Cstr_tuple (_ :: _) | Types.Cstr_record _ -> true
  | Types.Cstr_tuple [] -> false

let find_local_type local_types path =
  match path with
  | Path.Pident id ->
      List.find_map
        (fun (candidate, declaration) ->
          if Ident.same id candidate then Some declaration else None)
        local_types
  | _ -> None

let find_type local_types ~fallback_env env path =
  match Env.find_type path env with
  | declaration -> Some declaration
  | exception Not_found -> (
      match Env.find_type path fallback_env with
      | declaration -> Some declaration
      | exception Not_found -> find_local_type local_types path)

let classify_constr local_types ~fallback_env env path =
  if List.exists (Path.same path) immediate_paths then Immediate
  else if List.exists (Path.same path) boxed_paths then Boxed
  else
    match find_type local_types ~fallback_env env path with
    | None -> Unknown_class
    | Some declaration -> (
        match declaration.type_kind with
        | Types.Type_record (_, (Record_regular | Record_float)) -> Boxed
        | Types.Type_variant (constructors, Variant_regular)
          when constructors <> []
               && List.for_all constructor_is_boxed constructors ->
            Boxed
        | Types.Type_variant (constructors, Variant_regular)
          when List.for_all (fun constructor -> not (constructor_is_boxed constructor)) constructors ->
            Immediate
        | Types.Type_abstract _ when Option.is_none declaration.type_manifest -> Abstract
        | _ -> Unknown_class)

let expanded_type env ty =
  match (try Some (Ctype.expand_head env ty) with _ -> None)
        [@lintocaml.allow "swallowed-exception"]
  with
  | None -> None
  | Some ty -> Some ty

let classify_type local_types ~fallback_env env ty =
  match expanded_type env ty with
  | None -> Unknown_class
  | Some ty -> (
      match Types.get_desc ty with
      | Types.Tconstr (path, _, _) -> classify_constr local_types ~fallback_env env path
      | Types.Ttuple _ -> Boxed
      | Types.Tarrow _ -> Functional
      | _ -> Unknown_class)

let type_name env ty =
  match expanded_type env ty with
  | Some ty -> (
      match Types.get_desc ty with
      | Types.Tconstr (path, _, _) -> Some (Path.name path)
      | _ -> None)
  | None -> None

(* The single place that knows how a Texp_apply argument is represented. 5.2
   uses [expression option]; 5.4 wraps it in [arg_or_omitted]. Anything needing
   the positional arguments of a call goes through here. *)
let applied_expression (arg : _) : Typedtree.expression option =
#if OCAML_VERSION >= (5, 4, 0)
  match arg with Typedtree.Arg e -> Some e | Typedtree.Omitted () -> None
#else
  arg
#endif

let positional_arguments args =
  List.filter_map
    (fun (label, arg) ->
      match label with
      | Asttypes.Labelled _ | Asttypes.Optional _ -> None
      | Asttypes.Nolabel -> applied_expression arg)
    args

let convert_constant (c : Asttypes.constant) =
  match c with
  | Asttypes.Const_int n -> Int n
  | Asttypes.Const_int32 n -> Int32 n
  | Asttypes.Const_int64 n -> Int64 n
  | Asttypes.Const_nativeint n -> Nativeint n
  | Asttypes.Const_string (s, _, _) -> String s
  | Asttypes.Const_float f -> Float f
  | Asttypes.Const_char c -> Char c

let rec pattern_catches_all : type k. k Typedtree.general_pattern -> bool =
 fun p ->
  match p.pat_desc with
  | Typedtree.Tpat_any | Typedtree.Tpat_var _ -> true
#if OCAML_VERSION >= (5, 4, 0)
  | Typedtree.Tpat_alias (p, _, _, _, _) -> pattern_catches_all p
#else
  | Typedtree.Tpat_alias (p, _, _, _) -> pattern_catches_all p
#endif
  | Typedtree.Tpat_or (left, right, _) ->
      pattern_catches_all left || pattern_catches_all right
  | _ -> false

module String_set = Set.Make (String)

let rec value_pattern_constructors (pattern : Typedtree.pattern) =
  match pattern.pat_desc with
  | Typedtree.Tpat_construct (_, constructor, _, _) ->
      Some (String_set.singleton constructor.cstr_name)
  | Typedtree.Tpat_or (left, right, _) -> (
      match (value_pattern_constructors left, value_pattern_constructors right) with
      | Some left_names, Some right_names -> Some (String_set.union left_names right_names)
      | _ -> None)
#if OCAML_VERSION >= (5, 4, 0)
  | Typedtree.Tpat_alias (nested, _, _, _, _) -> value_pattern_constructors nested
#else
  | Typedtree.Tpat_alias (nested, _, _, _) -> value_pattern_constructors nested
#endif
  | _ -> None

let rec pattern_constructors : type k.
    k Typedtree.general_pattern -> String_set.t option =
 fun pattern ->
  match pattern.pat_desc with
  | Typedtree.Tpat_construct (_, constructor, _, _) ->
      Some (String_set.singleton constructor.cstr_name)
  | Typedtree.Tpat_value value_pattern ->
      value_pattern_constructors (value_pattern :> Typedtree.pattern)
  | Typedtree.Tpat_or (left, right, _) -> (
      match (pattern_constructors left, pattern_constructors right) with
      | Some left_names, Some right_names -> Some (String_set.union left_names right_names)
      | _ -> None)
#if OCAML_VERSION >= (5, 4, 0)
  | Typedtree.Tpat_alias (nested, _, _, _, _) -> pattern_constructors nested
#else
  | Typedtree.Tpat_alias (nested, _, _, _) -> pattern_constructors nested
#endif
  | _ -> None

let pattern_protects_fatal pattern =
  match pattern_constructors pattern with
  | None -> false
  | Some names ->
      List.for_all
        (fun name -> String_set.mem name names)
        [ "Out_of_memory"; "Stack_overflow"; "Break" ]

let pattern_discards (pattern : Typedtree.pattern) =
  match pattern.pat_desc with Typedtree.Tpat_any -> true | _ -> false

(* True when the pattern matches the empty list, possibly among other shapes.
   Constructor patterns only exist on value patterns, so a computation pattern
   is downcast first. A cons pattern never matches []; inspecting its tail here
   would confuse [x] with the empty-list pattern. *)
let rec pattern_matches_empty : type k. k Typedtree.general_pattern -> bool =
 fun pattern ->
  match pattern.pat_desc with
  | Typedtree.Tpat_value value_pattern -> (
      match (value_pattern :> Typedtree.pattern).pat_desc with
      | Typedtree.Tpat_construct (_, constructor, [], _) ->
          String.equal constructor.cstr_name "[]"
      | _ -> false)
  | Typedtree.Tpat_or (left, right, _) ->
      pattern_matches_empty left || pattern_matches_empty right
#if OCAML_VERSION >= (5, 4, 0)
  | Typedtree.Tpat_alias (p, _, _, _, _) -> pattern_matches_empty p
#else
  | Typedtree.Tpat_alias (p, _, _, _) -> pattern_matches_empty p
#endif
  | _ -> false

(* True only when every value accepted by the pattern is a non-empty list. *)
let rec pattern_requires_nonempty : type k. k Typedtree.general_pattern -> bool =
 fun pattern ->
  match pattern.pat_desc with
  | Typedtree.Tpat_value value_pattern -> (
      match (value_pattern :> Typedtree.pattern).pat_desc with
      | Typedtree.Tpat_construct (_, constructor, [ _; _ ], _) ->
          String.equal constructor.cstr_name "::"
      | _ -> false)
  | Typedtree.Tpat_or (left, right, _) ->
      pattern_requires_nonempty left && pattern_requires_nonempty right
#if OCAML_VERSION >= (5, 4, 0)
  | Typedtree.Tpat_alias (p, _, _, _, _) -> pattern_requires_nonempty p
#else
  | Typedtree.Tpat_alias (p, _, _, _) -> pattern_requires_nonempty p
#endif
  | _ -> false

let pattern_name (pattern : Typedtree.pattern) =
  match pattern.pat_desc with
  | Typedtree.Tpat_var (id, _, _) -> Some (Ident.unique_name id)
  | _ -> None

let caught_ident : type k. k Typedtree.general_pattern -> Ident.t option =
 fun pattern ->
  match pattern.pat_desc with
  | Typedtree.Tpat_var (id, _, _) -> Some id
#if OCAML_VERSION >= (5, 4, 0)
  | Typedtree.Tpat_alias (_, id, _, _, _) -> Some id
#else
  | Typedtree.Tpat_alias (_, id, _, _) -> Some id
#endif
  | _ -> None

let is_caught_exception id (e : Typedtree.expression) =
  match e.exp_desc with
  | Typedtree.Texp_ident (Path.Pident candidate, _, _) -> Ident.same id candidate
  | _ -> false

let rec body_reraises id (e : Typedtree.expression) =
  match e.exp_desc with
  | Typedtree.Texp_apply
      ( { exp_desc = Typedtree.Texp_ident (path, _, _); _ },
#if OCAML_VERSION >= (5, 4, 0)
        [ (_, Typedtree.Arg argument) ] ) ->
#else
        [ (_, Some argument) ] ) ->
#endif
      Expr_view.path_is (Path.name path) [ "Stdlib.raise"; "Stdlib.raise_notrace" ]
      && is_caught_exception id argument
  | Typedtree.Texp_sequence (_, tail) -> body_reraises id tail
  | Typedtree.Texp_let (_, _, body) -> body_reraises id body
  | Typedtree.Texp_ifthenelse (_, then_, Some else_) ->
      body_reraises id then_ && body_reraises id else_
  | _ -> false

let allowed_rules (attrs : Parsetree.attributes) =
  let string_literal expression =
    match expression.Parsetree.pexp_desc with
#if OCAML_VERSION >= (5, 3, 0)
    | Pexp_constant
        { pconst_desc = Pconst_string (value, _, _); _ } ->
        Some value
#else
    | Pexp_constant (Pconst_string (value, _, _)) -> Some value
#endif
    | _ -> None
  in
  let tuple_strings expression =
    match expression.Parsetree.pexp_desc with
#if OCAML_VERSION >= (5, 4, 0)
    | Pexp_tuple [ (_, first); (_, second) ] -> (
#else
    | Pexp_tuple [ first; second ] -> (
#endif
        match (string_literal first, string_literal second) with
        | Some id, Some reason -> Some (id, reason)
        | _ -> None)
    | _ -> None
  in
  List.filter_map
    (fun (a : Parsetree.attribute) ->
      if a.attr_name.txt <> "lintocaml.allow" then None
      else
        match a.attr_payload with
        | Parsetree.PStr
            [ { pstr_desc =
#if OCAML_VERSION >= (5, 3, 0)
                  Pstr_eval
                    ( { pexp_desc =
                          Pexp_constant
                            { pconst_desc = Pconst_string (id, _, _); _ };
                        _ },
                      _ );
#else
                  Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (id, _, _)); _ }, _);
#endif
                _ } ] ->
            Some { rule_id = id; reason = None }
        | Parsetree.PStr
            [ { pstr_desc = Pstr_eval (expression, _); _ } ] -> (
                match tuple_strings expression with
                | Some (id, reason) -> Some { rule_id = id; reason = Some reason }
                | None -> None)
        | _ -> None)
    attrs

let attribute_string_payload (attribute : Parsetree.attribute) =
  match attribute.attr_payload with
  | Parsetree.PStr
      [ { pstr_desc =
#if OCAML_VERSION >= (5, 3, 0)
            Pstr_eval
              ( { pexp_desc =
                    Pexp_constant
                      { pconst_desc = Pconst_string (value, _, _); _ };
                  _ },
                _ );
#else
            Pstr_eval
              ({ pexp_desc = Pexp_constant (Pconst_string (value, _, _)); _ }, _);
#endif
          _ } ] ->
      Some value
  | _ -> None

let resolved_name path =
  match path with
  | Path.Pident id -> Ident.unique_name id
  | _ -> Path.name path

let rec convert local_types ~fallback_env (e : Typedtree.expression) : Expr_view.t =
  let desc =
    match e.exp_desc with
    | Typedtree.Texp_ident (path, _, _) -> Ident (resolved_name path)
    | Typedtree.Texp_constant c -> Const (convert_constant c)
    | Typedtree.Texp_construct (_, cd, args) ->
        Construct
          { name = cd.cstr_name;
            args = List.map (convert local_types ~fallback_env) args }
    | Typedtree.Texp_apply (fn, args) ->
        let callee =
          match fn.exp_desc with
          | Typedtree.Texp_ident (path, _, _) -> Some (resolved_name path)
          | _ -> None
        in
        Apply
          { callee;
            args = List.filter_map (convert_arg local_types ~fallback_env) args }
    | Typedtree.Texp_ifthenelse (c, t, els) ->
        If
          { cond = convert local_types ~fallback_env c;
            then_ = convert local_types ~fallback_env t;
            else_ = Option.map (convert local_types ~fallback_env) els }
#if OCAML_VERSION >= (5, 3, 0)
    | Typedtree.Texp_try (body, cases, _effect_cases) ->
#else
    | Typedtree.Texp_try (body, cases) ->
#endif
        Try
          { body = convert local_types ~fallback_env body;
            handlers =
              List.map
                (fun (c : _ Typedtree.case) ->
                  let reraises =
                    Option.fold ~none:false ~some:(fun id -> body_reraises id c.c_rhs)
                      (caught_ident c.c_lhs)
                  in
                  { catches_all = pattern_catches_all c.c_lhs;
                    guarded = Option.is_some c.c_guard;
                    reraises;
                    protects_fatal =
                      Option.is_none c.c_guard && reraises
                      && pattern_protects_fatal c.c_lhs;
                    h_loc = convert_location c.c_lhs.pat_loc })
                cases }
#if OCAML_VERSION >= (5, 3, 0)
    | Typedtree.Texp_match (scrutinee, cases, _effect_cases, _) ->
#else
    | Typedtree.Texp_match (scrutinee, cases, _) ->
#endif
        let converted_scrutinee =
          convert local_types ~fallback_env scrutinee
        in
        let bool_case
            (case : Typedtree.computation Typedtree.case) =
          match (case.c_lhs.pat_desc, case.c_guard) with
          | Typedtree.Tpat_value pattern, None -> (
              let pattern = (pattern :> Typedtree.pattern) in
              match pattern.pat_desc with
              | Typedtree.Tpat_construct (_, constructor, [], _)
                when String.equal constructor.cstr_name "true"
                     || String.equal constructor.cstr_name "false" ->
                  Some
                    ( constructor.cstr_name,
                      convert local_types ~fallback_env case.c_rhs )
              | _ -> None)
          | _ -> None
        in
        let option_case
            (case : Typedtree.computation Typedtree.case) =
          match (case.c_lhs.pat_desc, case.c_guard) with
          | Typedtree.Tpat_value pattern, None -> (
              let pattern = (pattern :> Typedtree.pattern) in
              match pattern.pat_desc with
              | Typedtree.Tpat_construct (_, constructor, [], _)
                when String.equal constructor.cstr_name "None" ->
                  Some
                    ( `None,
                      convert local_types ~fallback_env case.c_rhs )
              | Typedtree.Tpat_construct (_, constructor, [ argument ], _)
                when String.equal constructor.cstr_name "Some" ->
                  Option.map
                    (fun binding ->
                      ( `Some binding,
                        convert local_types ~fallback_env case.c_rhs ))
                    (pattern_name argument)
              | _ -> None)
          | _ -> None
        in
        let option_cases = List.filter_map option_case cases in
        (match
           ( cases,
             List.filter_map bool_case cases,
             option_cases )
         with
        | [ _; _ ], [ ("true", when_true); ("false", when_false) ]
            , _
        | [ _; _ ], [ ("false", when_false); ("true", when_true) ]
            , _ ->
            Bool_match
              { scrutinee = converted_scrutinee; when_true; when_false }
        | [ _; _ ], _, [ (`Some binding, when_some); (`None, when_none) ]
        | [ _; _ ], _, [ (`None, when_none); (`Some binding, when_some) ]
          when type_name_is [ "Stdlib.option" ] converted_scrutinee ->
            Option_match
              { scrutinee = converted_scrutinee;
                binding;
                when_some;
                when_none }
        | _ ->
            let converted_arms =
              List.map
                (fun (case : _ Typedtree.case) ->
                  convert local_types ~fallback_env case.c_rhs)
                cases
            in
            Match
              { scrutinee = converted_scrutinee;
                arms = converted_arms;
                empty_arm =
                  List.exists
                    (fun (case : Typedtree.computation Typedtree.case) ->
                      pattern_matches_empty case.c_lhs)
                    cases })
    | Typedtree.Texp_sequence (a, b) ->
        Sequence
          (convert local_types ~fallback_env a, convert local_types ~fallback_env b)
    | Typedtree.Texp_let (_, bindings, body) ->
        Let
          { bindings =
              List.map
                (fun (binding : Typedtree.value_binding) ->
                  convert local_types ~fallback_env binding.vb_expr)
                bindings;
            body = convert local_types ~fallback_env body }
    | Typedtree.Texp_function (params, Typedtree.Tfunction_body body) ->
        let names =
          List.map
            (fun (param : Typedtree.function_param) ->
              match (param.fp_arg_label, param.fp_kind) with
              | Asttypes.Nolabel, Typedtree.Tparam_pat pattern -> pattern_name pattern
              | _ -> None)
            params
        in
        Function
          {
            params = List.filter_map Fun.id names;
            (* Set by the structure walker, which is where recursion is
               visible; a lone expression cannot tell. *)
            recursive = false;
            simple_params = List.for_all Option.is_some names;
            body = convert local_types ~fallback_env body;
          }
    | _ -> Other
  in
  { loc = convert_location e.exp_loc;
    ty = classify_type local_types ~fallback_env e.exp_env e.exp_type;
    type_name = type_name e.exp_env e.exp_type;
    desc;
    allowed = allowed_rules e.exp_attributes }

and convert_arg local_types ~fallback_env (label, arg) =
  match label with
  | Asttypes.Labelled _ | Asttypes.Optional _ -> None
  | Asttypes.Nolabel ->
#if OCAML_VERSION >= (5, 4, 0)
      (match arg with
      | Typedtree.Arg e -> Some (convert local_types ~fallback_env e)
      | Typedtree.Omitted () -> None)
#else
      (match arg with
      | Some e -> Some (convert local_types ~fallback_env e)
      | None -> None)
#endif

type annotation =
  | Structure of Typedtree.structure
  | Signature of Typedtree.signature

type load_error =
  | Unsupported_compiler of string
  | Invalid_cmt of string

let resolve_load_directory ~cmt_path directory =
  if not (Filename.is_relative directory) && Sys.file_exists directory then
    Some directory
  else
    let rec search parent remaining =
      if remaining = 0 then None
      else
        let candidate = Filename.concat parent directory in
        if Sys.file_exists candidate then Some candidate
        else
          let next = Filename.dirname parent in
          if String.equal next parent then None else search next (remaining - 1)
    in
    search (Filename.dirname cmt_path) 10

let initialise_load_path path (cmt : Cmt_format.cmt_infos) =
  let resolve directories =
    List.filter_map (resolve_load_directory ~cmt_path:path) directories
  in
  Env.reset_cache ();
  Load_path.init ~auto_include:Load_path.no_auto_include
    ~visible:(resolve cmt.cmt_loadpath.visible)
    ~hidden:(resolve cmt.cmt_loadpath.hidden)

let invalid_cmt_error = function
  | Out_of_memory | Stack_overflow | Sys.Break as exn -> raise exn
  (* A mixed build tree - two compilers writing one _build - surfaces here.
     Saying "unreadable" sends the user hunting for a corrupt file when the
     answer is to rebuild. *)
  | Cmi_format.Error (Cmi_format.Wrong_version_interface (_, version)) ->
      Unsupported_compiler version
  | Cmi_format.Error (Cmi_format.Not_an_interface _) ->
      Invalid_cmt "not a compiler artifact"
  | Cmi_format.Error (Cmi_format.Corrupted_interface _) ->
      Invalid_cmt "corrupted artifact; rebuild the project"
  | Sys_error message | Failure message | Invalid_argument message -> Invalid_cmt message
  | End_of_file -> Invalid_cmt "truncated compiler artifact"
  | exn -> Invalid_cmt (Printexc.to_string exn)

let load_cmt (path : string) =
  match Cmt_format.read_cmt path with
  | exception Cmi_format.Error (Cmi_format.Wrong_version_interface (_, version)) ->
      Error (Unsupported_compiler version)
  (* Printexc has no printer for the compiler's own exceptions, so the default
     rendering is "Cmi_format.Error(_)", which tells a user nothing. Name the
     cases that actually occur. *)
  | exception exn -> Error (invalid_cmt_error exn)
  | cmt -> (
      try
        initialise_load_path path cmt;
        match cmt.cmt_annots with
        | Cmt_format.Implementation structure ->
            Ok
              (Some
                 (Structure structure, cmt.cmt_sourcefile, cmt.cmt_source_digest))
        | Cmt_format.Interface signature ->
            Ok
              (Some
                 (Signature signature, cmt.cmt_sourcefile, cmt.cmt_source_digest))
        | _ -> Ok None
      with
      | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
      | exn -> Error (invalid_cmt_error exn))

let iter_structure (str : Typedtree.structure) ~(f : Expr_view.t -> unit) =
  let super = Tast_iterator.default_iterator in
  let local_types = ref [] in
  let type_declaration self (declaration : Typedtree.type_declaration) =
    local_types := (declaration.typ_id, declaration.typ_type) :: !local_types;
    super.type_declaration self declaration
  in
  let recursive_bindings = ref [] in
  let note_recursive rec_flag (bindings : Typedtree.value_binding list) =
    if rec_flag = Asttypes.Recursive then
      recursive_bindings :=
        List.map (fun (b : Typedtree.value_binding) -> b.vb_expr.exp_loc) bindings
        @ !recursive_bindings
  in
  let collect_expr self (e : Typedtree.expression) =
    (match e.exp_desc with
    | Typedtree.Texp_let (rec_flag, bindings, _) -> note_recursive rec_flag bindings
    | _ -> ());
    super.expr self e
  in
  let collect_item self (item : Typedtree.structure_item) =
    (match item.str_desc with
    | Typedtree.Tstr_value (rec_flag, bindings) -> note_recursive rec_flag bindings
    | _ -> ());
    super.structure_item self item
  in
  let collector =
    { super with type_declaration; expr = collect_expr; structure_item = collect_item }
  in
  collector.structure collector str;
  let structure_allowed = ref [] in
  (* Expressions in match arms where the scrutinee is known to be non-empty.
     Facts are keyed by expression location so they cannot leak into a later
     expression that happens to use the same list binding. *)
  let match_guarded : (Loc.t, string list) Hashtbl.t = Hashtbl.create 16 in
  (* Locations inside a [try] that catches the exception the call raises.
     [try Hashtbl.find t k with Not_found -> 0] handles the partiality
     explicitly, so it is not a defect. *)
  let handled_here : (Loc.t, string list) Hashtbl.t = Hashtbl.create 16 in
  let caught_exception_names cases =
    List.filter_map
      (fun (case : Typedtree.value Typedtree.case) ->
        match case.c_lhs.pat_desc with
        | Typedtree.Tpat_construct (_, cd, _, _) -> Some cd.cstr_name
        | Typedtree.Tpat_any | Typedtree.Tpat_var _ -> Some "_"
        | _ -> None)
      cases
  in
  let note_handled (e : Typedtree.expression) =
    match e.exp_desc with
#if OCAML_VERSION >= (5, 3, 0)
    | Typedtree.Texp_try (body, cases, _effect_cases) ->
#else
    | Typedtree.Texp_try (body, cases) ->
#endif
        let caught = caught_exception_names cases in
        if caught <> [] then begin
          let super = Tast_iterator.default_iterator in
          let mark self (inner : Typedtree.expression) =
            let key = convert_location inner.exp_loc in
            let existing = Option.value (Hashtbl.find_opt handled_here key) ~default:[] in
            Hashtbl.replace handled_here key (caught @ existing);
            super.expr self inner
          in
          let it = { super with expr = mark } in
          it.expr it body
        end
    | _ -> ()
  in
  (* Locations inside the [then] branch of a guard that proves a list is long
     enough, mapped to the proven minimum length.

     [if List.length l > 0 then List.hd l] is correct code. Only comparisons
     against a literal are read, and only the branch the guard proves is
     marked, so nothing here depends on evaluating arithmetic. A guard that
     proves nothing (a dynamic bound, the wrong direction) leaves the call
     reported, which is the safe default. *)
  let length_guarded : (Loc.t, (string * int) list) Hashtbl.t = Hashtbl.create 16 in

  let literal_int (e : Typedtree.expression) =
    match e.exp_desc with
    | Typedtree.Texp_constant (Asttypes.Const_int n) -> Some n
    | _ -> None
  in
  let length_of (e : Typedtree.expression) =
    match e.exp_desc with
    | Typedtree.Texp_apply ({ exp_desc = Typedtree.Texp_ident (path, _, _); _ }, args)
      when Expr_view.path_is (Path.name path) [ "Stdlib.List.length" ] -> (
        match positional_arguments args with
        | [ { exp_desc = Typedtree.Texp_ident (Path.Pident id, _, _); _ } ] ->
            Some (Ident.unique_name id)
        | _ -> None)
    | _ -> None
  in
  (* Returns the minimum length the condition proves, if any. *)
  let proven_minimum (cond : Typedtree.expression) =
    match cond.exp_desc with
    | Typedtree.Texp_apply ({ exp_desc = Typedtree.Texp_ident (op, _, _); _ }, args) -> (
        match positional_arguments args with
        | [ a; b ] -> (
        let operator = Expr_view.strip_stdlib (Path.name op) in
        match (length_of a, literal_int b, literal_int a, length_of b) with
        (* List.length l > n  /  >= n  /  <> 0 *)
        | Some name, Some n, _, _ -> (
            match operator with
            | ">" when n < max_int -> Some (name, n + 1)
            | ">=" -> Some (name, n)
            | "<>" when n = 0 -> Some (name, 1)
            | _ -> None)
        (* n < List.length l  /  n <= List.length l  /  0 <> List.length l *)
        | _, _, Some n, Some name -> (
            match operator with
            | "<" when n < max_int -> Some (name, n + 1)
            | "<=" -> Some (name, n)
            | "<>" when n = 0 -> Some (name, 1)
            | _ -> None)
        | _ -> None)
        | _ -> None)
    | _ -> None
  in
  let note_length_guard (e : Typedtree.expression) =
    match e.exp_desc with
    | Typedtree.Texp_ifthenelse (cond, then_branch, _) -> (
        match proven_minimum cond with
        | None -> ()
        | Some fact ->
            let super = Tast_iterator.default_iterator in
            let mark self (inner : Typedtree.expression) =
              let key = convert_location inner.exp_loc in
              let existing =
                Option.value (Hashtbl.find_opt length_guarded key) ~default:[]
              in
              Hashtbl.replace length_guarded key (fact :: existing);
              super.expr self inner
            in
            let it = { super with expr = mark } in
            it.expr it then_branch)
    | _ -> ()
  in
  (* The index a partial call needs to be valid: [List.hd l] needs length >= 1,
     [List.nth l k] needs length >= k + 1. A non-literal index proves nothing. *)
  let required_length name args =
    match (Expr_view.strip_stdlib name, args) with
    | ("List.hd" | "List.tl"), _ -> Some 1
    | "List.nth", [ _; index ] -> (
        match literal_int index with
        | Some n when n >= 0 && n < max_int -> Some (n + 1)
        | _ -> None)
    | _ -> None
  in
  let bounds_are_proven (e : Typedtree.expression) =
    match e.exp_desc with
    | Typedtree.Texp_apply ({ exp_desc = Typedtree.Texp_ident (path, _, _); _ }, args)
      -> (
        let positional = positional_arguments args in
        match required_length (Path.name path) positional with
        | None -> false
        | Some needed -> (
            match positional with
            | { exp_desc = Typedtree.Texp_ident (Path.Pident id, _, _); _ } :: _ -> (
                let subject = Ident.unique_name id in
                match Hashtbl.find_opt length_guarded (convert_location e.exp_loc) with
                | None -> false
                | Some facts ->
                    List.exists
                      (fun (name, minimum) ->
                        String.equal name subject && minimum >= needed)
                      facts)
            | _ -> false))
    | _ -> false
  in
  (* Which exception each partial function raises, so the handler has to match. *)
  let raises_of name =
    match name with
    | "Stdlib.Hashtbl.find" | "Stdlib.List.assoc" | "Stdlib.List.find"
    | "Stdlib.List.assq" | "Stdlib.List.memq" ->
        Some "Not_found"
    | "Stdlib.List.hd" | "Stdlib.List.tl" | "Stdlib.List.nth" -> Some "Failure"
    | "Stdlib.Option.get" | "Stdlib.Result.get_ok" | "Stdlib.Result.get_error" ->
        Some "Invalid_argument"
    | _ -> None
  in
  let partiality_is_handled (e : Typedtree.expression) =
    match e.exp_desc with
    | Typedtree.Texp_apply ({ exp_desc = Typedtree.Texp_ident (path, _, _); _ }, _) -> (
        match raises_of (Path.name path) with
        | None -> false
        | Some raised -> (
            match Hashtbl.find_opt handled_here (convert_location e.exp_loc) with
            | None -> false
            | Some caught -> List.exists (fun c -> c = raised || c = "_") caught))
    | _ -> false
  in
  let note_match_guards (e : Typedtree.expression) =
    match e.exp_desc with
#if OCAML_VERSION >= (5, 3, 0)
    | Typedtree.Texp_match (scrutinee, cases, _effect_cases, _) ->
#else
    | Typedtree.Texp_match (scrutinee, cases, _) ->
#endif
        (match scrutinee.exp_desc with
        | Typedtree.Texp_ident (Path.Pident id, _, _) ->
            let subject = Ident.unique_name id in
            let empty_already_handled = ref false in
            let mark (body : Typedtree.expression) =
              let iterator = Tast_iterator.default_iterator in
              let expr self (inner : Typedtree.expression) =
                let key = convert_location inner.exp_loc in
                let existing =
                  Option.value (Hashtbl.find_opt match_guarded key) ~default:[]
                in
                Hashtbl.replace match_guarded key (subject :: existing);
                iterator.expr self inner
              in
              let iterator = { iterator with expr } in
              iterator.expr iterator body
            in
            List.iter
              (fun (case : Typedtree.computation Typedtree.case) ->
                if !empty_already_handled || pattern_requires_nonempty case.c_lhs then
                  mark case.c_rhs;
                if Option.is_none case.c_guard && pattern_matches_empty case.c_lhs then
                  empty_already_handled := true)
              cases
        | _ -> ())
    | _ -> ()
  in
  let is_guarded_partial_call (e : Typedtree.expression) =
    match e.exp_desc with
    | Typedtree.Texp_apply
        ( { exp_desc = Typedtree.Texp_ident (path, _, _); _ },
#if OCAML_VERSION >= (5, 4, 0)
          [ (_, Typedtree.Arg list) ] )
#else
          [ (_, Some list) ] )
#endif
      when Expr_view.path_is (resolved_name path) [ "Stdlib.List.hd"; "Stdlib.List.tl" ]
      -> (
        match list.exp_desc with
        | Typedtree.Texp_ident (Path.Pident id, _, _) ->
            let subject = Ident.unique_name id in
            Option.fold ~none:false
              ~some:(List.exists (String.equal subject))
              (Hashtbl.find_opt match_guarded (convert_location e.exp_loc))
        | _ -> false)
    | _ -> false
  in
  let emit (view : Expr_view.t) =
    let view =
      match view.desc with
      | Expr_view.Function fn
        when List.exists
               (fun location -> convert_location location = view.loc)
               !recursive_bindings ->
          { view with desc = Expr_view.Function { fn with recursive = true } }
      | _ -> view
    in
    (* A local attribute is more specific than a floating structure attribute,
       so its audit reason wins when both suppress the same rule. *)
    f { view with allowed = view.allowed @ !structure_allowed }
  in
  let expr self e =
    note_match_guards e;
    note_handled e;
    note_length_guard e;
    let converted = convert !local_types ~fallback_env:str.str_final_env e in
    if bounds_are_proven e then
      emit
        { converted with
          allowed =
            converted.allowed
            @ [ { rule_id = "partial-function";
                  reason = Some "a length guard proves this index is in range" } ] }
    else if partiality_is_handled e then
      emit
        { converted with
          allowed =
            converted.allowed
            @ [ { rule_id = "partial-function";
                  reason = Some "the exception it raises is handled here" } ] }
    else if is_guarded_partial_call e then
      emit
        { converted with
          allowed =
            converted.allowed
            @ [ { rule_id = "partial-function";
                  reason = Some "guarded by an empty-list match arm" } ] }
    else emit converted;
    super.expr self e
  in
  let value_binding self (binding : Typedtree.value_binding) =
    (if pattern_discards binding.vb_pat then
      let discarded =
        convert !local_types ~fallback_env:str.str_final_env binding.vb_expr
      in
      emit
        { loc = convert_location binding.vb_loc;
          ty = Unknown_class;
          type_name = None;
          desc = Discard discarded;
          allowed = allowed_rules binding.vb_attributes });
    super.value_binding self binding
  in
  let structure_item self item =
    match item.Typedtree.str_desc with
    | Typedtree.Tstr_attribute attribute ->
        let inherited = !structure_allowed in
        structure_allowed := allowed_rules [ attribute ] @ inherited;
        f
          { loc = convert_location item.str_loc;
            ty = Unknown_class;
            type_name = None;
            desc =
              Attribute
                { name = attribute.attr_name.txt;
                  payload = attribute_string_payload attribute };
            allowed = inherited }
    | _ -> super.structure_item self item
  in
  let structure self nested =
    let inherited = !structure_allowed in
    super.structure self nested;
    structure_allowed := inherited
  in
  let it = { super with expr; value_binding; structure_item; structure } in
  it.structure it str

let iter_signature (signature : Typedtree.signature) ~(f : Expr_view.t -> unit) =
  let super = Tast_iterator.default_iterator in
  let signature_allowed = ref [] in
  let signature_item self item =
    match item.Typedtree.sig_desc with
    | Typedtree.Tsig_attribute attribute ->
        let inherited = !signature_allowed in
        signature_allowed := allowed_rules [ attribute ] @ inherited;
        f
          { loc = convert_location item.sig_loc;
            ty = Unknown_class;
            type_name = None;
            desc =
              Attribute
                { name = attribute.attr_name.txt;
                  payload = attribute_string_payload attribute };
            allowed = inherited }
    | _ -> super.signature_item self item
  in
  let walk_signature self nested =
    let inherited = !signature_allowed in
    super.signature self nested;
    signature_allowed := inherited
  in
  let iterator = { super with signature_item; signature = walk_signature } in
  iterator.signature iterator signature

let iter_views annotation ~f =
  match annotation with
  | Structure structure -> iter_structure structure ~f
  | Signature signature -> iter_signature signature ~f
