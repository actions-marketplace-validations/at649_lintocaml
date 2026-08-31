open Lintocaml_engine

type t = { contents : string; line_offsets : int array }

let of_string contents =
  let offsets = ref [ 0 ] in
  String.iteri
    (fun index character ->
      if Char.equal character '\n' then offsets := (index + 1) :: !offsets)
    contents;
  { contents; line_offsets = Array.of_list (List.rev !offsets) }

let read path =
  match In_channel.with_open_bin path In_channel.input_all with
  | contents -> Ok (of_string contents)
  | exception Sys_error message -> Error message

let offset source line column =
  if line < 1 || line > Array.length source.line_offsets || column < 0 then None
  else
    let line_start = source.line_offsets.(line - 1) in
    let line_end =
      if line = Array.length source.line_offsets then String.length source.contents
      else source.line_offsets.(line) - 1
    in
    if column > line_end - line_start then None else Some (line_start + column)

let range source (location : Loc.t) =
  Option.bind (offset source location.line location.col) (fun start ->
      Option.bind (offset source location.end_line location.end_col) (fun finish ->
          if finish < start then None else Some (start, finish)))

let slice source location =
  Option.map
    (fun (start, finish) -> String.sub source.contents start (finish - start))
    (range source location)
