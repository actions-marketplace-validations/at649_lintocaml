type t = { file : string; line : int; col : int; end_line : int; end_col : int }

let compare a b =
  match String.compare a.file b.file with
  | 0 ->
      Stdlib.compare
        (a.line, a.col, a.end_line, a.end_col)
        (b.line, b.col, b.end_line, b.end_col)
  | ordering -> ordering

let pp ppf t = Fmt.pf ppf "%s:%d:%d" t.file t.line (t.col + 1)
