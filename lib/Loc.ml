type t = {
  line : int;
  column : int;
}

type span = {
  span_start : t;
  span_end : t;
}

let show x =
  Printf.sprintf "%d:%d" x.line x.column

let show_span s =
  let x1 = show s.span_start in
  let x2 = show s.span_end in
  Printf.sprintf "%s,%s" x1 x2

let pp fmt x =
  Format.pp_print_string fmt (show x)

let pp_span fmt s =
  Format.pp_print_string fmt (show_span s)

let make l c =
  { line = l; column = c }

let make_span l1 c1 l2 c2 =
  { span_start = make l1 c1; span_end = make l2 c2 }

let of_string s =
  let regex = Str.regexp "^[0-9]+:[0-9]+$" in
  if Str.string_match regex s 0
    then Some (Scanf.sscanf s "%d:%d" make)
    else None

let span_of_string s =
  let regex = Str.regexp "^[0-9]+:[0-9]+,[0-9]+:[0-9]+$" in
  if Str.string_match regex s 0
    then Some (Scanf.sscanf s "%d:%d,%d:%d" make_span)
    else None

let sexp_of x =
  Sexplib.Sexp.Atom (show x)

let sexp_of_span s =
  Sexplib.Sexp.Atom (show_span s)

let string_of_sexp sexp =
  match sexp with
  | Sexplib.Sexp.List _ -> raise (Failure "TODO")
  | Sexplib.Sexp.Atom s -> s

let of_sexp sexp =
  match (of_string (string_of_sexp sexp)) with
  | Some pos -> pos
  | None     -> raise (Failure "TODO")

let span_of_sexp sexp =
  match (span_of_string (string_of_sexp sexp)) with
  | Some span -> span
  | None      -> raise (Failure "TODO")

let of_lexpos (p : Lexing.position) =
  { line   = p.pos_lnum;
    column = p.pos_cnum - p.pos_bol
  }

let span_of_lexpos (p1, p2) =
  { span_start = of_lexpos p1;
    span_end   = of_lexpos p2
  }

