(** {1 Types} *)

type t = {
  line : int;
  column : int;
}

type span = {
  span_start : t;
  span_end : t;
}


(** {1 String Conversions} *)

val show : t -> string
(** Convert a [t] to a string. *)

val show_span : span -> string
(** Convert a [span] to a string. *)

val pp : Format.formatter -> t -> unit
(** Pretty print a given [t]. *)

val pp_span : Format.formatter -> span -> unit
(** Pretty print a given [span]. *)

val of_string : string -> t option
(** Parse a [t] string. *)

val span_of_string : string -> span option
(** Parse a [span] string. *)


(** {1 [Sexp.t] Conversions} *)

val sexp_of : t -> Sexplib.Sexp.t
(** Convert a [t] to an [Sexp.t] *)

val of_sexp : Sexplib.Sexp.t -> t
(** Convert an [Sexp.t] to a [t] *)

val sexp_of_span : span -> Sexplib.Sexp.t
(** Convert a [span] to an [Sexp.t] *)

val span_of_sexp : Sexplib.Sexp.t -> span
(** Convert an [Sexp.t] to a [span] *)


(** {1 [Lexing.position] Conversions} *)

val of_lexpos : Lexing.position -> t
(** Convert a [Lexing.position] to a [t]. *)

val span_of_lexpos : Lexing.position * Lexing.position -> span
(** Convert a pair of [Lexing.position]s to a [span]. *)
