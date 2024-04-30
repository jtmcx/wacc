open Sexplib.Std

type exp =
  | Constant of string
  [@@deriving sexp]

type statement =
  | Return of exp
  [@@deriving sexp]

type function_definition =
  | Function of {
      fn_name : string;
      fn_body : statement
    }
  [@@deriving sexp]

type program =
  | Program of function_definition
  [@@deriving sexp]
