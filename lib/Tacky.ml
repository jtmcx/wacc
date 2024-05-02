open Sexplib.Std

type value =
  | Constant of int
  | Var of string
  [@@deriving sexp]

type unary_operator =
  | Complement
  | Negate
  [@@deriving sexp]

type instruction =
  | Unary of {
      unary_op: unary_operator;
      unary_src: value;
      unary_dst: value;
    }
  | Return of value
  [@@deriving sexp]

type function_definition =
  | Function of {
      fn_name : string;
      fn_body : instruction list
    }
  [@@deriving sexp]

type program =
  | Program of function_definition
  [@@deriving sexp]
