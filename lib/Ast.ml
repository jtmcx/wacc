open Sexplib.Std

type unary_operator =
  | Complement
  | Negate
  [@@deriving sexp]

type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  [@@deriving sexp]

type exp =
  | Constant of string
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp
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
