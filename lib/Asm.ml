open Sexplib.Std

type operand =
  | Imm of int
  | Register
  [@@deriving sexp]

type instruction =
  | Mov of operand * operand
  | Ret
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
