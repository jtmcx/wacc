open Sexplib.Std

type reg =
  | AX
  | R10
  [@@deriving sexp]

type operand =
  | Imm of int
  | Reg of reg
  | Pseudo of string
  | Stack of int
  [@@deriving sexp]

type unary_operator =
  | Neg
  | Not
  [@@deriving sexp]

type instruction =
  | Mov of operand * operand
  | Unary of unary_operator * operand
  | AllocateStack of int
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
