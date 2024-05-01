let codegen_exp (Ast.Constant x) =
  (* TODO: out of bounds errors. *)
  Asm.Imm (int_of_string x)

let codegen_statement (Ast.Return e) =
  [Asm.Mov (codegen_exp e, Asm.Register); Asm.Ret]

let codegen_function (Ast.Function fn) =
  Asm.Function {
    fn_name = fn.fn_name;
    fn_body = codegen_statement fn.fn_body
  }

let codegen (Ast.Program fn) =
  Asm.Program (codegen_function fn)
