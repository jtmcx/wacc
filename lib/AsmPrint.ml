let string_of_operand = function
  | Asm.Reg Asm.AX  -> "%eax"
  | Asm.Reg Asm.DX  -> "%edx"
  | Asm.Reg Asm.R10 -> "%r10d"
  | Asm.Reg Asm.R11 -> "%r11d"
  | Asm.Stack n     -> Printf.sprintf "%d(%%rbp)" n
  | Asm.Imm x       -> Printf.sprintf "$%d" x
  | Asm.Pseudo _    -> raise (Failure "TODO")

let string_of_unop = function
  | Asm.Neg -> "negl"
  | Asm.Not -> "notl"

let string_of_binop = function
  | Asm.Add  -> "addl"
  | Asm.Sub  -> "subl"
  | Asm.Mult -> "imull"

let print_instruction f = function
  | Asm.Mov (src, dst) ->
      let src' = string_of_operand src in
      let dst' = string_of_operand dst in
      Printf.fprintf f "\tmovl\t%s, %s\n" src' dst'
  | Asm.Ret ->
      Printf.fprintf f "\tmovq\t%%rbp, %%rsp\n";
      Printf.fprintf f "\tpopq\t%%rbp\n";
      Printf.fprintf f "\tret\n"
  | Asm.Unary (op, arg) ->
      let op' = string_of_unop op in
      Printf.fprintf f "\t%s\t%s\n" op' (string_of_operand arg)
  | Asm.Binary (op, src, dst) ->
      let op' = string_of_binop op in
      let src' = string_of_operand src in
      let dst' = string_of_operand dst in
      Printf.fprintf f "\t%s\t%s, %s\n" op' src' dst'
  | Asm.Idiv arg ->
      Printf.fprintf f "\tidivl\t%s\n" (string_of_operand arg)
  | Asm.Cdq ->
      Printf.fprintf f "\tcdq\n"
  | Asm.AllocateStack n ->
      Printf.fprintf f "\tsubq\t$%d, %%rsp\n" n

let print_function f (Asm.Function fn) =
  Printf.fprintf f "\t.globl %s\n" fn.fn_name;
  Printf.fprintf f "%s:\n" fn.fn_name;
  Printf.fprintf f "\tpushq\t%%rbp\n";
  Printf.fprintf f "\tmovq\t%%rsp, %%rbp\n";
  List.iter (print_instruction f) fn.fn_body

let print f (Asm.Program fn) =
  print_function f fn;
  Printf.fprintf f ".section .note.GNU-stack,\"\",@progbits\n"
