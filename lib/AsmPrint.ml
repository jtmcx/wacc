let print_operand = function
  | Asm.Reg Asm.AX  -> "%eax"
  | Asm.Reg Asm.R10 -> "%r10d"
  | Asm.Stack n     -> Printf.sprintf "%d(%%rbp)" n
  | Asm.Imm x       -> Printf.sprintf "$%d" x
  | Asm.Pseudo _    -> raise (Failure "TODO")

let print_instruction f = function
  | Asm.Mov (src, dst) ->
      let src' = print_operand src in
      let dst' = print_operand dst in
      Printf.fprintf f "\tmovl\t%s, %s\n" src' dst'
  | Asm.Unary (op, arg) ->
      let op' = match op with
        | Asm.Neg -> "negl"
        | Asm.Not -> "notl"
      in
      Printf.fprintf f "\t%s\t%s\n" op' (print_operand arg)
  | Asm.AllocateStack n ->
      Printf.fprintf f "\tsubq\t$%d, %%rsp\n" n
  | Asm.Ret ->
      Printf.fprintf f "\tmovq\t%%rbp, %%rsp\n";
      Printf.fprintf f "\tpopq\t%%rbp\n";
      Printf.fprintf f "\tret\n"

let print_function f (Asm.Function fn) =
  Printf.fprintf f "\t.globl %s\n" fn.fn_name;
  Printf.fprintf f "%s:\n" fn.fn_name;
  Printf.fprintf f "\tpushq\t%%rbp\n";
  Printf.fprintf f "\tmovq\t%%rsp, %%rbp\n";
  List.iter (print_instruction f) fn.fn_body

let print f (Asm.Program fn) =
  print_function f fn;
  Printf.fprintf f ".section .note.GNU-stack,\"\",@progbits\n"
