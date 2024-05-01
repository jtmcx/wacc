let emit_operand = function
  | Asm.Register -> "%eax"
  | Asm.Imm x -> Printf.sprintf "$%d" x

let emit_instruction = function
  | Asm.Mov (src, dst) ->
      Printf.printf "\tmovl\t%s, %s\n" (emit_operand src) (emit_operand dst)
  | Asm.Ret ->
      Printf.printf "\tret\n"

let emit_function (Asm.Function fn) =
  Printf.printf "\t.globl %s\n" fn.fn_name;
  Printf.printf "%s:\n" fn.fn_name;
  List.iter emit_instruction fn.fn_body

let emit f (Asm.Program fn) =
  emit_function fn;
  Printf.fprintf f ".section .note.GNU-stack,\"\",@progbits\n"
