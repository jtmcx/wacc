(** A [Frame.t] is a map of variable names to stack offsets. Note that
    every variable is four bytes. The size of the frame is simply the
    number of stack variables times four. *)

module Frame = struct
  type t = (string, int) Hashtbl.t

  let create () = Hashtbl.create 8
  let size t = 4 * Hashtbl.length t

  (** Lookup the offset of a variable in the stack frame. If the
      variable doesn't exist, allocate four bytes for the variable. *)
  let lookup x (frame : t) =
    match Hashtbl.find_opt frame x with
    | Some k -> k
    | None ->
        let k = -(size frame) - 4 in
        Hashtbl.add frame x k; k
end


(** Replace pseudo registers with stack allocated variables. *)
let replace_pseudos instrs =
  let frame = Frame.create () in
  let subst = function
    | Asm.Pseudo x -> Asm.Stack (Frame.lookup x frame)
    | op           -> op
  in
  let subst_instruction = function
    | Asm.Mov (src, dst)        -> Asm.Mov (subst src, subst dst)
    | Asm.Unary (op, dst)       -> Asm.Unary (op, subst dst)
    | Asm.Binary (op, src, dst) -> Asm.Binary (op, subst src, subst dst)
    | Asm.Cdq                   -> Asm.Cdq
    | Asm.Idiv src              -> Asm.Idiv (subst src)
    | Asm.Ret                   -> Asm.Ret
    | Asm.AllocateStack _       -> raise (Failure "Unexpected AllocateStack")
  in
  (* Patch every pseudo variable with a stack offset. Prepend the entire
     instruction block with a single stack allocation for all variables. *)
  Asm.AllocateStack (Frame.size frame) :: List.map subst_instruction instrs


let fixup_instructions instrs =
  let open Asm in
  let out = Queue.create () in
  let push x = Queue.push x out in
  let fixup_instruction = function
    | Mov (Stack k1, Stack k2) ->
        push (Mov (Stack k1, Reg R10));
        push (Mov (Reg R10, Stack k2))
    | Binary (op, Stack k1, Stack k2) ->
        push (Mov (Stack k1, Reg R10));
        push (Binary (op, Reg R10, Stack k2))
    | Idiv (Imm x) ->
        push (Mov (Imm x, Reg R10));
        push (Idiv (Reg R10))
    | x -> push x
  in
  List.iter fixup_instruction instrs;
  List.of_seq (Queue.to_seq out)


let convert_unop = function
  | Tacky.Complement -> Asm.Not
  | Tacky.Negate     -> Asm.Neg

let convert_binop = function
  | Tacky.Add      -> Asm.Add
  | Tacky.Subtract -> Asm.Sub
  | Tacky.Multiply -> Asm.Mult
  | _              -> raise (Failure "TODO")

let emit_value = function
  | Tacky.Constant x -> Asm.Imm x
  | Tacky.Var x      -> Asm.Pseudo x

let emit_unary op src dst instructions =
  let op'  = convert_unop op in
  let src' = emit_value src in
  let dst' = emit_value dst in
  Queue.push (Asm.Mov (src', dst')) instructions;
  Queue.push (Asm.Unary (op', dst')) instructions

let emit_binary op src1 src2 dst instructions =
  let op'   = convert_binop op in
  let src1' = emit_value src1 in
  let src2' = emit_value src2 in
  let dst'  = emit_value dst in
  Queue.push (Asm.Mov (src1', dst')) instructions;
  Queue.push (Asm.Binary (op', src2', dst')) instructions

let emit_divide src1 src2 dst instructions =
  let src1' = emit_value src1 in
  let src2' = emit_value src2 in
  let dst'  = emit_value dst in
  Queue.push (Asm.Mov (src1', Asm.Reg Asm.AX)) instructions;
  Queue.push Asm.Cdq instructions;
  Queue.push (Asm.Idiv src2') instructions;
  Queue.push (Asm.Mov (Asm.Reg Asm.AX, dst')) instructions

let emit_remainder src1 src2 dst instructions =
  let src1' = emit_value src1 in
  let src2' = emit_value src2 in
  let dst'  = emit_value dst in
  Queue.push (Asm.Mov (src1', Asm.Reg Asm.AX)) instructions;
  Queue.push Asm.Cdq instructions;
  Queue.push (Asm.Idiv src2') instructions;
  Queue.push (Asm.Mov (Asm.Reg Asm.DX, dst')) instructions

let emit_return value instructions =
  let src = emit_value value in
  let dst = Asm.Reg Asm.AX in
  Queue.push (Asm.Mov (src, dst)) instructions;
  Queue.push Asm.Ret instructions

let emit_instruction out = function
  | Tacky.Return x ->
      emit_return x out
  | Tacky.Unary (op, src, dst) ->
      emit_unary op src dst out
  | Tacky.Binary (Tacky.Divide, src1, src2, dst) ->
      emit_divide src1 src2 dst out
  | Tacky.Binary (Tacky.Remainder, src1, src2, dst) ->
      emit_remainder src1 src2 dst out
  | Tacky.Binary (op, src1, src2, dst) ->
      emit_binary op src1 src2 dst out


let emit_body instructions =
  let out = Queue.create () in
  List.iter (emit_instruction out) instructions;
  List.of_seq (Queue.to_seq out)

let emit (Tacky.Program (Tacky.Function fn)) =
  let body = emit_body fn.fn_body in
  let body = replace_pseudos body in
  let body = fixup_instructions body in
  Asm.Program (Asm.Function {
    fn_name = fn.fn_name;
    fn_body = body
  })

