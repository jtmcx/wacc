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
let fixup_pseudos instrs =
  let frame = Frame.create () in
  let fixup = function
    | Asm.Pseudo x -> Asm.Stack (Frame.lookup x frame)
    | op           -> op
  in
  let fixup_instruction = function
    | Asm.Mov (src, dst)  -> Asm.Mov (fixup src, fixup dst)
    | Asm.Unary (op, dst) -> Asm.Unary (op, fixup dst)
    | Asm.AllocateStack _ -> raise (Failure "Unexpected AllocateStack")
    | Asm.Ret             -> Asm.Ret
  in
  (* Patch every pseudo variable with a stack offset. Prepend the entire
     instruction block with a single stack allocation for all variables. *)
  Asm.AllocateStack (Frame.size frame) :: List.map fixup_instruction instrs


(** [fixup_movs instrs] witll replace any instance of 'mov %m(%rbp)
    %n(%rbp)' in [instrs] with two instructions: 'mov %m(%rbp) %r10d'
    and 'mov %r10d %n(%rbp)'. *)
let fixup_movs instrs =
  let open Asm in
  let out = Queue.create () in
  let push x = Queue.push x out in
  let fixup_instruction = function
    | Mov (Stack k1, Stack k2) ->
        push (Mov (Stack k1, Reg R10));
        push (Mov (Reg R10, Stack k2))
    | x -> push x
  in
  List.iter fixup_instruction instrs;
  List.of_seq (Queue.to_seq out)


let emit_value = function
  | Tacky.Constant x -> Asm.Imm x
  | Tacky.Var x      -> Asm.Pseudo x

let emit_unary out op src dst =
  let op' = match op with
    | Tacky.Complement -> Asm.Not
    | Tacky.Negate     -> Asm.Neg
  in
  let src' = emit_value src in
  let dst' = emit_value dst in
  Queue.push (Asm.Mov (src', dst')) out;
  Queue.push (Asm.Unary (op', dst')) out

let emit_instruction out = function
  | Tacky.Return x ->
      let src = emit_value x in
      let dst = Asm.Reg Asm.AX in
      Queue.push (Asm.Mov (src, dst)) out;
      Queue.push Asm.Ret out
  | Tacky.Unary (op, src, dst) ->
      emit_unary out op src dst

let emit_body instructions =
  let out = Queue.create () in
  List.iter (emit_instruction out) instructions;
  List.of_seq (Queue.to_seq out)

let emit (Tacky.Program (Tacky.Function fn)) =
  let body = emit_body fn.fn_body in
  let body = fixup_pseudos body in
  let body = fixup_movs body in
  Asm.Program (Asm.Function {
    fn_name = fn.fn_name;
    fn_body = body
  })

