(** A context that keeps track of fresh variable names. *)
module Context : sig
  val reset : unit -> unit
  (** Reset the context. *)

  val fresh : unit -> string
  (** Generate a fresh variable name. *)
end = struct
  let n = ref 0
  let reset () = n := 0
  let fresh () = n := !n + 1; Printf.sprintf "tmp%d" !n
end


let convert_binop = function
  | Ast.Add       -> Tacky.Add
  | Ast.Subtract  -> Tacky.Subtract
  | Ast.Multiply  -> Tacky.Multiply
  | Ast.Divide    -> Tacky.Divide
  | Ast.Remainder -> Tacky.Remainder

let convert_unop = function
  | Ast.Complement -> Tacky.Complement
  | Ast.Negate     -> Tacky.Negate


let rec emit_tacky e instructions =
  match e with
  | Ast.Constant x ->
      (* TODO: handle when x is out of bounds. *)
      Tacky.Constant (int_of_string x)
  | Ast.Unary (op, e) ->
      emit_unary op e instructions
  | Ast.Binary (op, e1, e2) ->
      emit_binary op e1 e2 instructions

and emit_unary op e instructions =
  let src = emit_tacky e instructions in
  let dst_name = Context.fresh () in
  let dst = Tacky.Var (dst_name) in
  let tacky_op = convert_unop op in
  Queue.push (Tacky.Unary (tacky_op, src, dst)) instructions;
  dst

and emit_binary op e1 e2 instructions =
  let v1 = emit_tacky e1 instructions in
  let v2 = emit_tacky e2 instructions in
  let dst_name = Context.fresh () in
  let dst = Tacky.Var (dst_name) in
  let tacky_op = convert_binop op in
  Queue.push (Tacky.Binary (tacky_op, v1, v2, dst)) instructions;
  dst

let emit (Ast.Program (Ast.Function fn)) =
  Context.reset ();
  (* Compile the function body to a list of tack instructions. *)
  let instructions (Ast.Return e) =
    let out = Queue.create () in
    let dst = emit_tacky e out in
    Queue.push (Tacky.Return dst) out;
    List.of_seq (Queue.to_seq out)
  in
  Tacky.Program (Tacky.Function {
    fn_name = fn.fn_name;
    fn_body = instructions fn.fn_body
  })
