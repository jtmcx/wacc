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

let rec emit_exp out = function
  | Ast.Constant x ->
      (* TODO: handle when x is out of bounds. *)
      Tacky.Constant (int_of_string x)
  | Ast.Unary (op, e) ->
      emit_unary out op e

and emit_unary out op e =
  let op = match op with
    | Ast.Complement -> Tacky.Complement
    | Ast.Negate     -> Tacky.Negate
  in
  let src = emit_exp out e in
  let dst = Tacky.Var (Context.fresh ()) in
  Queue.push (Tacky.Unary (op, src, dst)) out; dst

let emit_instructions (Ast.Return e) =
  let out = Queue.create () in
  let dst = emit_exp out e in
  Queue.push (Tacky.Return dst) out;
  List.of_seq (Queue.to_seq out)

let emit (Ast.Program (Ast.Function fn)) =
  Context.reset ();
  Tacky.Program (Tacky.Function {
    fn_name = fn.fn_name;
    fn_body = emit_instructions fn.fn_body
  })
