open GT       
open Language.Expr
open Language.Stmt

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Language.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let evalInsn config insn = match config, insn with
  | (y::x::stack, conf),                (BINOP op) -> ((Language.Expr.performOperation op x y)::stack, conf)
  | (stack, conf),                      (CONST x)  -> (x::stack, conf)
  | (stack, (state, x::input, output)), READ       -> (x::stack, (state, input, output))
  | (x::stack, (state, input, output)), WRITE      -> (stack, (state, input, output @ [x]))
  | (stack, (state, input, output)),    (LD z)     -> ((state z)::stack, (state, input, output))
  | (x::stack, (state, input, output)), (ST z)     -> (stack, (Language.Expr.update z x state, input, output))
;;
let rec eval config prg = match prg with
  | insn::prg -> eval (evalInsn config insn) prg
  | []        -> config
;;

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compileExpr expr = match expr with
  | Const x            -> [CONST x]
  | Var z              -> [LD z]
  | Binop (op, e1, e2) -> compileExpr e1 @ compileExpr e2 @ [BINOP op]
;;
let rec compile stmt = match stmt with
  | Read z        -> [READ; ST z]
  | Write e       -> compileExpr e @ [WRITE]
  | Assign (z, e) -> compileExpr e @ [ST z]
  | Seq (t1, t2)  -> compile t1 @ compile t2
;;
let rec compile =
  let rec expr = function
  | Language.Expr.Var   x          -> [LD x]
  | Language.Expr.Const n          -> [CONST n]
  | Language.Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Language.Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
  | Language.Stmt.Read x        -> [READ; ST x]
  | Language.Stmt.Write e       -> expr e @ [WRITE]
  | Language.Stmt.Assign (x, e) -> expr e @ [ST x]
;;
