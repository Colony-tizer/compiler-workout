(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let castBoolToInt value =
      if value then 1 else 0;;
    let castIntToBool value =
      value <> 0;;

    let performOperation operator value1 value2 =
      match operator with
      | "!!" -> castBoolToInt(castIntToBool value1 || castIntToBool value2)
      | "&&" -> castBoolToInt(castIntToBool value1 && castIntToBool value2)
      | "==" -> castBoolToInt(value1 == value2)
      | "!=" -> castBoolToInt(value1 != value2)
      | "<=" -> castBoolToInt(value1 <= value2)
      | "<"  -> castBoolToInt(value1 < value2)
      | ">=" -> castBoolToInt(value1 >= value2)
      | ">"  -> castBoolToInt(value1 > value2)
      | "+"  -> value1 + value2
      | "-"  -> value1 - value2
      | "*"  -> value1 * value2
      | "/"  -> value1 / value2
      | "%"  -> value1 mod value2
      |  _   -> failwith (Printf.sprintf "Undefined operator %s" operator)
    ;;

    let rec eval state expression =
      match expression with
      | Const (c) -> c
      | Var (var) -> state var
      | Binop (operator, value1, value2) ->
         let eval_val1 = eval state value1 in
         let eval_val2 = eval state value2 in
         performOperation operator eval_val1 eval_val2
    ;;
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
     *)
    let parseBinop operations = List.map (fun operation -> ostap ($(operation)), fun first second -> Binop (operation, first, second)) operations
     ostap (
      primary: const | var | binop;
      const: value:DECIMAL {Const value};
      var: name:IDENT {Var name};
      binop: -"(" parse -")";
      parse: !(Util.expr
        (fun x -> x)
        [|
          `Lefta, parseBinop ["!!"];
          `Lefta, parseBinop ["&&"];
          `Nona, parseBinop ["=="; "!="; "<="; "<"; ">="; ">"];
          `Lefta, parseBinop ["+"; "-"];
          `Lefta, parseBinop ["*"; "/"; "%"]
        |] primary
      )
     );;

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval config stmt = match config, stmt with
      | (s, z::i, o), (Read x)        -> (Expr.update x z s, i, o)
      | (s, i, o),    (Write e)       -> (s, i, o @ [Expr.eval s e])
      | (s, i, o),    (Assign (x, e)) -> (Expr.update x (Expr.eval s e) s, i, o)
      | _,            (Seq (t1, t2))  -> let config' = eval config t1 in eval config' t2
    ;;

    (* Statement parser *)
    ostap (
     primary: read | write | assign;
      read: -"read" -"(" v:IDENT -")" {Read v};
      write: -"write" -"(" expression:!(Expr.parse) -")" {Write expression};
      assign: variable:IDENT -":=" expression:!(Expr.parse) {Assign (variable, expression)};
      parse: !(Ostap.Util.expr
        (fun x -> x)
        [|
          `Righta, [ostap (";"), fun state1 state2 -> Seq (state1, state2)]
        |] primary
      )
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
