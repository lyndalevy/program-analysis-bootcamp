(* ================================================================
   Exercise 5: Calculator Parser -- Main Driver
   ================================================================

   This file reads expressions from built-in examples, parses them
   with the Menhir-generated parser, and prints + evaluates the
   resulting AST.

   TODO: Implement [string_of_op], [string_of_expr], and [eval].
   The parse + main driver code is provided.

   Run with:  dune exec modules/module0-warmup/exercises/calculator-parser/starter/main.exe
   ================================================================ *)

open Ast

(* ----------------------------------------------------------------
   Part 1: Pretty-Printing
   ---------------------------------------------------------------- *)

(** [string_of_op op] returns "+", "-", "*", or "/". *)
let string_of_op (o : op) : string =
  (* EXERCISE: pattern match on Add, Sub, Mul, Div *)
  match o with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
[@@warning "-32"]

(** [string_of_expr e] returns a fully parenthesized string.
    Examples:
      Num 3            --> "3"
      Var "x"          --> "x"
      Neg (Num 5)      --> "(- 5)"
      BinOp(Add, Num 1, Num 2)  --> "(1 + 2)" *)
let rec string_of_expr (e : expr) : string =
  (* EXERCISE: pattern match on Num, Var, Neg, BinOp
     Hint: add [rec] when ready *)
  match e with
  | Num n -> string_of_int n
  | Var x -> x
  | Neg e1 -> Printf.sprintf "(- %s)" (string_of_expr e1)
  | BinOp (op, e1, e2) ->
      Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_op op) (string_of_expr e2)

(* ----------------------------------------------------------------
   Part 2: Evaluation
   ---------------------------------------------------------------- *)

(** [eval e] evaluates [e] if it contains no variables.
    Returns [Some n] on success, [None] if a Var is encountered.
    Division by zero returns [None]. *)
let rec eval (e : expr) : int option =
  (* EXERCISE: handle Num, Var, Neg, and BinOp.
     For BinOp: evaluate both sides; if both are Some, compute.
     For Div: check for zero denominator.
     Hint: add [rec] when ready *)
  match e with
  | Num n -> Some n
  | Var _ -> None
  | Neg e1 ->
      (match eval e1 with
       | Some n -> Some (-n)
       | None -> None)
  | BinOp (op, e1, e2) ->
      (match eval e1, eval e2 with
       | Some n1, Some n2 ->
           (match op with
            | Add -> Some (n1 + n2)
            | Sub -> Some (n1 - n2)
            | Mul -> Some (n1 * n2)
            | Div -> if n2 = 0 then None else Some (n1 / n2))
       | _, _ -> None)

(* ----------------------------------------------------------------
   Provided: Parse helper and main driver
   ---------------------------------------------------------------- *)

(** [parse_string s] parses the string [s] into an expr. *)
let parse_string (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  Parser.program Lexer.token lexbuf

(** [test_expr s] parses, prints, and evaluates the expression. *)
let test_expr (s : string) : unit =
  Printf.printf "Input:  %s\n" s;
  let e = parse_string s in
  Printf.printf "AST:    %s\n" (string_of_expr e);
  (match eval e with
   | Some n -> Printf.printf "Result: %d\n" n
   | None   -> Printf.printf "Result: <cannot evaluate>\n");
  Printf.printf "\n"

let () =
  Printf.printf "=== Exercise 5: Calculator Parser ===\n\n";

  test_expr "42";
  test_expr "1 + 2";
  test_expr "3 * 4 + 5";
  test_expr "(3 + 4) * 5";
  test_expr "10 - 3 - 2";
  test_expr "-7";
  test_expr "x + 1";

  Printf.printf "Done!\n"
