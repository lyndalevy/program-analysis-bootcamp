(* transformations.ml - AST transformation passes.

   Each transformation is a pure function: it takes an AST (or part of one)
   and returns a *new* AST with the transformation applied.  The original
   tree is never mutated.

   Implement the three transformations below.  Each one exercises a
   different kind of recursive tree rewriting. *)

open Shared_ast.Ast_types

(* --------------------------------------------------------------------------
   1. Constant folding
   --------------------------------------------------------------------------
   Simplify expressions whose operands are known at compile time.

   Strategy:
     - Recursively fold sub-expressions first (bottom-up).
     - After folding, check whether a BinOp has two IntLit children.
       If so, evaluate the operation and return a single IntLit (or BoolLit
       for comparison / logical operators).
     - Leave everything else unchanged.

   Examples:
     BinOp(Add, IntLit 2, IntLit 3)           --> IntLit 5
     BinOp(Mul, IntLit 2, BinOp(Add, IntLit 1, IntLit 3))
                                                --> IntLit 8
     BinOp(Add, Var "x", IntLit 1)            --> BinOp(Add, Var "x", IntLit 1)
*)
let rec constant_fold (expr : expr) : expr =
  let folded = match expr with
    | IntLit _ | BoolLit _ | Var _ -> expr
    | BinOp (op, e1, e2) ->
      let e1' = constant_fold e1 in
      let e2' = constant_fold e2 in
      (match op, e1', e2' with
       (* Arithmetic operators *)
       | Add, IntLit n1, IntLit n2 -> IntLit (n1 + n2)
       | Sub, IntLit n1, IntLit n2 -> IntLit (n1 - n2)
       | Mul, IntLit n1, IntLit n2 -> IntLit (n1 * n2)
       | Div, IntLit n1, IntLit n2 when n2 <> 0 -> IntLit (n1 / n2)
       (* Comparison operators *)
       | Eq, IntLit n1, IntLit n2 -> BoolLit (n1 = n2)
       | Neq, IntLit n1, IntLit n2 -> BoolLit (n1 <> n2)
       | Lt, IntLit n1, IntLit n2 -> BoolLit (n1 < n2)
       | Gt, IntLit n1, IntLit n2 -> BoolLit (n1 > n2)
       | Le, IntLit n1, IntLit n2 -> BoolLit (n1 <= n2)
       | Ge, IntLit n1, IntLit n2 -> BoolLit (n1 >= n2)
       (* Logical operators on booleans *)
       | And, BoolLit b1, BoolLit b2 -> BoolLit (b1 && b2)
       | Or, BoolLit b1, BoolLit b2 -> BoolLit (b1 || b2)
       (* Comparison on booleans *)
       | Eq, BoolLit b1, BoolLit b2 -> BoolLit (b1 = b2)
       | Neq, BoolLit b1, BoolLit b2 -> BoolLit (b1 <> b2)
       (* Otherwise, keep the BinOp with folded children *)
       | _ -> BinOp (op, e1', e2'))
    | UnaryOp (uop, e) ->
      let e' = constant_fold e in
      (match uop, e' with
       | Neg, IntLit n -> IntLit (-n)
       | Not, BoolLit b -> BoolLit (not b)
       | _ -> UnaryOp (uop, e'))
    | Call (name, args) ->
      Call (name, List.map constant_fold args)
  in
  folded

(* --------------------------------------------------------------------------
   2. Variable renaming
   --------------------------------------------------------------------------
   Replace every occurrence of a variable named [old_name] with [new_name]
   throughout a list of statements.  This includes:
     - Var references inside expressions
     - The left-hand side of Assign statements
   Other identifiers (function names in Call, etc.) are left alone.

   You will need a helper to rename inside expressions as well.

   Example:
     rename_variable "x" "tmp"
       [Assign("x", IntLit 1); Print [Var "x"]]
     -->
       [Assign("tmp", IntLit 1); Print [Var "tmp"]]
*)
let rename_variable (old_name : string) (new_name : string)
    (stmts : stmt list) : stmt list =
  let rec rename_expr e =
    match e with
    | IntLit _ | BoolLit _ -> e
    | Var v -> if v = old_name then Var new_name else e
    | BinOp (op, e1, e2) -> BinOp (op, rename_expr e1, rename_expr e2)
    | UnaryOp (uop, e1) -> UnaryOp (uop, rename_expr e1)
    | Call (name, args) -> Call (name, List.map rename_expr args)
  in
  let rec rename_stmt s =
    match s with
    | Assign (v, e) ->
      let v' = if v = old_name then new_name else v in
      Assign (v', rename_expr e)
    | If (cond, then_b, else_b) ->
      If (rename_expr cond, rename_stmts then_b, rename_stmts else_b)
    | While (cond, body) ->
      While (rename_expr cond, rename_stmts body)
    | Return None -> s
    | Return (Some e) -> Return (Some (rename_expr e))
    | Print exprs -> Print (List.map rename_expr exprs)
    | Block stmts -> Block (rename_stmts stmts)
  and rename_stmts stmts =
    List.map rename_stmt stmts
  in
  rename_stmts stmts

(* --------------------------------------------------------------------------
   3. Dead-code elimination
   --------------------------------------------------------------------------
   Remove statements that can never execute.  Two cases to handle:

   a) Unreachable code after Return:
      In a statement list, once a Return is encountered, all subsequent
      statements in that same list are dead and should be removed.

   b) Trivially-decided If:
      - If(BoolLit true,  then_branch, _)  --> replace with then_branch
      - If(BoolLit false, _, else_branch)   --> replace with else_branch

   Apply these rules recursively into nested blocks (If branches, While
   bodies, Block contents).

   Example:
     [Return (Some (IntLit 42)); Print [Var "unreachable"]]
     -->
     [Return (Some (IntLit 42))]
*)
let rec eliminate_dead_code (stmts : stmt list) : stmt list =
  let elim_stmt s =
    match s with
    | Assign _ | Print _ | Return _ -> s
    | If (cond, then_b, else_b) ->
      (match cond with
       | BoolLit true -> Block (eliminate_dead_code then_b)
       | BoolLit false -> Block (eliminate_dead_code else_b)
       | _ -> If (cond, eliminate_dead_code then_b, eliminate_dead_code else_b))
    | While (cond, body) -> While (cond, eliminate_dead_code body)
    | Block stmts -> Block (eliminate_dead_code stmts)
  in
  let rec process_list acc = function
    | [] -> List.rev acc
    | Return _ as ret :: _ -> List.rev (ret :: acc)  (* Stop at Return *)
    | s :: rest -> process_list (elim_stmt s :: acc) rest
  in
  process_list [] stmts
