(* ast_visualizer.ml - AST Structure Mapping Exercise
   ===================================================
   Implement three ways to visualize an AST built from Shared_ast.Ast_types:

   1. dump_ast       - Indented tree representation showing node types
   2. count_node_types - Count occurrences of each AST node type
   3. print_tree     - Pretty-print as human-readable pseudo-code

   Each function operates on a stmt list (i.e., a function body).
   Refer to Ast_types for the type definitions:
     expr: IntLit, BoolLit, Var, BinOp, UnaryOp, Call
     stmt: Assign, If, While, Return, Print, Block
*)

open Shared_ast.Ast_types

(* ------------------------------------------------------------------ *)
(* Helper: create an indentation string of [n] levels (2 spaces each) *)
(* ------------------------------------------------------------------ *)
let indent n = String.make (n * 2) ' '

(* ------------------------------------------------------------------ *)
(* Operator helpers (used by both dump_ast and print_tree)            *)
(* ------------------------------------------------------------------ *)
let string_of_op = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
  | Eq  -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">"
  | Le  -> "<=" | Ge  -> ">=" | And -> "&&" | Or -> "||"

let string_of_uop = function
  | Neg -> "-" | Not -> "!"

(* ================================================================== *)
(* 1. dump_ast : stmt list -> string                                  *)
(*                                                                    *)
(*    Produce an indented, tree-style dump of the AST.                *)
(*    Example output for  Assign("x", IntLit 5):                      *)
(*      Assign("x")                                                   *)
(*        IntLit(5)                                                   *)
(*                                                                    *)
(*    For BinOp(Add, IntLit 2, IntLit 3):                             *)
(*      BinOp(+)                                                      *)
(*        IntLit(2)                                                   *)
(*        IntLit(3)                                                   *)
(* ================================================================== *)

let rec dump_expr (depth : int) (e : expr) : string =
  let pad = indent depth in
  match e with
  | IntLit n ->
    Printf.sprintf "%sIntLit(%d)" pad n
  | BoolLit b ->
    Printf.sprintf "%sBoolLit(%b)" pad b
  | Var s ->
    Printf.sprintf "%sVar(\"%s\")" pad s
  | BinOp (op, e1, e2) ->
    Printf.sprintf "%sBinOp(%s)\n%s\n%s"
      pad (string_of_op op)
      (dump_expr (depth + 1) e1)
      (dump_expr (depth + 1) e2)
  | UnaryOp (op, e1) ->
    Printf.sprintf "%sUnaryOp(%s)\n%s"
      pad (string_of_uop op)
      (dump_expr (depth + 1) e1)
  | Call (name, args) ->
    let args_str = String.concat "\n" (List.map (dump_expr (depth + 1)) args) in
    if args = [] then Printf.sprintf "%sCall(\"%s\")" pad name
    else Printf.sprintf "%sCall(\"%s\")\n%s" pad name args_str

and dump_stmt (depth : int) (s : stmt) : string =
  let pad = indent depth in
  match s with
  | Assign (v, e) ->
    Printf.sprintf "%sAssign(\"%s\")\n%s" pad v (dump_expr (depth + 1) e)
  | If (cond, then_b, else_b) ->
    Printf.sprintf "%sIf\n%s\n%sThen\n%s\n%sElse\n%s"
      pad (dump_expr (depth + 1) cond)
      pad (dump_stmts (depth + 1) then_b)
      pad (dump_stmts (depth + 1) else_b)
  | While (cond, body) ->
    Printf.sprintf "%sWhile\n%s\n%sBody\n%s"
      pad (dump_expr (depth + 1) cond)
      pad (dump_stmts (depth + 1) body)
  | Return None ->
    Printf.sprintf "%sReturn()" pad
  | Return (Some e) ->
    Printf.sprintf "%sReturn\n%s" pad (dump_expr (depth + 1) e)
  | Print exprs ->
    let args_str = String.concat "\n" (List.map (dump_expr (depth + 1)) exprs) in
    if exprs = [] then Printf.sprintf "%sPrint" pad
    else Printf.sprintf "%sPrint\n%s" pad args_str
  | Block stmts ->
    Printf.sprintf "%sBlock\n%s" pad (dump_stmts (depth + 1) stmts)

and dump_stmts (depth : int) (stmts : stmt list) : string =
  String.concat "\n" (List.map (dump_stmt depth) stmts)

let dump_ast (stmts : stmt list) : string =
  dump_stmts 0 stmts


(* ================================================================== *)
(* 2. count_node_types : stmt list -> (string * int) list             *)
(*                                                                    *)
(*    Walk the AST and count how many of each node type appear.       *)
(*    Return an association list like:                                 *)
(*      [("Assign", 2); ("BinOp", 3); ("IntLit", 4); ...]            *)
(*                                                                    *)
(*    Node type names to count:                                       *)
(*      Statements: "Assign", "If", "While", "Return", "Print",      *)
(*                  "Block"                                           *)
(*      Expressions: "IntLit", "BoolLit", "Var", "BinOp",            *)
(*                   "UnaryOp", "Call"                                *)
(* ================================================================== *)

let inc (key : string) (counts : (string * int) list) : (string * int) list =
  if List.mem_assoc key counts then
    List.map (fun (k, v) -> if k = key then (k, v + 1) else (k, v)) counts
  else
    counts @ [(key, 1)]

let rec count_expr (acc : (string * int) list) (e : expr) : (string * int) list =
  match e with
  | IntLit _ ->
    inc "IntLit" acc
  | BoolLit _ ->
    inc "BoolLit" acc
  | Var _ ->
    inc "Var" acc
  | BinOp (_, e1, e2) ->
    let acc = inc "BinOp" acc in
    let acc = count_expr acc e1 in
    count_expr acc e2
  | UnaryOp (_, e1) ->
    let acc = inc "UnaryOp" acc in
    count_expr acc e1
  | Call (_, args) ->
    let acc = inc "Call" acc in
    List.fold_left count_expr acc args

and count_stmt (acc : (string * int) list) (s : stmt) : (string * int) list =
  match s with
  | Assign (_, e) ->
    let acc = inc "Assign" acc in
    count_expr acc e
  | If (cond, then_b, else_b) ->
    let acc = inc "If" acc in
    let acc = count_expr acc cond in
    let acc = count_stmts acc then_b in
    count_stmts acc else_b
  | While (cond, body) ->
    let acc = inc "While" acc in
    let acc = count_expr acc cond in
    count_stmts acc body
  | Return None ->
    inc "Return" acc
  | Return (Some e) ->
    let acc = inc "Return" acc in
    count_expr acc e
  | Print exprs ->
    let acc = inc "Print" acc in
    List.fold_left count_expr acc exprs
  | Block stmts ->
    let acc = inc "Block" acc in
    count_stmts acc stmts

and count_stmts (acc : (string * int) list) (stmts : stmt list) : (string * int) list =
  List.fold_left count_stmt acc stmts

let count_node_types (stmts : stmt list) : (string * int) list =
  count_stmts [] stmts


(* ================================================================== *)
(* 3. print_tree : stmt list -> string                                *)
(*                                                                    *)
(*    Pretty-print the AST as human-readable pseudo-code.             *)
(*    Example:                                                        *)
(*      x = (2 + 3);                                                  *)
(*      if ((x > 0)) {                                                *)
(*        y = 1;                                                      *)
(*      } else {                                                      *)
(*        y = 0;                                                      *)
(*      }                                                             *)
(* ================================================================== *)

let rec expr_to_string (e : expr) : string =
  match e with
  | BinOp (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (expr_to_string e1) (string_of_op op) (expr_to_string e2)
  | UnaryOp (op, e1) ->
    Printf.sprintf "(%s%s)" (string_of_uop op) (expr_to_string e1)
  | Call (name, args) ->
    Printf.sprintf "%s(%s)" name (String.concat ", " (List.map expr_to_string args))
  | IntLit n  -> string_of_int n
  | BoolLit b -> string_of_bool b
  | Var s     -> s

and pp_stmt (depth : int) (s : stmt) : string =
  let pad = indent depth in
  match s with
  | Assign (v, e) ->
    Printf.sprintf "%s%s = %s;" pad v (expr_to_string e)
  | If (cond, then_b, else_b) ->
    let then_str = pp_stmts (depth + 1) then_b in
    let else_part =
      if else_b = [] then ""
      else Printf.sprintf "\n%s} else {\n%s" pad (pp_stmts (depth + 1) else_b)
    in
    Printf.sprintf "%sif (%s) {\n%s%s\n%s}"
      pad (expr_to_string cond) then_str else_part pad
  | While (cond, body) ->
    Printf.sprintf "%swhile (%s) {\n%s\n%s}"
      pad (expr_to_string cond) (pp_stmts (depth + 1) body) pad
  | Return None ->
    Printf.sprintf "%sreturn;" pad
  | Return (Some e) ->
    Printf.sprintf "%sreturn %s;" pad (expr_to_string e)
  | Print exprs ->
    Printf.sprintf "%sprint(%s);" pad (String.concat ", " (List.map expr_to_string exprs))
  | Block stmts ->
    Printf.sprintf "%s{\n%s\n%s}" pad (pp_stmts (depth + 1) stmts) pad

and pp_stmts (depth : int) (stmts : stmt list) : string =
  String.concat "\n" (List.map (pp_stmt depth) stmts)

let print_tree (stmts : stmt list) : string =
  pp_stmts 0 stmts


(* ================================================================== *)
(* Main - run the visualizer on the sample programs                   *)
(* ================================================================== *)
let () =
  let open Sample_asts in
  let programs = [
    ("simple_arithmetic", simple_arithmetic);
    ("branching",         branching);
    ("loop_example",      loop_example);
  ] in
  List.iter (fun (label, prog) ->
    let body = (List.hd prog).body in
    Printf.printf "=== %s ===\n\n" label;

    Printf.printf "--- dump_ast ---\n%s\n\n" (dump_ast body);

    Printf.printf "--- count_node_types ---\n";
    let counts = count_node_types body in
    List.iter (fun (name, n) ->
      Printf.printf "  %s: %d\n" name n
    ) counts;
    print_newline ();

    Printf.printf "--- print_tree ---\n%s\n\n" (print_tree body);
  ) programs
