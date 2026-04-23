(* traversals.ml - AST traversal algorithms exercise. *)

open Shared_ast.Ast_types

let string_of_op = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
  | Eq  -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">"
  | Le  -> "<=" | Ge  -> ">=" | And -> "&&" | Or -> "||"

let string_of_uop = function
  | Neg -> "-" | Not -> "!"

let label_of_expr (e : expr) : string =
  match e with
  | IntLit n    -> Printf.sprintf "IntLit(%d)" n
  | BoolLit b   -> Printf.sprintf "BoolLit(%b)" b
  | Var s       -> Printf.sprintf "Var(%s)" s
  | BinOp (op, _, _)  -> Printf.sprintf "BinOp(%s)" (string_of_op op)
  | UnaryOp (op, _)   -> Printf.sprintf "UnaryOp(%s)" (string_of_uop op)
  | Call (name, _)    -> Printf.sprintf "Call(%s)" name

let label_of_stmt (s : stmt) : string =
  match s with
  | Assign _ -> "Assign"
  | If _     -> "If"
  | While _  -> "While"
  | Return _ -> "Return"
  | Print _  -> "Print"
  | Block _  -> "Block"

let pre_order (stmts : stmt list) : string list =
  let rec go_expr e =
    label_of_expr e ::
    (match e with
     | IntLit _ | BoolLit _ | Var _ -> []
     | BinOp (_, e1, e2)  -> go_expr e1 @ go_expr e2
     | UnaryOp (_, e1)    -> go_expr e1
     | Call (_, args)     -> List.concat_map go_expr args)
  and go_stmt s =
    label_of_stmt s ::
    (match s with
     | Assign (_, e)          -> go_expr e
     | If (cond, then_b, else_b) ->
       go_expr cond @ go_stmts then_b @ go_stmts else_b
     | While (cond, body)     -> go_expr cond @ go_stmts body
     | Return None            -> []
     | Return (Some e)        -> go_expr e
     | Print exprs            -> List.concat_map go_expr exprs
     | Block ss               -> go_stmts ss)
  and go_stmts ss = List.concat_map go_stmt ss
  in
  go_stmts stmts

let post_order (stmts : stmt list) : string list =
  let rec go_expr e =
    (match e with
     | IntLit _ | BoolLit _ | Var _ -> []
     | BinOp (_, e1, e2)  -> go_expr e1 @ go_expr e2
     | UnaryOp (_, e1)    -> go_expr e1
     | Call (_, args)     -> List.concat_map go_expr args)
    @ [label_of_expr e]
  and go_stmt s =
    (match s with
     | Assign (_, e)          -> go_expr e
     | If (cond, then_b, else_b) ->
       go_expr cond @ go_stmts then_b @ go_stmts else_b
     | While (cond, body)     -> go_expr cond @ go_stmts body
     | Return None            -> []
     | Return (Some e)        -> go_expr e
     | Print exprs            -> List.concat_map go_expr exprs
     | Block ss               -> go_stmts ss)
    @ [label_of_stmt s]
  and go_stmts ss = List.concat_map go_stmt ss
  in
  go_stmts stmts

type node = Stmt of stmt | Expr of expr

let children = function
  | Stmt (Assign (_, e))          -> [Expr e]
  | Stmt (If (cond, then_b, else_b)) ->
    Expr cond :: List.map (fun s -> Stmt s) then_b
    @ List.map (fun s -> Stmt s) else_b
  | Stmt (While (cond, body))     ->
    Expr cond :: List.map (fun s -> Stmt s) body
  | Stmt (Return None)            -> []
  | Stmt (Return (Some e))        -> [Expr e]
  | Stmt (Print exprs)            -> List.map (fun e -> Expr e) exprs
  | Stmt (Block ss)               -> List.map (fun s -> Stmt s) ss
  | Expr (IntLit _)
  | Expr (BoolLit _)
  | Expr (Var _)                  -> []
  | Expr (BinOp (_, e1, e2))      -> [Expr e1; Expr e2]
  | Expr (UnaryOp (_, e1))        -> [Expr e1]
  | Expr (Call (_, args))         -> List.map (fun e -> Expr e) args

let label_of_node = function
  | Stmt s -> label_of_stmt s
  | Expr e -> label_of_expr e

let bfs (stmts : stmt list) : string list =
  let q = Queue.create () in
  List.iter (fun s -> Queue.add (Stmt s) q) stmts;
  let result = ref [] in
  while not (Queue.is_empty q) do
    let node = Queue.pop q in
    result := label_of_node node :: !result;
    List.iter (fun c -> Queue.add c q) (children node)
  done;
  List.rev !result
