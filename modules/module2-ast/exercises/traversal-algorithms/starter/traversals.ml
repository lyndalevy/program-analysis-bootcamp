(* traversals.ml - AST traversal algorithms exercise.
   Implement three classic tree traversal strategies on the AST:
   pre-order (depth-first), post-order (depth-first), and
   breadth-first (level-order).

   Each function walks a list of statements and collects a string label
   for every node visited. Labels should look like:
     Statements: "Assign", "If", "While", "Return", "Print", "Block"
     Expressions: "IntLit(3)", "BoolLit(true)", "Var(x)", "BinOp(+)",
                  "UnaryOp(-)", "Call(f)"
*)

open Shared_ast.Ast_types

(** Helper: produce a string label for a single expression node.
    Examples: IntLit(3), BoolLit(true), Var(x), BinOp(+), UnaryOp(-), Call(f) *)
let label_of_expr (e : expr) : string =
  match e with
  | IntLit n -> "IntLit(" ^ string_of_int n ^ ")"
  | BoolLit b -> "BoolLit(" ^ string_of_bool b ^ ")"
  | Var v -> "Var(" ^ v ^ ")"
  | BinOp (op, _, _) ->
    let op_str = match op with
      | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">"
      | Le -> "<=" | Ge -> ">=" | And -> "&&" | Or -> "||"
    in
    "BinOp(" ^ op_str ^ ")"
  | UnaryOp (uop, _) ->
    let uop_str = match uop with Neg -> "-" | Not -> "!" in
    "UnaryOp(" ^ uop_str ^ ")"
  | Call (name, _) -> "Call(" ^ name ^ ")"

(** Helper: produce a string label for a single statement node.
    Examples: "Assign", "If", "While", "Return", "Print", "Block" *)
let label_of_stmt (s : stmt) : string =
  match s with
  | Assign _ -> "Assign"
  | If _ -> "If"
  | While _ -> "While"
  | Return _ -> "Return"
  | Print _ -> "Print"
  | Block _ -> "Block"

(** Pre-order depth-first traversal.
    Visit the current node FIRST, then recurse into its children
    left-to-right.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["Assign"; "BinOp(+)"; "IntLit(1)"; "IntLit(2)"]

    Hint: write a mutual recursion with helpers for expr and stmt lists. *)
let pre_order (stmts : stmt list) : string list =
  let rec pre_expr e =
    label_of_expr e ::
    (match e with
     | IntLit _ | BoolLit _ | Var _ -> []
     | BinOp (_, e1, e2) -> pre_expr e1 @ pre_expr e2
     | UnaryOp (_, e1) -> pre_expr e1
     | Call (_, args) -> List.concat_map pre_expr args)
  and pre_stmt s =
    label_of_stmt s ::
    (match s with
     | Assign (_, e) -> pre_expr e
     | If (cond, then_b, else_b) ->
       pre_expr cond @ pre_stmts then_b @ pre_stmts else_b
     | While (cond, body) -> pre_expr cond @ pre_stmts body
     | Return None -> []
     | Return (Some e) -> pre_expr e
     | Print exprs -> List.concat_map pre_expr exprs
     | Block stmts -> pre_stmts stmts)
  and pre_stmts stmts =
    List.concat_map pre_stmt stmts
  in
  pre_stmts stmts

(** Post-order depth-first traversal.
    Recurse into children FIRST, then visit the current node.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["IntLit(1)"; "IntLit(2)"; "BinOp(+)"; "Assign"]

    Hint: same structure as pre_order but emit the label at the end. *)
let post_order (stmts : stmt list) : string list =
  let rec post_expr e =
    (match e with
     | IntLit _ | BoolLit _ | Var _ -> []
     | BinOp (_, e1, e2) -> post_expr e1 @ post_expr e2
     | UnaryOp (_, e1) -> post_expr e1
     | Call (_, args) -> List.concat_map post_expr args)
    @ [label_of_expr e]
  and post_stmt s =
    (match s with
     | Assign (_, e) -> post_expr e
     | If (cond, then_b, else_b) ->
       post_expr cond @ post_stmts then_b @ post_stmts else_b
     | While (cond, body) -> post_expr cond @ post_stmts body
     | Return None -> []
     | Return (Some e) -> post_expr e
     | Print exprs -> List.concat_map post_expr exprs
     | Block stmts -> post_stmts stmts)
    @ [label_of_stmt s]
  and post_stmts stmts =
    List.concat_map post_stmt stmts
  in
  post_stmts stmts

(** Breadth-first (level-order) traversal.
    Visit all nodes at depth d before any node at depth d+1.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["Assign"; "BinOp(+)"; "IntLit(1)"; "IntLit(2)"]
    (In this small case it happens to match pre-order, but differs on
     deeper trees with multiple siblings.)

    Hint: use the OCaml Queue module.
      1. Seed the queue with all top-level stmts.
      2. Dequeue a node, emit its label, enqueue its children.
      3. Repeat until the queue is empty.
    You will need a sum type or two queues to handle both stmt and expr
    nodes uniformly. *)
type node = StmtNode of stmt | ExprNode of expr

let bfs (stmts : stmt list) : string list =
  let q = Queue.create () in
  List.iter (fun s -> Queue.add (StmtNode s) q) stmts;

  let rec loop acc =
    if Queue.is_empty q then
      List.rev acc
    else
      let node = Queue.take q in
      match node with
      | StmtNode s ->
        let label = label_of_stmt s in
        (match s with
         | Assign (_, e) -> Queue.add (ExprNode e) q
         | If (cond, then_b, else_b) ->
           Queue.add (ExprNode cond) q;
           List.iter (fun s -> Queue.add (StmtNode s) q) then_b;
           List.iter (fun s -> Queue.add (StmtNode s) q) else_b
         | While (cond, body) ->
           Queue.add (ExprNode cond) q;
           List.iter (fun s -> Queue.add (StmtNode s) q) body
         | Return None -> ()
         | Return (Some e) -> Queue.add (ExprNode e) q
         | Print exprs -> List.iter (fun e -> Queue.add (ExprNode e) q) exprs
         | Block stmts -> List.iter (fun s -> Queue.add (StmtNode s) q) stmts);
        loop (label :: acc)
      | ExprNode e ->
        let label = label_of_expr e in
        (match e with
         | IntLit _ | BoolLit _ | Var _ -> ()
         | BinOp (_, e1, e2) ->
           Queue.add (ExprNode e1) q;
           Queue.add (ExprNode e2) q
         | UnaryOp (_, e1) -> Queue.add (ExprNode e1) q
         | Call (_, args) -> List.iter (fun e -> Queue.add (ExprNode e) q) args);
        loop (label :: acc)
  in
  loop []
