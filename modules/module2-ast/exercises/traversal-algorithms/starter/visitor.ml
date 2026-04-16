(* visitor.ml - AST visitor pattern exercises.
   Implement two common visitor-style operations that walk the AST
   and accumulate information. *)

open Shared_ast.Ast_types

(** Count the number of each node type in a statement list.
    Returns an association list like:
      [("Assign", 3); ("IntLit", 5); ("BinOp", 2); ...]
    The keys are constructor names WITHOUT parameters (e.g., "IntLit"
    not "IntLit(3)"). Order does not matter.

    Hint:
      - Write recursive helpers for expr and stmt.
      - Use a mutable Hashtbl or a ref to a Map to accumulate counts,
        or thread an accumulator through the recursion.
      - Don't forget to count the node itself AND recurse into its
        children. *)
let count_nodes (stmts : stmt list) : (string * int) list =
  let inc key counts =
    let rec loop acc = function
      | [] -> List.rev_append acc [(key, 1)]
      | (k, v) :: rest when k = key -> List.rev_append acc ((k, v + 1) :: rest)
      | x :: rest -> loop (x :: acc) rest
    in
    loop [] counts
  in

  let rec count_expr acc e =
    let name = match e with
      | IntLit _ -> "IntLit"
      | BoolLit _ -> "BoolLit"
      | Var _ -> "Var"
      | BinOp _ -> "BinOp"
      | UnaryOp _ -> "UnaryOp"
      | Call _ -> "Call"
    in
    let acc' = inc name acc in
    match e with
    | IntLit _ | BoolLit _ | Var _ -> acc'
    | BinOp (_, e1, e2) -> count_expr (count_expr acc' e1) e2
    | UnaryOp (_, e1) -> count_expr acc' e1
    | Call (_, args) -> List.fold_left count_expr acc' args

  and count_stmt acc s =
    let name = match s with
      | Assign _ -> "Assign"
      | If _ -> "If"
      | While _ -> "While"
      | Return _ -> "Return"
      | Print _ -> "Print"
      | Block _ -> "Block"
    in
    let acc' = inc name acc in
    match s with
    | Assign (_, e) -> count_expr acc' e
    | If (cond, then_b, else_b) ->
      let acc'' = count_expr acc' cond in
      let acc''' = count_stmts acc'' then_b in
      count_stmts acc''' else_b
    | While (cond, body) ->
      let acc'' = count_expr acc' cond in
      count_stmts acc'' body
    | Return None -> acc'
    | Return (Some e) -> count_expr acc' e
    | Print exprs -> List.fold_left count_expr acc' exprs
    | Block stmts -> count_stmts acc' stmts

  and count_stmts acc stmts =
    List.fold_left count_stmt acc stmts
  in

  count_stmts [] stmts

(** Evaluate a constant expression, returning Some int if the
    expression contains only integer literals and arithmetic operators,
    or None if it contains variables, booleans, calls, or comparison
    operators.

    Supported operators: Add, Sub, Mul, Div (integer division).
    Division by zero should return None.

    Examples:
      evaluate (IntLit 42)                        => Some 42
      evaluate (BinOp (Add, IntLit 1, IntLit 2))  => Some 3
      evaluate (BinOp (Add, IntLit 1, Var "x"))   => None
      evaluate (BoolLit true)                      => None

    Hint: use Option.bind or match on recursive results. *)
let rec evaluate (e : expr) : int option =
  match e with
  | IntLit n -> Some n
  | BoolLit _ | Var _ | Call _ -> None
  | UnaryOp (uop, e1) ->
    (match uop with
     | Neg ->
       (match evaluate e1 with
        | Some n -> Some (-n)
        | None -> None)
     | Not -> None)
  | BinOp (op, e1, e2) ->
    match op with
    | Add | Sub | Mul | Div ->
      (match evaluate e1, evaluate e2 with
       | Some v1, Some v2 ->
         (match op with
          | Add -> Some (v1 + v2)
          | Sub -> Some (v1 - v2)
          | Mul -> Some (v1 * v2)
          | Div -> if v2 = 0 then None else Some (v1 / v2)
          | _ -> None)
       | _ -> None)
    | Eq | Neq | Lt | Gt | Le | Ge | And | Or -> None
