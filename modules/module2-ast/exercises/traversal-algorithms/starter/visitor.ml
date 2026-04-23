(* visitor.ml - AST visitor pattern exercises. *)

open Shared_ast.Ast_types

let count_nodes (stmts : stmt list) : (string * int) list =
  let inc key counts =
    if List.mem_assoc key counts then
      List.map (fun (k, v) -> if k = key then (k, v + 1) else (k, v)) counts
    else
      counts @ [(key, 1)]
  in
  let rec count_expr acc e =
    match e with
    | IntLit _  -> inc "IntLit" acc
    | BoolLit _ -> inc "BoolLit" acc
    | Var _     -> inc "Var" acc
    | BinOp (_, e1, e2)  ->
      let acc = inc "BinOp" acc in
      count_expr (count_expr acc e1) e2
    | UnaryOp (_, e1) ->
      let acc = inc "UnaryOp" acc in
      count_expr acc e1
    | Call (_, args) ->
      let acc = inc "Call" acc in
      List.fold_left count_expr acc args
  and count_stmt acc s =
    match s with
    | Assign (_, e)  ->
      let acc = inc "Assign" acc in
      count_expr acc e
    | If (cond, then_b, else_b) ->
      let acc = inc "If" acc in
      let acc = count_expr acc cond in
      let acc = List.fold_left count_stmt acc then_b in
      List.fold_left count_stmt acc else_b
    | While (cond, body) ->
      let acc = inc "While" acc in
      let acc = count_expr acc cond in
      List.fold_left count_stmt acc body
    | Return None    -> inc "Return" acc
    | Return (Some e) ->
      let acc = inc "Return" acc in
      count_expr acc e
    | Print exprs ->
      let acc = inc "Print" acc in
      List.fold_left count_expr acc exprs
    | Block ss ->
      let acc = inc "Block" acc in
      List.fold_left count_stmt acc ss
  in
  List.fold_left count_stmt [] stmts

let evaluate (e : expr) : int option =
  let rec go = function
    | IntLit n -> Some n
    | BinOp (op, e1, e2) -> (
        match op with
        | Add | Sub | Mul | Div -> (
            match go e1, go e2 with
            | Some v1, Some v2 -> (
                match op with
                | Add -> Some (v1 + v2)
                | Sub -> Some (v1 - v2)
                | Mul -> Some (v1 * v2)
                | Div -> if v2 = 0 then None else Some (v1 / v2)
                | _ -> None)
            | _ -> None)
        | _ -> None)
    | UnaryOp (Neg, e1) -> Option.map (fun v -> -v) (go e1)
    | _ -> None
  in
  go e
