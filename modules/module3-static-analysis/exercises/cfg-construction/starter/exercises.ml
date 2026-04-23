(** CFG Construction Exercises. *)

open Shared_ast.Ast_types

let build_cfg_sequential (stmts : stmt list) : Cfg.cfg =
  let entry  = Cfg.create_block "ENTRY" [] in
  let b1     = Cfg.create_block "B1" stmts in
  let exit_b = Cfg.create_block "EXIT" [] in
  let blocks =
    Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY" entry
    |> Cfg.StringMap.add "B1"    b1
    |> Cfg.StringMap.add "EXIT"  exit_b
  in
  let cfg = { Cfg.entry = "ENTRY"; exit_label = "EXIT"; blocks } in
  let cfg = Cfg.add_edge cfg "ENTRY" "B1" in
  let cfg = Cfg.add_edge cfg "B1"    "EXIT" in
  cfg

(* Split stmts into (pre, s, post) around the first matching statement. *)
let split_on pred stmts =
  let rec go pre = function
    | [] -> failwith "split_on: not found"
    | s :: rest ->
      if pred s then (List.rev pre, s, rest)
      else go (s :: pre) rest
  in
  go [] stmts

let build_cfg_ifelse (stmts : stmt list) : Cfg.cfg =
  let (pre, if_stmt, post) =
    split_on (function If _ -> true | _ -> false) stmts
  in
  let (then_stmts, else_stmts) =
    match if_stmt with
    | If (_, t, e) -> (t, e)
    | _ -> assert false
  in
  let entry  = Cfg.create_block "ENTRY"  [] in
  let b_cond = Cfg.create_block "B_cond" pre in
  let b_then = Cfg.create_block "B_then" then_stmts in
  let b_else = Cfg.create_block "B_else" else_stmts in
  let b_join = Cfg.create_block "B_join" post in
  let exit_b = Cfg.create_block "EXIT"   [] in
  let blocks =
    Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY"  entry
    |> Cfg.StringMap.add "B_cond" b_cond
    |> Cfg.StringMap.add "B_then" b_then
    |> Cfg.StringMap.add "B_else" b_else
    |> Cfg.StringMap.add "B_join" b_join
    |> Cfg.StringMap.add "EXIT"   exit_b
  in
  let cfg = { Cfg.entry = "ENTRY"; exit_label = "EXIT"; blocks } in
  let cfg = Cfg.add_edge cfg "ENTRY"  "B_cond" in
  let cfg = Cfg.add_edge cfg "B_cond" "B_then" in
  let cfg = Cfg.add_edge cfg "B_cond" "B_else" in
  let cfg = Cfg.add_edge cfg "B_then" "B_join" in
  let cfg = Cfg.add_edge cfg "B_else" "B_join" in
  let cfg = Cfg.add_edge cfg "B_join" "EXIT"   in
  cfg

let build_cfg_while (stmts : stmt list) : Cfg.cfg =
  let (pre, while_stmt, post) =
    split_on (function While _ -> true | _ -> false) stmts
  in
  let body =
    match while_stmt with
    | While (_, b) -> b
    | _ -> assert false
  in
  let entry  = Cfg.create_block "ENTRY"  [] in
  let b_pre  = Cfg.create_block "B_pre"  pre in
  let b_cond = Cfg.create_block "B_cond" [] in
  let b_body = Cfg.create_block "B_body" body in
  let b_post = Cfg.create_block "B_post" post in
  let exit_b = Cfg.create_block "EXIT"   [] in
  let blocks =
    Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY"  entry
    |> Cfg.StringMap.add "B_pre"  b_pre
    |> Cfg.StringMap.add "B_cond" b_cond
    |> Cfg.StringMap.add "B_body" b_body
    |> Cfg.StringMap.add "B_post" b_post
    |> Cfg.StringMap.add "EXIT"   exit_b
  in
  let cfg = { Cfg.entry = "ENTRY"; exit_label = "EXIT"; blocks } in
  let cfg = Cfg.add_edge cfg "ENTRY"  "B_pre"  in
  let cfg = Cfg.add_edge cfg "B_pre"  "B_cond" in
  let cfg = Cfg.add_edge cfg "B_cond" "B_body" in
  let cfg = Cfg.add_edge cfg "B_cond" "B_post" in
  let cfg = Cfg.add_edge cfg "B_body" "B_cond" in
  let cfg = Cfg.add_edge cfg "B_post" "EXIT"   in
  cfg
