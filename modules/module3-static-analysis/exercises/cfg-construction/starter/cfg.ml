(** Control Flow Graph implementation. *)

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type basic_block = {
  label : string;
  stmts : Shared_ast.Ast_types.stmt list;
  mutable succs : string list;
  mutable preds : string list;
}

type cfg = {
  entry : string;
  exit_label : string;
  blocks : basic_block StringMap.t;
}

let create_block (label : string) (stmts : Shared_ast.Ast_types.stmt list) : basic_block =
  { label; stmts; succs = []; preds = [] }

let add_edge (cfg : cfg) (src : string) (dst : string) : cfg =
  let src_block = StringMap.find src cfg.blocks in
  let dst_block = StringMap.find dst cfg.blocks in
  src_block.succs <- src_block.succs @ [dst];
  dst_block.preds <- dst_block.preds @ [src];
  cfg

let predecessors (cfg : cfg) (label : string) : string list =
  (StringMap.find label cfg.blocks).preds

let successors (cfg : cfg) (label : string) : string list =
  (StringMap.find label cfg.blocks).succs

let to_string (cfg : cfg) : string =
  StringMap.fold (fun _key block acc ->
    let succs_str = "[" ^ String.concat "; " block.succs ^ "]" in
    let preds_str = "[" ^ String.concat "; " block.preds ^ "]" in
    acc ^
    Printf.sprintf "Block: %s (%d stmts)\n  succs: %s\n  preds: %s\n\n"
      block.label (List.length block.stmts) succs_str preds_str
  ) cfg.blocks ""
