open Ast

let debug = ref false 
(* true => not report error, false =>  *)

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id
*)
let error ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

let file ?debug:(b=false) (p: Ast.file) : Ast.tfile =
  debug := b;
  if !debug then
    []  (* 當 debug 模式開啟時，返回空列表 *)
  else
    let rec type_check_stmt stmt =
      match stmt with
      | Sprint expr ->
          let texpr = type_check_expr expr in
          TSprint texpr (* 將 Sprint 節點轉換為 TSprint *)
      | _ -> failwith "Unsupported statement"
    
    and type_check_expr expr =
      match expr with
      | Ecst c -> TEcst c
      | _ -> failwith "Unsupported expression"
    
    in (* 這裡補充 "in" 來結束 let 定義 *)

    []  (* 返回空列表，或者你可以根據具體邏輯進行修改 *)
