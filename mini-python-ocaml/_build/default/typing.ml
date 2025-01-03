
open Ast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id
*)
let error ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")


let file ?debug:(b=false) (p: Ast.file) : Ast.tfile =
  debug := b;  (* 设置全局变量 debug *)
  if !debug then
    []  (* 如果 debug 为 true，则返回空列表 *)
  else
    failwith "Debug is false"  (* 如果 debug 为 false，则抛出异常 *)
