
open Format
open X86_64
open Ast

let debug = ref false

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  { text = globl "main" ++ label "main" ++ ret;   (* TODO *)
    data = nop; }                                 (* TODO *)
