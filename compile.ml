open Format
open X86_64
open Ast

let debug = ref false

(* New label generation *)
let label_counter = ref 0

let new_label () =
  let label = Printf.sprintf "L%d" !label_counter in
  incr label_counter;
  label

let compile_constant = function
  | Cnone -> 0L
  | Cbool true -> 1L
  | Cbool false -> 0L
  | Cstring _ -> failwith "Strings are not supported in code generation"
  | Cint i -> i

let rec compile_expr = function
  | TEcst c -> movq (imm64 (compile_constant c)) !%rax
  | TEvar v -> movq (ind ~ofs:v.v_ofs rbp) !%rax
  | TEbinop (op, lhs, rhs) ->
      compile_expr lhs ++
      pushq !%rax ++
      compile_expr rhs ++
      popq rbx ++
      (match op with
       | Badd -> addq !%rbx !%rax
       | Bsub -> subq !%rax !%rbx ++ movq !%rbx !%rax
       | Bmul -> imulq !%rbx !%rax
       | Bdiv -> cqto ++ idivq !%rbx
       | Bmod -> cqto ++ idivq !%rbx ++ movq !%rdx !%rax
       | Beq -> cmpq !%rbx !%rax ++ sete !%al ++ movzbq !%al rax
       | Bneq -> cmpq !%rbx !%rax ++ setne !%al ++ movzbq !%al rax
       | Blt -> cmpq !%rbx !%rax ++ setl !%al ++ movzbq !%al rax
       | Ble -> cmpq !%rbx !%rax ++ setle !%al ++ movzbq !%al rax
       | Bgt -> cmpq !%rbx !%rax ++ setg !%al ++ movzbq !%al rax
       | Bge -> cmpq !%rbx !%rax ++ setge !%al ++ movzbq !%al rax
       | Band -> andq !%rbx !%rax
       | Bor -> orq !%rbx !%rax)
  | TEunop (Uneg, e) -> compile_expr e ++ negq !%rax
  | TEunop (Unot, e) -> compile_expr e ++ notq !%rax
  | TEcall (fn, args) ->
      List.fold_right (fun arg code -> compile_expr arg ++ pushq !%rax ++ code) args nop ++
      call fn.fn_name ++
      addq (imm (8 * List.length args)) !%rsp
  | TElist _ -> failwith "Lists are not supported in code generation"
  | TErange _ -> failwith "Range is not supported in code generation"
  | TEget _ -> failwith "Get is not supported in code generation"

let rec compile_stmt = function
  | TSif (cond, then_branch, else_branch) ->
      let else_label = new_label () in
      let end_label = new_label () in
      compile_expr cond ++
      testq !%rax !%rax ++
      jz else_label ++
      compile_stmt then_branch ++
      jmp end_label ++
      label else_label ++
      compile_stmt else_branch ++
      label end_label
  | TSreturn e -> compile_expr e ++ ret
  | TSassign (v, e) -> compile_expr e ++ movq !%rax (ind ~ofs:v.v_ofs rbp)
  | TSprint e -> 
    compile_expr e ++
    movq !%rax !%rsi ++
    leaq (lab "fmt") rdi ++
    movq (imm 0) !%rax ++
    call "printf"
  | TSblock stmts -> List.fold_left (++) nop (List.map compile_stmt stmts)
  | TSfor _ -> failwith "For loops are not supported in code generation"
  | TSeval e -> compile_expr e
  | TSset _ -> failwith "Set is not supported in code generation"

let compile_def (fn, body) =
  label fn.fn_name ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  compile_stmt body ++
  movl (imm 0) !%eax ++
  popq rbp ++
  ret

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  { text = 
      globl "main" ++ 
      label "main" ++ 
      List.fold_left (++) nop (List.map compile_def p);
    data = 
      label "fmt" ++
      string "%d\n" }