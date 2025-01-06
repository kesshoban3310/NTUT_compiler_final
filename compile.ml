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

let string_constants = ref []

let compile_constant = function
  | Cnone -> 0L
  | Cbool true -> 1L
  | Cbool false -> 0L
  | Cstring s -> 1L
  | Cint i -> i

let rec compile_expr = function
  | TEcst c -> movq (imm64 (compile_constant c)) !%rax
  | TEvar v -> movq (ind ~ofs:v.v_ofs rbp) !%rax
  | TEbinop (op, lhs, rhs) ->
      (match lhs, rhs with
       | TEcst (Cstring s1), TEcst (Cstring s2) ->
           let lbl1 = new_label () in
           let lbl2 = new_label () in
           string_constants := (lbl1, s1) :: (lbl2, s2) :: !string_constants;
           leaq (lab lbl2) rsi ++
           leaq (lab lbl1) rdi ++
           movq (imm 0) !%rax ++
           call "strcat"
       | TEcst (Cint _), TEcst (Cstring _)
       | TEcst (Cstring _), TEcst (Cint _) ->
           failwith "Type error: cannot add integer and string"
       | _ ->
           compile_expr rhs ++
           pushq !%rax ++
           compile_expr lhs ++
           popq rbx ++
           (match op with
            | Badd -> addq !%rbx !%rax
            | Bsub -> subq !%rbx !%rax
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
            | Bor -> orq !%rbx !%rax))
  | TEunop (Uneg, e) -> compile_expr e ++ negq !%rax
  | TEunop (Unot, e) -> compile_expr e ++ notq !%rax
  | TEcall (fn, args) ->
      List.fold_right (fun arg code -> compile_expr arg ++ pushq !%rax ++ code) args nop ++
      call fn.fn_name ++
      addq (imm (8 * List.length args)) !%rsp
  | TElist _ -> failwith "Lists are not supported in code generation"
  | TErange _ -> failwith "Range is not supported in code generation"
  | TEget _ -> failwith "Get is not supported in code generation"

(* Define a table to store variable names and their types *)
let var_table = Hashtbl.create 10

(* Function to record variable name and type *)
let record_var_name var_name var_type =
  Hashtbl.replace var_table var_name var_type

let rec compile_stmt = function
  | TSif (cond, then_branch, else_branch) ->
      let else_label = new_label () in
      let end_label = new_label () in
      compile_expr cond ++
      testq !%rax !%rax ++
      jz else_label ++
      compile_stmt then_branch ++
      jmp end_label ++
      X86_64.label else_label ++
      compile_stmt else_branch ++
      X86_64.label end_label
  | TSreturn e -> compile_expr e ++ ret
  | TSassign (v, e) -> 
      (* Record the variable name and type *)
      (match e with
       | TEcst (Cstring s) ->
           record_var_name v.v_name "string";
           let lbl = new_label () in
           string_constants := (lbl, s) :: !string_constants;
           leaq (lab lbl) rax ++
           movq !%rax (ind ~ofs:v.v_ofs rbp)
       | _ -> compile_expr e ++ movq !%rax (ind ~ofs:v.v_ofs rbp))
  | TSprint e -> 
      (match e with
       | TEvar v ->
           (match Hashtbl.find_opt var_table v.v_name with
            | Some "string" ->
                compile_expr e ++
                movq !%rax !%rsi ++
                leaq (lab "fmt_str") rdi ++
                movq (imm 0) !%rax ++
                call "printf"
            | _ -> 
                compile_expr e ++
                movq !%rax !%rsi ++
                leaq (lab "fmt_int") rdi ++
                movq (imm 0) !%rax ++
                call "printf")
       | TEbinop (Badd, TEcst (Cstring _), TEcst (Cstring _)) ->
           compile_expr e ++
           movq !%rax !%rsi ++
           leaq (lab "fmt_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf"
       | TEcst (Cstring s) ->
           let lbl = new_label () in
           string_constants := (lbl, s) :: !string_constants;
           leaq (lab lbl) rax ++
           movq !%rax !%rsi ++
           leaq (lab "fmt_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf"
       | TEcst (Cbool true) ->
           leaq (lab "true_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf"          
       | TEbinop (Band, _, _) | TEbinop (Bor, _, _)
       | TEbinop (Beq, _, _) | TEbinop (Bneq, _, _) | TEbinop (Blt, _, _) 
       | TEbinop (Ble, _, _) | TEbinop (Bgt, _, _) | TEbinop (Bge, _, _) ->
           let false_label = new_label () in
           let end_label = new_label () in
           compile_expr e ++
           testq !%rax !%rax ++
           jz false_label ++
           leaq (lab "true_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf" ++
           jmp end_label ++
           X86_64.label false_label ++
           leaq (lab "false_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf" ++
           X86_64.label end_label
       | TEcst (Cbool false) ->
           leaq (lab "false_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf"
       | _ ->
           compile_expr e ++
           movq !%rax !%rsi ++
           leaq (lab "fmt_int") rdi ++
           movq (imm 0) !%rax ++
           call "printf")
  | TSblock stmts -> List.fold_left (++) nop (List.map compile_stmt stmts)
  | TSfor _ -> failwith "For loops are not supported in code generation"
  | TSeval e -> compile_expr e
  | TSset _ -> failwith "Set is not supported in code generation"

let compile_def (fn, body) =
  X86_64.label fn.fn_name ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  compile_stmt body ++
  movl (imm 0) !%eax ++
  popq rbp ++
  ret

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let text_section = 
    globl "main" ++ 
    X86_64.label "main" ++ 
    List.fold_left (++) nop (List.map compile_def p) in
  let data_section = 
    List.fold_left (fun acc (lbl, str) -> acc ++ X86_64.label lbl ++ string str) nop !string_constants ++
    X86_64.label "fmt_int" ++
    string "%d\n" ++
    X86_64.label "fmt_str" ++
    string "%s\n" ++
    X86_64.label "true_str" ++
    string "True\n" ++
    X86_64.label "false_str" ++
    string "False\n"
    in
  { text = text_section; data = data_section }