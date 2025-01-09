open Format
open X86_64
open Ast
open Utils
open Functions
open Component

let debug = ref false

let rec compile_expr (env : env_t) (parent_env : env_t) (expr : Ast.texpr)
  : X86_64.text * X86_64.data
  =
  match expr with
  | TEcst c ->
    (match c with
     | Cnone ->
       ( movq (imm (2 * byte)) !%rdi
         ++ call "malloc_wrapper"
         ++ movq (imm 0) (ind rax)
         ++ movq (imm 0) (ind ~ofs:byte rax)
       , nop )
     | Cbool b ->
       ( movq (imm (2 * byte)) !%rdi
         ++ call "malloc_wrapper"
         ++ movq (imm 1) (ind rax)
         ++ movq (imm (if b then 1 else 0)) (ind ~ofs:byte rax)
       , nop )
     | Cint i ->
       ( movq (imm (2 * byte)) !%rdi
         ++ call "malloc_wrapper"
         ++ movq (imm 2) (ind rax)
         ++ movq (imm64 i) (ind ~ofs:byte rax)
       , nop )
     | Cstring s ->
       let len = String.length s in
       let string_label = unique_label parent_env "Cstring" in
       ( movq (imm (3 * byte)) !%rdi
         ++ call "malloc_wrapper"
         ++ movq (imm 3) (ind rax)
         ++ movq (imm len) (ind ~ofs:byte rax)
         ++ leaq (lab string_label) rdi
         ++ movq !%rdi (ind ~ofs:(2 * byte) rax)
       , label string_label ++ string s ))
  | TEvar v ->
    (try
       let ofs = StringMap.find v.v_name env.vars in
       movq (ind ~ofs:(-ofs - parent_env.stack_offset) rbp) !%rax, nop
     with
     | Not_found ->
       failwith (Printf.sprintf "Variable %s not found in environment" v.v_name))
  | TEbinop (op, e1, e2) ->
    (match op with
     | Band ->
       let text_code1, data_code1 = compile_expr env parent_env e1 in
       let text_code2, data_code2 = compile_expr env parent_env e2 in
       let and_false = unique_label parent_env "and_false" in
       ( text_code1
         ++ movq (ind ~ofs:byte rax) !%rdi
         ++ cmpq (imm 0) !%rdi
         ++ je and_false
         ++ text_code2
         ++ label and_false
       , data_code1 ++ data_code2 )
     | Bor ->
       let text_code1, data_code1 = compile_expr env parent_env e1 in
       let text_code2, data_code2 = compile_expr env parent_env e2 in
       let or_true = unique_label parent_env "or_true" in
       ( text_code1
         ++ movq (ind ~ofs:byte rax) !%rdi
         ++ cmpq (imm 0) !%rdi
         ++ jne or_true
         ++ text_code2
         ++ label or_true
       , data_code1 ++ data_code2 )
     | Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Bneq | Blt | Ble | Bgt | Bge ->
       let text_code1, data_code1 = compile_expr env parent_env e1 in
       let text_code2, data_code2 = compile_expr env parent_env e2 in
       (match op with
        | Badd ->
          let add_end = unique_label parent_env "add_end" in
          let add_int = unique_label parent_env "add_int" in
          let add_string = unique_label parent_env "add_string" in
          ( text_code1
            ++ movq !%rax !%rdi
            ++ pushq !%rdi
            ++ text_code2
            ++ popq rdi
            ++ movq !%rax !%rsi
            ++ movq (ind rdi) !%r10
            ++ movq (ind rsi) !%r11
            ++ cmpq !%r10 !%r11
            ++ jne "runtime_error"
            ++ cmpq (imm 2) !%r10
            ++ je add_int
            ++ cmpq (imm 3) !%r10
            ++ je add_string
            ++ cmpq (imm 4) !%r10
            ++ jne "runtime_error"
            ++ call "concat_list"
            ++ jmp add_end
            ++ label add_int
            ++ movq (ind ~ofs:byte rdi) !%rdi
            ++ addq (ind ~ofs:byte rsi) !%rdi
            ++ pushq (reg rdi)
            ++ movq (imm (2 * byte)) (reg rdi)
            ++ call "malloc_wrapper"
            ++ popq rdi
            ++ movq (imm 2) (ind rax)
            ++ movq (reg rdi) (ind ~ofs:byte rax)
            ++ jmp add_end
            ++ label add_string
            ++ movq (ind ~ofs:byte rdi) !%rdx
            ++ addq (ind ~ofs:byte rsi) !%rdx
            ++ movq !%rdx !%rcx
            ++ addq (imm 1) !%rdx
            ++ imulq (imm byte) !%rdx
            ++ pushq !%rcx
            ++ pushq !%rdi
            ++ pushq !%rsi
            ++ movq !%rdx !%rdi
            ++ call "malloc_wrapper"
            ++ popq rsi
            ++ popq rdi
            ++ movq (ind ~ofs:(2 * byte) rdi) !%rdi
            ++ movq (ind ~ofs:(2 * byte) rsi) !%rsi
            ++ pushq !%rdi
            ++ pushq !%rsi
            ++ movq !%rdi !%rsi
            ++ movq !%rax !%rdi
            ++ call "strcat_wrapper"
            ++ popq rsi
            ++ popq rdi
            ++ movq !%rax !%rdi
            ++ call "strcat_wrapper"
            ++ movq (imm (3 * byte)) !%rdi
            ++ movq !%rax !%rsi
            ++ pushq !%rsi
            ++ call "malloc_wrapper"
            ++ popq rsi
            ++ popq rcx
            ++ movq (imm 3) (ind rax)
            ++ movq !%rcx (ind ~ofs:byte rax)
            ++ movq !%rsi (ind ~ofs:(2 * byte) rax)
            ++ label add_end
          , data_code1 ++ data_code2 )
        | Bsub ->
          arith_asm text_code1 text_code2 (subq !%rsi !%rdi), data_code1 ++ data_code2
        | Bmul ->
          arith_asm text_code1 text_code2 (imulq !%rsi !%rdi), data_code1 ++ data_code2
        | Bdiv ->
          ( arith_asm
              text_code1
              text_code2
              (movq !%rdi !%rax
               ++ cqto
               ++ movq !%rsi !%rbx
               ++ idivq !%rbx
               ++ movq !%rax !%rdi)
          , data_code1 ++ data_code2 )
        | Bmod ->
          ( arith_asm
              text_code1
              text_code2
              (movq !%rdi !%rax
               ++ cqto
               ++ movq !%rsi !%rbx
               ++ idivq !%rbx
               ++ movq !%rdx !%rdi)
          , data_code1 ++ data_code2 )
        | Beq ->
          let beq = unique_label parent_env "beq_false" in
          let beq_end = unique_label parent_env "beq_end" in
          ( text_code1
            ++ movq !%rax !%rdi
            ++ pushq !%rdi
            ++ text_code2
            ++ popq rdi
            ++ movq !%rax !%rsi
            ++ call "func_difference_eq"
            ++ cmpq (imm 0) !%rax
            ++ jne beq
            ++ bool_builder 1
            ++ jmp beq_end
            ++ label beq
            ++ bool_builder 0
            ++ label beq_end
          , data_code1 ++ data_code2 )
        | Bneq ->
          let beq = unique_label parent_env "beq_false" in
          let beq_end = unique_label parent_env "beq_end" in
          ( text_code1
            ++ movq !%rax !%rdi
            ++ pushq !%rdi
            ++ text_code2
            ++ popq rdi
            ++ movq !%rax !%rsi
            ++ call "func_difference_neq"
            ++ cmpq (imm 0) !%rax
            ++ je beq
            ++ bool_builder 1
            ++ jmp beq_end
            ++ label beq
            ++ bool_builder 0
            ++ label beq_end
          , data_code1 ++ data_code2 )
        | Blt ->
          let blt = unique_label parent_env "blt_false" in
          let blt_end = unique_label parent_env "blt_end" in
          ( text_code1
            ++ movq !%rax !%rdi
            ++ pushq !%rdi
            ++ text_code2
            ++ popq rdi
            ++ movq !%rax !%rsi
            ++ call "func_difference_sss"
            ++ cmpl (imm 0) !%eax
            ++ jge blt
            ++ bool_builder 1
            ++ jmp blt_end
            ++ label blt
            ++ bool_builder 0
            ++ label blt_end
          , data_code1 ++ data_code2 )
        | Ble ->
          let ble = unique_label parent_env "ble_false" in
          let ble_end = unique_label parent_env "ble_end" in
          ( text_code1
            ++ movq !%rax !%rdi
            ++ pushq !%rdi
            ++ text_code2
            ++ popq rdi
            ++ movq !%rax !%rsi
            ++ call "func_difference_sss"
            ++ cmpl (imm 0) !%eax
            ++ jg ble
            ++ bool_builder 1
            ++ jmp ble_end
            ++ label ble
            ++ bool_builder 0
            ++ label ble_end
          , data_code1 ++ data_code2 )
        | Bgt ->
          let bgt = unique_label parent_env "bgt_false" in
          let bgt_end = unique_label parent_env "bgt_end" in
          ( text_code1
            ++ movq !%rax !%rdi
            ++ pushq !%rdi
            ++ text_code2
            ++ popq rdi
            ++ movq !%rax !%rsi
            ++ call "func_difference_sss"
            ++ cmpl (imm 0) !%eax
            ++ jle bgt
            ++ bool_builder 1
            ++ jmp bgt_end
            ++ label bgt
            ++ bool_builder 0
            ++ label bgt_end
          , data_code1 ++ data_code2 )
        | Bge ->
          let bge = unique_label parent_env "bge_false" in
          let bge_end = unique_label parent_env "bge_end" in
          ( text_code1
            ++ movq !%rax !%rdi
            ++ pushq !%rdi
            ++ text_code2
            ++ popq rdi
            ++ movq !%rax !%rsi
            ++ call "func_difference_sss"
            ++ cmpl (imm 0) !%eax
            ++ jl bge
            ++ bool_builder 1
            ++ jmp bge_end
            ++ label bge
            ++ bool_builder 0
            ++ label bge_end
          , data_code1 ++ data_code2 )
        | _ -> failwith "Unsupported binop"))
  | TEunop (op, e) ->
    (match op with
     | Uneg ->
       let text_code, data_code = compile_expr env parent_env e in
       ( text_code
         ++ movq (ind rax) !%r10
         ++ cmpq (imm 2) !%r10
         ++ jne "runtime_error"
         ++ movq (ind ~ofs:byte rax) !%rdi
         ++ negq !%rdi
         ++ pushq !%rdi
         ++ movq (imm (2 * byte)) !%rdi
         ++ call "malloc_wrapper"
         ++ popq rdi
         ++ movq (imm 2) (ind rax)
         ++ movq !%rdi (ind ~ofs:byte rax)
       , data_code )
     | Unot ->
       let text_code, data_code = compile_expr env parent_env e in
       ( text_code
         ++ movq (ind rax) !%r10
         ++ cmpq (imm 1) !%r10
         ++ jne "runtime_error"
         ++ movq (ind ~ofs:byte rax) !%rdi
         ++ xorq (imm 1) !%rdi
         ++ pushq !%rdi
         ++ movq (imm (2 * byte)) !%rdi
         ++ call "malloc_wrapper"
         ++ popq rdi
         ++ movq (imm 1) (ind rax)
         ++ movq !%rdi (ind ~ofs:byte rax)
       , data_code ))
  | TEcall (fn, args) ->
    (match fn.fn_name with
     | "len" ->
       if List.length args = 1
       then (
         let text_code, data_code = compile_expr env parent_env (List.hd args) in
         text_code ++ call "func_len", data_code)
       else failwith "len function takes exactly one argument"
     | "list" ->
       if List.length args = 1
       then (
         let text_code, data_code = compile_expr env parent_env (List.hd args) in
         text_code ++ movq !%rax !%rdi ++ call "func_list", data_code)
       else failwith "list function takes exactly one argument"
     | _ ->
       List.fold_left
         (fun (text_acc, data_acc, acc_local_var) arg ->
           let text_expr, data_expr = compile_expr env parent_env arg in
           text_acc ++ text_expr ++ pushq !%rax, data_acc ++ data_expr, acc_local_var + 1)
         (nop, nop, 0)
         (List.rev args)
       |> fun (var_text, var_data, local_var) ->
       var_text ++ call (main_guard fn.fn_name) ++ repeat local_var (popq r15), var_data)
  | TElist l ->
    let len = List.length l in
    List.fold_left
      (fun (text_acc, data_acc, counter) i ->
        let text_expr, data_expr = compile_expr env parent_env i in
        ( text_acc
          ++ pushq !%rax
          ++ text_expr
          ++ movq !%rax !%rsi
          ++ popq rax
          ++ movq !%rsi (ind ~ofs:counter rax)
        , data_acc ++ data_expr
        , counter + byte ))
      (nop, nop, 2 * byte)
      l
    |> fun (text_code, data_code, _) ->
    ( movq (imm ((len + 2) * byte)) !%rdi
      ++ call "malloc_wrapper"
      ++ movq (imm 4) (ind rax)
      ++ movq (imm len) (ind ~ofs:byte rax)
      ++ text_code
    , data_code )
  | TErange e ->
    let text_code, data_code = compile_expr env parent_env e in
    ( text_code
      ++ movq (ind rax) !%r10
      ++ cmpq (imm 2) !%r10
      ++ jne "runtime_error"
      ++ movq (ind ~ofs:byte rax) !%r10
      ++ movq (imm (2 * byte)) !%rdi
      ++ call "malloc_wrapper"
      ++ movq (imm 5) (ind rax)
      ++ movq !%r10 (ind ~ofs:byte rax)
    , data_code )
  | TEget (e1, e2) ->
    let text_code1, data_code1 = compile_expr env parent_env e1 in
    let text_code2, data_code2 = compile_expr env parent_env e2 in
    ( text_code1
      ++ movq !%rax !%rdi
      ++ pushq !%rdi
      ++ text_code2
      ++ popq rdi
      ++ movq !%rax !%rsi
      ++ call "func_list_get"
      ++ movq (ind rax) !%rax
    , data_code1 ++ data_code2 )
;;

let rec compile_stmt (env : env_t) (parent_env : env_t) (stmt : Ast.tstmt)
  : X86_64.text * X86_64.data * bool
  =
  match stmt with
  | TSif (cond, s1, s2) ->
    let text_code_cond, data_code_cond = compile_expr env parent_env cond in
    let text_code_s1, data_code_s1, is_return_s1 = compile_stmt env parent_env s1 in
    let text_code_s2, data_code_s2, is_return_s2 = compile_stmt env parent_env s2 in
    let else_label = unique_label parent_env "if_else" in
    let end_label = unique_label parent_env "if_end" in
    ( text_code_cond
      ++ cmpq (imm 0) (ind ~ofs:byte rax)
      ++ je else_label
      ++ text_code_s1
      ++ jmp end_label
      ++ label else_label
      ++ text_code_s2
      ++ label end_label
    , data_code_cond ++ data_code_s1 ++ data_code_s2
    , is_return_s1 && is_return_s2 )
  | TSreturn expr ->
    let text_code, data_code = compile_expr env parent_env expr in
    ( text_code ++ addq (imm env.stack_offset) !%rsp ++ movq !%rbp !%rsp ++ popq rbp ++ ret
    , data_code
    , true )
  | TSassign (var, expr) ->
    (try
       let offset = StringMap.find var.v_name env.vars in
       let text_code, data_code = compile_expr env parent_env expr in
       text_code ++ movq !%rax (ind ~ofs:(-offset) rbp), data_code, false
     with
     | Not_found ->
       env.stack_offset <- env.stack_offset + byte;
       let offset = env.stack_offset in
       let text_code, data_code = compile_expr env parent_env expr in
       env.vars <- StringMap.add var.v_name offset env.vars;
       text_code ++ movq !%rax (ind ~ofs:(-offset) rbp), data_code, false)
  | TSprint expr ->
    let text_code, data_code = compile_expr env parent_env expr in
    ( text_code ++ movq !%rax !%rdi ++ call "print_value" ++ put_character '\n'
    , data_code
    , false )
  | TSblock stmts ->
    List.fold_left
      (fun (acc_text, acc_data, acc_is_return) stmt ->
        let text_code, data_code, is_return = compile_stmt env parent_env stmt in
        acc_text ++ text_code, acc_data ++ data_code, acc_is_return || is_return)
      (nop, nop, false)
      stmts
  | TSfor (var, expr, body) ->
    let expr_text_code, expr_data_code = compile_expr env parent_env expr in
    let compile_loop_code env offset expr_text_code body_text_code =
      let loop_label = unique_label parent_env "for_loop" in
      let end_label = unique_label parent_env "for_end" in
      let do_jump = unique_label parent_env "for_do_jump" in
      expr_text_code
      ++ movq !%rax !%rdi
      ++ movq (ind ~ofs:byte rax) !%rcx
      ++ addq (imm (2 * byte)) !%rdi
      ++ movq !%rdi !%rsi
      ++ movq (ind rdi) !%r10
      ++ movq !%r10 (ind ~ofs:(-offset) rbp)
      ++ jmp do_jump
      ++ label loop_label
      ++ testq !%rcx !%rcx
      ++ jz end_label
      ++ addq (imm byte) !%rsi
      ++ movq (ind rsi) !%r10
      ++ movq !%r10 (ind ~ofs:(-offset) rbp)
      ++ label do_jump
      ++ testq !%rcx !%rcx
      ++ jz end_label
      ++ pushq !%rcx
      ++ pushq !%rdi
      ++ pushq !%rsi
      ++ body_text_code
      ++ popq rsi
      ++ popq rdi
      ++ popq rcx
      ++ decq !%rcx
      ++ jmp loop_label
      ++ label end_label
    in
    (try
       (* Case: var already in env. *)
       let found_ofs = StringMap.find var.v_name env.vars in
       let body_text_code, body_data_code, is_return = compile_stmt env parent_env body in
       let loop_code = compile_loop_code env found_ofs expr_text_code body_text_code in
       loop_code, expr_data_code ++ body_data_code, is_return
     with
     | Not_found ->
       (* Case: var not in env, must allocate new stack offset. *)
       env.stack_offset <- env.stack_offset + byte;
       env.vars <- StringMap.add var.v_name env.stack_offset env.vars;
       let item_ofs = env.stack_offset in
       let body_text_code, body_data_code, is_return = compile_stmt env parent_env body in
       let loop_code = compile_loop_code env item_ofs expr_text_code body_text_code in
       loop_code, expr_data_code ++ body_data_code, is_return)
  | TSeval expr ->
    let text_code, data_code = compile_expr env parent_env expr in
    text_code, data_code, false
  | TSset (e1, e2, e3) ->
    let text_code1, data_code1 = compile_expr env parent_env e1 in
    let text_code2, data_code2 = compile_expr env parent_env e2 in
    let text_code3, data_code3 = compile_expr env parent_env e3 in
    ( text_code1
      ++ movq !%rax !%rdi
      ++ pushq !%rdi
      ++ text_code2
      ++ popq rdi
      ++ movq !%rax !%rsi
      ++ call "func_list_get"
      ++ movq !%rax !%rdi
      ++ pushq !%rdi
      ++ text_code3
      ++ popq rdi
      ++ movq !%rax (ind rdi)
    , data_code1 ++ data_code2
    , false )
;;

let compile_def env parent_env ((fn, body) : Ast.tdef) : X86_64.text * X86_64.data =
  (* add vars in env *)
  List.fold_left
    (fun (offset, n_offset, vars) var ->
      let new_offset = offset + byte in
      let new_n_offset = n_offset - byte in
      new_offset, new_n_offset, StringMap.add var.v_name (new_n_offset - byte) vars)
    (env.stack_offset, 0, env.vars)
    fn.fn_params
  |> fun (stack_offsets, _, updated_vars) ->
  env.stack_offset <- stack_offsets;
  env.vars <- updated_vars;
  let body_code, data_code, is_return = compile_stmt env parent_env body in
  let prologue = pushq !%rbp ++ movq !%rsp !%rbp ++ subq (imm env.stack_offset) !%rsp in
  if fn.fn_name = "__main"
  then (
    let epilogue =
      addq (imm env.stack_offset) !%rsp
      ++ xorq !%rax !%rax
      ++ movq !%rbp !%rsp
      ++ popq rbp
      ++ ret
    in
    label "main" ++ prologue ++ body_code ++ epilogue, data_code)
  else if is_return
  then label (main_guard fn.fn_name) ++ prologue ++ body_code, data_code
  else (
    let epilogue =
      addq (imm env.stack_offset) !%rsp
      ++ none_builder
      ++ movq !%rbp !%rsp
      ++ popq rbp
      ++ ret
    in
    label (main_guard fn.fn_name) ++ prologue ++ body_code ++ epilogue, data_code)
;;

let file ?debug:(b = false) (p : Ast.tfile) : X86_64.program =
  debug := b;
  let global_env = create_env () in
  let text_fn, data_fn = func global_env in
  let text_code, data_code =
    List.fold_left
      (fun (text_acc, data_acc) (fn, body) ->
        let env = create_env () in
        let text_fn, data_fn = compile_def env global_env (fn, body) in
        text_acc ++ text_fn, data_acc ++ data_fn)
      (nop, nop)
      p
  in
  let csfw = [ "malloc"; "putchar"; "printf"; "strcmp"; "strcpy"; "strcat" ] in
  { text =
      List.fold_left (fun acc fn -> acc ++ c_standard_function_wrapper fn) nop csfw
      ++ text_fn
      ++ globl "main"
      ++ text_code
  ; data = data_fn ++ data_code ++ inline "\t.section .note.GNU-stack,\"\",@progbits\n"
  }
;;