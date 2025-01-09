open Format
open X86_64
open Ast
open Component

(* 1. put_character *)
let put_character (c : char) : X86_64.text =
  let ascii_val = Char.code c in
  movq (imm ascii_val) !%rdi
  ++ call "putchar_wrapper"

(* 2. unique_label *)
let unique_label (env : env_t) (prefix : string) : string =
  let current =
    match StringMap.find_opt prefix env.counters with
    | Some n -> n
    | None -> 0
  in
  env.counters <- StringMap.add prefix (current + 1) env.counters;
  Printf.sprintf "%s%d" prefix current

(* 3. repeat *)
let repeat (n : int) (text : X86_64.text) : X86_64.text =
  let rec loop k acc =
    if k <= 0 then acc
    else loop (k - 1) (acc ++ text)
  in
  loop n nop

(* 4. main_guard *)
let main_guard (function_name : string) : label =
  if String.equal function_name "main" then "another_main"
  else function_name

(* 5. arith_asm *)
let arith_asm (code1 : X86_64.text) (code2 : X86_64.text) (instructions : X86_64.text)
  : X86_64.text
=
  let prepare_first =
    code1
    ++ movq (ind rax) !%r8
    ++ movq (ind ~ofs:byte rax) !%rdi
    ++ pushq !%rdi
    ++ pushq !%r8
  in
  let finalize =
    popq r8
    ++ popq rdi
    ++ movq (ind rax) !%r9
    ++ movq (ind ~ofs:byte rax) !%rsi
    ++ cmpq !%r8 !%r9
    ++ jne "runtime_error"
    ++ cmpq (imm 2) !%r8
    ++ jne "runtime_error"
    ++ instructions
    ++ pushq (reg rdi)
    ++ movq (imm (2 * byte)) (reg rdi)
    ++ call "malloc_wrapper"
    ++ popq rdi
    ++ movq (imm 2) (ind rax)
    ++ movq (reg rdi) (ind ~ofs:byte rax)
  in
  prepare_first
  ++ code2
  ++ finalize

(* 6. two_byte_operator_asm *)
let two_byte_operator_asm
  (env : env_t)
  (code1 : X86_64.text)
  (code2 : X86_64.text)
  (instructions : X86_64.text)
  : X86_64.text
=
  let lbl_two_byte = unique_label env "two_byte" in
  let lbl_string_cmp = unique_label env "string_cmp" in
  let lbl_end       = unique_label env "end_label" in

  let prepare_op =
    code1
    ++ movq (ind rax) !%r8
    ++ movq (ind ~ofs:byte rax) !%rdi
    ++ pushq !%rdi
    ++ pushq !%r8
    ++ code2
    ++ popq r8
    ++ popq rdi
    ++ movq (ind rax) !%r9
    ++ movq (ind ~ofs:byte rax) !%rsi
  in
  let check_type =
    cmpq !%r8 !%r9
    ++ jne "runtime_error"
    ++ cmpq (imm 0) !%r8
    ++ je "runtime_error"
    ++ cmpq (imm 2) !%r8
    ++ jle lbl_two_byte
    ++ cmpq (imm 3) !%r8
    ++ je lbl_string_cmp
    ++ cmpq (imm 4) !%r8
    ++ jne "runtime_error"
    ++ jmp lbl_end
  in
  let handle_two_byte =
    label lbl_two_byte
    ++ instructions
    ++ pushq (reg rdi)
    ++ movq (imm byte) (reg rdi)
    ++ call "malloc_wrapper"
    ++ popq rdi
    ++ movq (imm 1) (ind rax)
    ++ movq (reg rdi) (ind ~ofs:byte rax)
    ++ jmp lbl_end
  in
  let handle_string_cmp =
    label lbl_string_cmp
    ++ nop  
  in
  prepare_op
  ++ check_type
  ++ handle_two_byte
  ++ handle_string_cmp
  ++ label lbl_end


let c_standard_function_wrapper (fn_name : string) : X86_64.text =
  label (fn_name ^ "_wrapper")
  ++ pushq (reg r8)
  ++ pushq (reg r10)
  ++ pushq (reg rbp)
  ++ movq (reg rsp) (reg rbp)
  ++ andq (imm (-16)) (reg rsp)
  ++ call fn_name
  ++ movq (reg rbp) (reg rsp)
  ++ popq rbp
  ++ popq r10
  ++ popq r8
  ++ ret


let bool_builder i =
  movq (imm (2 * byte)) !%rdi
  ++ call "malloc_wrapper"
  ++ movq (imm 1) (ind rax)
  ++ movq (imm i) (ind ~ofs:byte rax)


let none_builder =
  movq (imm (2 * byte)) !%rdi
  ++ call "malloc_wrapper"
  ++ movq (imm 0) (ind rax)
  ++ movq (imm 0) (ind ~ofs:byte rax)


let difference env fn_label instructions none_guard =
  let lbl_diff_ty_eq          = unique_label env "func_diff_ty_eq" in
  let lbl_diff_value_bool_int = unique_label env "func_eq_value_bool_int" in
  let lbl_diff_value_string   = unique_label env "func_eq_value_string" in
  let lbl_diff_value_list_lp  = unique_label env "func_eq_value_list_loop" in
  let lbl_diff_value_list_end = unique_label env "func_eq_value_list_end" in
  let lbl_diff_value_end      = unique_label env "func_eq_value_end" in
  let lbl_counter_guard       = unique_label env "func_counter" in

  label fn_label
  ++ pushq !%rbp
  ++ movq !%rsp !%rbp

  ++ movq (ind rdi) !%r10
  ++ movq (ind rsi) !%r11
  ++ cmpq !%r10 !%r11
  ++ je lbl_diff_ty_eq
  ++ instructions

  ++ label lbl_diff_ty_eq
  ++ none_guard
  ++ cmpq (imm 2) !%r10
  ++ jle lbl_diff_value_bool_int
  ++ cmpq (imm 3) !%r10
  ++ je lbl_diff_value_string
  ++ cmpq (imm 4) !%r10
  ++ jne "runtime_error"

  ++ movq (ind ~ofs:byte rdi) !%rcx
  ++ movq (ind ~ofs:byte rsi) !%r10
  ++ cmpq !%r10 !%rcx
  ++ jl lbl_counter_guard
  ++ movq !%r10 !%rcx

  ++ label lbl_counter_guard
  ++ movq !%rdi !%r8
  ++ addq (imm (2 * byte)) !%r8
  ++ movq !%rsi !%r9
  ++ addq (imm (2 * byte)) !%r9

  ++ label lbl_diff_value_list_lp
  ++ testq !%rcx !%rcx
  ++ jz lbl_diff_value_list_end
  ++ pushq !%rdi
  ++ pushq !%rsi
  ++ movq (ind r8) !%rdi
  ++ movq (ind r9) !%rsi
  ++ pushq !%rcx
  ++ pushq !%r8
  ++ pushq !%r9
  ++ call fn_label
  ++ popq r9
  ++ popq r8
  ++ popq rcx
  ++ popq rsi
  ++ popq rdi

  ++ cmpq (imm 0) !%rax
  ++ jne lbl_diff_value_end
  ++ decq !%rcx
  ++ addq (imm byte) !%r8
  ++ addq (imm byte) !%r9
  ++ jmp lbl_diff_value_list_lp

  ++ label lbl_diff_value_bool_int
  ++ movq (ind ~ofs:byte rdi) !%rax
  ++ movq (ind ~ofs:byte rsi) !%r9
  ++ subq !%r9 !%rax
  ++ jmp lbl_diff_value_end

  ++ label lbl_diff_value_string
  ++ movq (ind ~ofs:(2 * byte) rdi) !%rdi
  ++ movq (ind ~ofs:(2 * byte) rsi) !%rsi
  ++ call "strcmp_wrapper"
  ++ jmp lbl_diff_value_end

  ++ label lbl_diff_value_list_end
  ++ movq (ind ~ofs:byte rdi) !%rax
  ++ movq (ind ~ofs:byte rsi) !%r9
  ++ subq !%r9 !%rax

  ++ label lbl_diff_value_end
  ++ leave
  ++ ret
;;