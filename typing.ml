open Ast

let debug = ref false
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
   error ~loc "unbound variable %s" id
*)
let error ?(loc = dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")
;;

let debug_msg f =
  if !debug
  then Format.eprintf ("@[" ^^ f ^^ "@]@.")
  else Format.ifprintf Format.err_formatter ("@[" ^^ f ^^ "@]@.")
;;

(* type of typed expressions *)
(* the type of a variable environment, record type *)
(* vars is a named component called "field" *)
(* vars is a field with type ty StringMap.t *)
(* a map type with ty as value and string as key *)

module StringMap = Map.Make (String)

type env_t =
  { vars : var StringMap.t
  ; funcs : fn StringMap.t
  }

(* defining a value of type env *)
let empty_env = { vars = StringMap.empty; funcs = StringMap.empty }

let find_var (env : env_t) (id : string) : var option =
  try Some (StringMap.find id env.vars) with
  | Not_found -> None
;;

(* type of typed expressions *)
let rec type_expr (env : env_t) (expr : Ast.expr) : Ast.texpr * env_t =
  match expr with
  | Ecst c -> TEcst c, env
  | Eident id ->
    let v =
      try StringMap.find id.id env.vars with
      | Not_found -> error ~loc:id.loc "unbound variable %s" id.id
    in
    TEvar v, env
  | Ebinop (op, e1, e2) ->
    let te1, env = type_expr env e1 in
    let te2, env = type_expr env e2 in
    TEbinop (op, te1, te2), env
  | Eunop (op, e) ->
    let te, env = type_expr env e in
    TEunop (op, te), env
  | Ecall (id, args) ->
    (match id.id with
     | "len" ->
       let targs, env =
         List.fold_left
           (fun (acc, env) arg ->
             let targ, env = type_expr env arg in
             targ :: acc, env)
           ([], env)
           args
       in
       let targs = List.rev targs in
       if List.length targs <> 1
       then
         error
           ~loc:id.loc
           "function %s expects %d argument(s) but got %d"
           id.id
           1
           (List.length targs)
       else TEcall ({ fn_name = "len"; fn_params = [] }, targs), env
     | "range" ->
       let targs, env =
         List.fold_left
           (fun (acc, env) arg ->
             let targ, env = type_expr env arg in
             targ :: acc, env)
           ([], env)
           args
       in
       let targs = List.rev targs in
       if List.length targs <> 1
       then
         error
           ~loc:id.loc
           "function %s expects %d argument(s) but got %d"
           id.id
           1
           (List.length targs)
       else TErange (List.hd targs), env
     | "list" ->
       let targs, env =
         List.fold_left
           (fun (acc, env) arg ->
             let targ, env = type_expr env arg in
             targ :: acc, env)
           ([], env)
           args
       in
       if List.length targs <> 1
       then
         error
           ~loc:id.loc
           "function %s expects at least %d argument(s) but got %d"
           id.id
           1
           (List.length targs)
       else (
         match List.hd targs with
         | TEcst _ -> error ~loc:id.loc "cannot create list from constant"
         | _ -> TEcall ({ fn_name = "list"; fn_params = [] }, targs), env)
     | _ ->
       let fn =
         try StringMap.find id.id env.funcs with
         | Not_found -> error ~loc:id.loc "unbound function %s" id.id
       in
       let targs, env =
         List.fold_left
           (fun (acc, env) arg ->
             let targ, env = type_expr env arg in
             targ :: acc, env)
           ([], env)
           args
       in
       let targs = List.rev targs in
       if List.length targs <> List.length fn.fn_params
       then
         error
           ~loc:id.loc
           "The function %s requires only %d argument(s), but %d were provided."
           id.id
           (List.length fn.fn_params)
           (List.length targs)
       else TEcall (fn, targs), env)
  | Elist l ->
    List.fold_left
      (fun (acc, env) e ->
        let te, env = type_expr env e in
        te :: acc, env)
      ([], env)
      l
    |> fun (tl, env) -> TElist (List.rev tl), env
  | Eget (e1, e2) ->
    let te1, env = type_expr env e1 in
    let te2, env = type_expr env e2 in
    TEget (te1, te2), env
;;

let rec type_stmt (env : env_t) (stmt : Ast.stmt) : Ast.tstmt * env_t =
  match stmt with
  | Sif (cond, then_, else_) ->
    let tcond, env = type_expr env cond in
    let tthen, env = type_stmt env then_ in
    let telse, env = type_stmt env else_ in
    TSif (tcond, tthen, telse), env
  | Sreturn e ->
    let te, env = type_expr env e in
    TSreturn te, env
  | Sassign (id, e) ->
    let te, env = type_expr env e in
    (match StringMap.find id.id env.vars with
     | v -> TSassign (v, te), env
     | exception Not_found ->
       let v = { v_name = id.id } in
       let env = { env with vars = StringMap.add id.id v env.vars } in
       TSassign (v, te), env)
  | Sprint e ->
    let te, env = type_expr env e in
    TSprint te, env
  | Sblock stmts ->
    let rec type_block stmts env =
      match stmts with
      | [] -> [], env
      | stmt :: stmts ->
        let tstmt, env' = type_stmt env stmt in
        let tstmts, env'' = type_block stmts env' in
        tstmt :: tstmts, env''
    in
    let stmts', env = type_block stmts env in
    TSblock stmts', env
  | Sfor (id, e, body) ->
    let te, env = type_expr env e in
    let v = { v_name = id.id } in
    let env = { env with vars = StringMap.add id.id v env.vars } in
    let tbody, env = type_stmt env body in
    TSfor (v, te, tbody), env
  | Seval e ->
    let te, env = type_expr env e in
    TSeval te, env
  | Sset (e1, e2, e3) ->
    let te1, env = type_expr env e1 in
    let te2, env = type_expr env e2 in
    let te3, env = type_expr env e3 in
    TSset (te1, te2, te3), env
;;

let type_def (env : env_t) ((fn_name, fn_params, body) : Ast.def) : Ast.tdef * env_t =
  let fn_params_vars = List.map (fun id -> { v_name = id.id }) fn_params in
  if List.length fn_params_vars
     <> List.length
          (List.sort_uniq
             (fun v1 v2 -> String.compare v1.v_name v2.v_name)
             fn_params_vars)
  then error ~loc:fn_name.loc "duplicated parameter name in function %s" fn_name.id;
  let fn = { fn_name = fn_name.id; fn_params = fn_params_vars } in
  let env_with_fn = { env with funcs = StringMap.add fn_name.id fn env.funcs } in
  let local_env =
    { vars =
        List.fold_left
          (fun vars_map var -> StringMap.add var.v_name var vars_map)
          StringMap.empty
          fn_params_vars
    ; funcs = env_with_fn.funcs
    }
  in
  let tstmt, _ = type_stmt local_env body in
  (fn, tstmt), env_with_fn
;;

let type_file (env : env_t) ((defs, main_stmt) : Ast.file) : Ast.tfile =
  (* type check the functions definitions *)
  let global_env, tdefs =
    List.fold_left
      (fun (env, acc) def ->
        let tdef, env_local = type_def env def in
        env_local, tdef :: acc)
      (env, [])
      defs
  in
  (* type check the main function *)
  let main_fn = { fn_name = "__main"; fn_params = [] } in
  let env = { env with funcs = StringMap.add "__main" main_fn global_env.funcs } in
  let tstmt, _ = type_stmt env main_stmt in
  let main_tdef = main_fn, tstmt in
  List.rev (main_tdef :: tdefs)
;;

let file ?debug:(b = false) (p : Ast.file) : Ast.tfile =
  debug := b;
  try
    let tfile = type_file empty_env p in
    tfile
  with
  | Error (loc, msg) -> raise (Error (loc, msg))
;;