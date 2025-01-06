open Ast

let is_debug_enabled = ref false

let dummy_location = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

let raise_error ?(loc=dummy_location) fmt =
  Format.kasprintf (fun msg -> raise (Error (loc, msg))) fmt

let debug_log fmt =
  if !is_debug_enabled then
    Format.eprintf (fmt ^^ "@.@.")
  else
    Format.ifprintf Format.err_formatter (fmt ^^ "@.@.")

module StringMap = Map.Make(String)

type environment = {
  variables: var StringMap.t;
  functions: fn StringMap.t;
}

let initial_environment = {
  variables = StringMap.empty;
  functions = StringMap.empty;
}

let lookup_variable (env: environment) (name: string) : var option =
  StringMap.find_opt name env.variables

let annotate_expression (env: environment) (expression: Ast.expr) : Ast.texpr * environment =
  let rec aux env expr =
    match expr with
    | Ecst constant ->
        TEcst constant, env
    | Eident id ->
        (match lookup_variable env id.id with
         | Some variable -> TEvar variable, env
         | None -> raise_error ~loc:id.loc "Unbound variable '%s'" id.id)
    | Ebinop (op, lhs, rhs) ->
        let typed_lhs, env = aux env lhs in
        let typed_rhs, env = aux env rhs in
        TEbinop (op, typed_lhs, typed_rhs), env
    | Eunop (op, operand) ->
        let typed_operand, env = aux env operand in
        TEunop (op, typed_operand), env
    | Ecall (id, args) ->
        let typed_args, env =
          List.fold_right (fun arg (acc, env) ->
            let typed_arg, env = aux env arg in
            (typed_arg :: acc, env))
            args ([], env)
        in
        (match id.id with
         | "len" | "range" ->
             if List.length typed_args <> 1 then
               raise_error ~loc:id.loc "Function '%s' expects 1 argument but got %d" id.id (List.length typed_args);
             (match id.id with
              | "len" -> TEcall ({ fn_name = "len"; fn_params = [] }, typed_args), env
              | "range" -> TErange (List.hd typed_args), env
              | _ -> assert false)
          | "list" ->
            if List.length typed_args <> 1 then
              raise_error ~loc:id.loc "Function 'list' expects exactly 1 argument but got %d" (List.length typed_args);
            (match List.hd typed_args with
              | TEcst (Cstring _)
              | TEcst (Cbool _)
              | TEcst (Cint _) ->
                  raise_error ~loc:id.loc "Function 'list' requires an iterable object, but got a non-iterable constant"
              | _ -> TEcall ({ fn_name = "list"; fn_params = [] }, typed_args), env)
         | _ ->
             let func =
               match StringMap.find_opt id.id env.functions with
               | Some fn -> fn
               | None -> raise_error ~loc:id.loc "Unbound function '%s'" id.id
             in
             if List.length typed_args <> List.length func.fn_params then
               raise_error ~loc:id.loc "Function '%s' expects %d arguments but got %d" id.id (List.length func.fn_params) (List.length typed_args);
             TEcall (func, typed_args), env)
    | Elist elements ->
        let typed_elements, env =
          List.fold_right (fun elem (acc, env) ->
            let typed_elem, env = aux env elem in
            (typed_elem :: acc, env))
            elements ([], env)
        in
        TElist typed_elements, env
    | Eget (collection, index) ->
        let typed_collection, env = aux env collection in
        let typed_index, env = aux env index in
        TEget (typed_collection, typed_index), env
  in
  aux env expression

let annotate_statement (env: environment) (statement: Ast.stmt) : Ast.tstmt * environment =
  let rec aux env stmt =
    match stmt with
    | Sif (condition, then_branch, else_branch) ->
        let typed_condition, env = annotate_expression env condition in
        let typed_then, env = aux env then_branch in
        let typed_else, env = aux env else_branch in
        TSif (typed_condition, typed_then, typed_else), env
    | Sreturn expr ->
        let typed_expr, env = annotate_expression env expr in
        TSreturn typed_expr, env
    | Sassign (id, expr) ->
        let typed_expr, env = annotate_expression env expr in
        (match lookup_variable env id.id with
         | Some variable -> TSassign (variable, typed_expr), env
         | None ->
             let new_var = { v_name = id.id; v_ofs = 0 } in
             let updated_env = { env with variables = StringMap.add id.id new_var env.variables } in
             TSassign (new_var, typed_expr), updated_env)
    | Sprint expr ->
        let typed_expr, env = annotate_expression env expr in
        TSprint typed_expr, env
    | Sblock stmts ->
        let rec annotate_block env = function
          | [] -> [], env
          | stmt :: rest ->
              let typed_stmt, env = aux env stmt in
              let typed_rest, env = annotate_block env rest in
              (typed_stmt :: typed_rest, env)
        in
        let typed_stmts, env = annotate_block env stmts in
        TSblock typed_stmts, env
    | Sfor (id, collection, body) ->
        let typed_collection, env = annotate_expression env collection in
        let loop_var = { v_name = id.id; v_ofs = 0 } in
        let updated_env = { env with variables = StringMap.add id.id loop_var env.variables } in
        let typed_body, env = aux updated_env body in
        TSfor (loop_var, typed_collection, typed_body), env
    | Seval expr ->
        let typed_expr, env = annotate_expression env expr in
        TSeval typed_expr, env
    | Sset (collection, index, value) ->
        let typed_collection, env = annotate_expression env collection in
        let typed_index, env = annotate_expression env index in
        let typed_value, env = annotate_expression env value in
        TSset (typed_collection, typed_index, typed_value), env
  in
  aux env statement

let annotate_definition (env: environment) (fn_name, params, body) : Ast.tdef * environment =
  let parameter_vars = List.map (fun param -> { v_name = param.id; v_ofs = 0 }) params in
  if List.length parameter_vars <> List.length (List.sort_uniq (fun v1 v2 -> String.compare v1.v_name v2.v_name) parameter_vars) then
    raise_error ~loc:fn_name.loc "Duplicate parameter name in function '%s'" fn_name.id;
  let function_record = { fn_name = fn_name.id; fn_params = parameter_vars } in
  let env_with_function = { env with functions = StringMap.add fn_name.id function_record env.functions } in
  let local_env = {
    variables = List.fold_left (fun acc var -> StringMap.add var.v_name var acc) StringMap.empty parameter_vars;
    functions = env_with_function.functions;
  } in
  let typed_body, _ = annotate_statement local_env body in
  (function_record, typed_body), env_with_function

let annotate_file (env: environment) (definitions, main) : Ast.tfile =
  let updated_env, typed_defs =
    List.fold_left (fun (env, acc) def ->
      let typed_def, updated_env = annotate_definition env def in
      updated_env, typed_def :: acc
    ) (env, []) definitions
  in
  let main_function = { fn_name = "main"; fn_params = [] } in
  let env_with_main = { updated_env with functions = StringMap.add "main" main_function updated_env.functions } in
  let typed_main, _ = annotate_statement env_with_main main in
  List.rev ((main_function, typed_main) :: typed_defs)

let file ?(debug=false) (parsed_file: Ast.file) : Ast.tfile =
  is_debug_enabled := debug;
  try
    annotate_file initial_environment parsed_file
  with
  | Error (loc, msg) -> raise (Error (loc, msg))
