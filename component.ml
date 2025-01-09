open Ast
module StringMap = Map.Make (String)


let byte = 8


type env_t =
  { mutable vars : int StringMap.t  
  ; funcs : fn StringMap.t          
  ; mutable stack_offset : int     
  ; mutable counters : int StringMap.t 
  }


let create_env () : env_t =
  { vars = StringMap.empty
  ; funcs = StringMap.empty
  ; stack_offset = 0
  ; counters = StringMap.empty
  }