
(** {2 Abstract Syntax of Mini-Python} *)

(** {3 Parsed trees}

   This is the output of the parser and the input of the type checker. *)

type location = Lexing.position * Lexing.position

type ident = { loc: location; id: string; }

type unop =
  | Uneg (** -e *)
  | Unot (** not e *)

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (** + - * // % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (** == != < <= > >= *)
  | Band | Bor                          (** and or *)

type constant =
  | Cnone
  | Cbool of bool
  | Cstring of string
  | Cint of int64

type expr =
  | Ecst of constant
  | Eident of ident
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Ecall of ident * expr list
  | Elist of expr list (** {[ [e1,e2,...] ]} *)
  | Eget of expr * expr (** {[ e1[e2] ]} *)

and stmt =
  | Sif of expr * stmt * stmt
  | Sreturn of expr
  | Sassign of ident * expr
  | Sprint of expr
  | Sblock of stmt list
  | Sfor of ident * expr * stmt
  | Seval of expr
  | Sset of expr * expr * expr (** {[ e1[e2] = e3 ]} *)

and def = ident * ident list * stmt

and file = def list * stmt

(** {3 Typed trees}

   This is the output of the type checker and the input of the code
   generation. *)

(** In the typed trees, all the occurrences of the same variable
   point to a single record of the following type. *)
type var = {
  v_name: string;
  mutable v_ofs: int; (** position wrt %rbp *)
}

(** Similarly, all the occurrences of a given function all point
   to a single record of the following type. *)
type fn = {
  fn_name: string;
  fn_params: var list;
}

type texpr =
  | TEcst of constant
  | TEvar of var
  | TEbinop of binop * texpr * texpr
  | TEunop of unop * texpr
  | TEcall of fn * texpr list
  | TElist of texpr list
  | TErange of texpr (** list(range(e1)) *)
  | TEget of texpr * texpr (** {[ e1[e2] ]} *)

type tstmt =
  | TSif of texpr * tstmt * tstmt
  | TSreturn of texpr
  | TSassign of var * texpr
  | TSprint of texpr
  | TSblock of tstmt list
  | TSfor of var * texpr * tstmt
  | TSeval of texpr
  | TSset of texpr * texpr * texpr (** {[ e1[e2] = e3 ]} *)

and tdef = fn * tstmt

and tfile = tdef list
  (** the block of global statements is now a `main` function *)
