
type typ =
  | TInt
  | TBool
  | TString
  | TStruct of string

let typ_to_string = function
  | TInt     -> "int"
  | TBool    -> "bool"
  | TString  -> "string"
  | TStruct c -> ("*"^c)

type unop  = Opp | Not
type binop = Add | Sub | Mul | Div | Rem
           | Lt  | Le  | Gt | Ge | Eq  | Neq
           | And | Or

type location = Lexing.position * Lexing.position
type ident = { loc : location; id : string }

type expr =  { edesc : expr_desc; eloc  : location; }
and expr_desc = 
  | Int    of int64
  | Bool   of bool
  | String of string
  | Unop   of unop * expr
  | Binop  of binop * expr * expr
  | Var    of ident
  | Dot    of expr * ident
  | Nil
  | New  of string
  | Call of ident * expr list
  | Print  of expr list
  
type instr = { idesc : instr_desc; iloc  : location; }
and instr_desc = 
  | Set    of (expr list) * (expr list)
  | Inc    of expr
  | Dec    of expr
  | If     of expr * seq * seq
  | For  of expr * seq
  | Block of seq
  | Vars of ident list * typ option * seq
  | Return of expr list
  | Expr   of expr

and seq = instr list

type func_def = {
    fname: ident;
    params: (ident * typ) list;
    return: typ list;
    body: seq;
  }
        
type struct_def = {
    sname: ident;
    fields: (ident * typ) list;
  }
  
type decl =
  | Fun of func_def
  | Struct  of struct_def

type program = bool * decl list
