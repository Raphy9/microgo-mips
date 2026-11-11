%{
  open Mgoast

  let mk_expr d s e = { edesc = d; eloc = (s, e) }
  let mk_instr d s e = { idesc = d; iloc = (s, e) }

  let expr_to_ident e =
    match e.edesc with
    | Var id -> id
    | _ -> failwith "Syntax error: Expected identifier on the left side of :="
%}

%token <int64> INT
%token <string> IDENT
%token <string> STRING

%token PACKAGE IMPORT TYPE STRUCT FUNC RETURN VAR
%token IF ELSE FOR NIL TRUE FALSE
%token NEW 

%token LPAR RPAR BEGIN END SEMI COMMA DOT EOF

%token PLUS MINUS STAR SLASH PERCENT      
%token ANDAND OROR NOT                    
%token EQEQ NEQ LT LE GT GE               
%token ASSIGN DECL PLUSEQ                 
%token PLUSPLUS MINUSMINUS                

%left OROR
%left ANDAND
%left EQEQ NEQ LT LE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc UMINUS NOT
%left DOT

%start prog
%type <Mgoast.program> prog

%%


prog:
| PACKAGE main=IDENT SEMI decls=list(decl) EOF
    { if main="main" then (false, decls) else failwith "Package name must be 'main'" }
| PACKAGE main=IDENT SEMI IMPORT fmt=STRING SEMI decls=list(decl) EOF
    { if main="main" && fmt="fmt" then (true, decls) else failwith "Package must be 'main' and import 'fmt'" }
;

decl:
| s=struct_decl { Struct s }
| f=func_decl   { Fun f }
;

struct_decl:
| TYPE id=ident STRUCT BEGIN fields=flattened_fields END SEMI
    { { sname = id; fields = fields } }
;

func_decl:
| FUNC id=ident LPAR params=flattened_params RPAR rets=return_types b=block SEMI
    { { fname = id; params = params; return = rets; body = b } }
| FUNC id=ident LPAR params=flattened_params RPAR rets=return_types b=block
    { { fname = id; params = params; return = rets; body = b } };

flattened_fields:
| fs = list(field_line) { List.flatten fs }
;

field_line:
| ids=separated_nonempty_list(COMMA, ident) t=typ SEMI 
    { List.map (fun id -> (id, t)) ids }
| ids=separated_nonempty_list(COMMA, ident) t=typ
    { List.map (fun id -> (id, t)) ids }
;

flattened_params:
| l=separated_list(COMMA, param_group) { List.flatten l }
;

param_group:
| ids=separated_nonempty_list(COMMA, ident) t=typ 
    { List.map (fun id -> (id, t)) ids }
;

return_types:
| { [] }
| t=typ { [t] }
| LPAR t = typs COMMA RPAR { t }
| LPAR t =  typs RPAR { t }
;

typs:
| ts = separated_nonempty_list(COMMA, typ) { ts }
;

ident:
| id=IDENT { { loc = ($startpos, $endpos); id = id } }
;

typ:
| IDENT { match $1 with
          | "int" -> TInt 
          | "bool" -> TBool 
          | "string" -> TString 
          | s -> failwith ("Unknown type: " ^ s) }
| STAR id=IDENT { TStruct id }
;

block:
| BEGIN seq=block_content END { seq }
;

block_content:
    { [] }
| SEMI rest=block_content 
    { rest }
| s=stmt SEMI rest=block_content 
    { s :: rest }
| s=stmt rest=block_content 
    { s :: rest }
;

stmt:
| VAR ids=separated_nonempty_list(COMMA, ident) t=typ
    { mk_instr (Vars(ids, Some t, [])) $startpos $endpos }
| VAR ids=separated_nonempty_list(COMMA, ident) t=option(typ) ASSIGN es=separated_nonempty_list(COMMA, expr)
    { 
      let set_instr = mk_instr (Set(List.map (fun id -> mk_expr (Var id) (fst id.loc) (snd id.loc)) ids, es)) $startpos $endpos in
      mk_instr (Vars(ids, t, [set_instr])) $startpos $endpos 
    }

| i=if_instr     { i }
| i=for_instr    { i }
| b=block        { mk_instr (Block b) $startpos $endpos }
| RETURN es=separated_list(COMMA, expr) { mk_instr (Return es) $startpos $endpos }

| es=separated_nonempty_list(COMMA, expr) suffix=simple_stmt_suffix
    { suffix es }
;

simple_stmt_suffix:
| ASSIGN rhs=separated_nonempty_list(COMMA, expr)
    { fun lhs -> mk_instr (Set(lhs, rhs)) $startpos $endpos }
| DECL rhs=separated_nonempty_list(COMMA, expr)
    { fun lhs ->
      let ids = List.map expr_to_ident lhs in
      let set_instr = mk_instr (Set(lhs, rhs)) $startpos $endpos in
      mk_instr (Vars(ids, None, [set_instr])) $startpos $endpos
    }
| PLUSPLUS
    { fun lhs ->
      match lhs with [e] -> mk_instr (Inc e) $startpos $endpos | _ -> failwith "Invalid ++" }
| MINUSMINUS
    { fun lhs ->
      match lhs with [e] -> mk_instr (Dec e) $startpos $endpos | _ -> failwith "Invalid --" }
| PLUSEQ rhs=expr
    { fun lhs ->
      match lhs with
      | [e1] ->
          let add = mk_expr (Binop(Add, e1, rhs)) $startpos $endpos in
          mk_instr (Set([e1], [add])) $startpos $endpos
      | _ -> failwith "Invalid +="
    }
| 
    { fun lhs ->
      match lhs with [e] -> mk_instr (Expr e) $startpos $endpos | _ -> failwith "Invalid expression stmt" }
;


if_instr:
| IF e=expr b1=block ELSE b2=block_or_if
    { mk_instr (If(e, b1, b2)) $startpos $endpos }
| IF e=expr b1=block
    { mk_instr (If(e, b1, [])) $startpos $endpos }
;

block_or_if:
| b=block { b }
| i=if_instr { [i] }
;

for_instr:
| FOR b=block
    { mk_instr (For(mk_expr (Bool true) $startpos $endpos, b)) $startpos $endpos }
| FOR e=expr b=block
    { mk_instr (For(e, b)) $startpos $endpos }
| FOR init=for_init SEMI cond=option(expr) SEMI step=option(simple_instr_factorized) b=block
    { 
      let cond_expr = match cond with None -> mk_expr (Bool true) $startpos $endpos | Some e -> e in
      let body_with_step = match step with None -> b | Some s -> b @ [s] in
      let loop_instr = mk_instr (For(cond_expr, body_with_step)) $startpos $endpos in

      match init with
      | None -> loop_instr
      | Some i -> 
          match i.idesc with
          | Vars(ids, typ, body_init) ->
               mk_instr (Vars(ids, typ, body_init @ [loop_instr])) $startpos $endpos
          | _ -> 
               mk_instr (Block [i; loop_instr]) $startpos $endpos
    }
;

for_init:
|  { None }
| i=simple_instr_factorized { Some i }
;

simple_instr_factorized:
| es=separated_nonempty_list(COMMA, expr) suffix=simple_stmt_suffix
    { suffix es }
;


expr:
| e=expr_desc { mk_expr e $startpos $endpos }
;

expr_desc:
| n=INT           { Int n }
| s=STRING        { String s }
| TRUE            { Bool true }
| FALSE           { Bool false }
| NIL             { Nil }
| id=ident        { Var id }

| NEW LPAR id=IDENT RPAR { New id } 

| e=expr DOT id=ident { Dot(e, id) }

| e=expr LPAR args=separated_list(COMMA, expr) RPAR 
    { 
      match e.edesc with
      | Var id -> Call(id, args) 
      | Dot ({edesc=Var {id="fmt";_};_}, {id="Print";_}) -> Print(args) 
      | _ -> failwith "Syntax error: Invalid function call or unsupported expression call"
    }

| MINUS e=expr %prec UMINUS { Unop(Opp, e) }
| NOT e=expr                { Unop(Not, e) }

| e1=expr PLUS e2=expr   { Binop(Add, e1, e2) }
| e1=expr MINUS e2=expr  { Binop(Sub, e1, e2) }
| e1=expr STAR e2=expr   { Binop(Mul, e1, e2) }
| e1=expr SLASH e2=expr  { Binop(Div, e1, e2) }
| e1=expr PERCENT e2=expr { Binop(Rem, e1, e2) }

| e1=expr EQEQ e2=expr   { Binop(Eq, e1, e2) }
| e1=expr NEQ e2=expr    { Binop(Neq, e1, e2) }
| e1=expr LT e2=expr     { Binop(Lt, e1, e2) }
| e1=expr LE e2=expr     { Binop(Le, e1, e2) }
| e1=expr GT e2=expr     { Binop(Gt, e1, e2) }
| e1=expr GE e2=expr     { Binop(Ge, e1, e2) }

| e1=expr ANDAND e2=expr { Binop(And, e1, e2) }
| e1=expr OROR e2=expr   { Binop(Or, e1, e2) }

| LPAR e=expr_desc RPAR  { e }
;