{
  open Lexing
  open Mgoparser

  exception Error of string

  let should_insert_semi = ref false

  let ret tok =
    should_insert_semi := (match tok with
      | IDENT _ | INT _ | STRING _ 
      | TRUE | FALSE | NIL | RETURN 
      | PLUSPLUS | MINUSMINUS 
      | RPAR | END  -> true
      | _ -> false);
    tok

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "package",    PACKAGE;
        "import",     IMPORT;
        "type",       TYPE;
        "struct",     STRUCT;
        "func",       FUNC;
        "return",     RETURN;
        "var",        VAR;
        "if",         IF;
        "else",       ELSE;
        "for",        FOR;
        "nil",        NIL;
        "true",       TRUE;
        "false",      FALSE;
        "new",        NEW;
      ] ;
    fun s ->
      try  ret (Hashtbl.find h s)
      with Not_found -> ret (IDENT(s))
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let hexa  = ['0'-'9' 'a'-'f' 'A'-'F']
let integer = digit+ | ("0x" | "0X") hexa+

let string_char = ['\032'-'\126'] # ['\\' '"']
let string_escape = '\\' ['\\' '"' 'n' 't']

rule token = parse
  | ['\n']            
      { new_line lexbuf;
        if !should_insert_semi then (should_insert_semi := false; SEMI)
        else token lexbuf }
        
  | [' ' '\t' '\r']+  { token lexbuf }

  | "/*"              { comment lexbuf; token lexbuf }
  | "//" [^ '\n']* { token lexbuf }

  | '"' ((string_char | string_escape)* as s) '"' 
      { ret (STRING (Scanf.unescaped s)) }

  | integer as n  { try 
                      ret (INT(Int64.of_string n))
                    with Failure _ -> raise (Error "literal constant too large") }

  | ident as id   { keyword_or_ident id }

  | ";"   { should_insert_semi := false; SEMI }
  | ","   { ret COMMA }
  | "."   { ret DOT }
  | "("   { ret LPAR }
  | ")"   { ret RPAR }
  | "{"   { ret BEGIN }
  | "}"   { ret END }
  
  | "+"   { ret PLUS }
  | "-"   { ret MINUS }
  | "*"   { ret STAR }
  | "/"   { ret SLASH }
  | "%"   { ret PERCENT }
  | "&&"  { ret ANDAND }
  | "||"  { ret OROR }
  | "!"   { ret NOT }
  | "=="  { ret EQEQ }
  | "!="  { ret NEQ }
  | "<"   { ret LT }
  | "<="  { ret LE }
  | ">"   { ret GT }
  | ">="  { ret GE }
  | "="   { ret ASSIGN }
  | ":="  { ret DECL }
  | "++"  { ret PLUSPLUS }
  | "--"  { ret MINUSMINUS }
  | "+="  { ret PLUSEQ }

  | _     { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  
  | eof   
      {
        if !should_insert_semi then (should_insert_semi := false; SEMI)
        else EOF }

and comment = parse
  | '\n' { new_line lexbuf; comment lexbuf }
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }