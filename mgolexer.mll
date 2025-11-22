{

  open Lexing
  open Mgoparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "package",    PACKAGE;
      "import",     IMPORT;
      "type",       TYPE;      
      "struct",     STRUCT;
      "else",       ELSE;
      "true",       TRUE;
      "false",      FALSE;
      "for",        FOR;
      "func",       FUNC;
      "if",         IF;
      "nil",        NIL;
      "return",     RETURN;
      "var",        VAR;   
      "int",        TINT;
      "string",     TSTRING;
      "bool",       TBOOL;
      "fmt",        FMT;
      "Print",      PRINT;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)

        
  let rec hexadecimal_to_int h acc i =
    let hexe_car_to_int c = 
      let code = Char.code c in
      if (102 >= code && code >= 97) then (code - 97 + 10)
      else if (70 >= code && code >= 65) then (code - 65 + 10)
      else if (48 <= code && code <= 57) then (code - 48)
      else raise (Error "ce caractere n'est pas un hexa")
    in
    if String.length h >= i then acc
    else hexadecimal_to_int h (acc*16 + (hexe_car_to_int (String.get h i))) (i+1)

  let entier_to_number s = 
    if String.length s >= 2 && ((String.get s 1) = 'x' || (String.get s 1) = 'X') then 
      Int64.of_int (hexadecimal_to_int s 0 2)
    else Int64.of_string s

  let chaine_to_string s = 
    String.sub s 1 (String.length s - 2)
}


let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let hexa = ['0'-'9'] | ['a' - 'f'] | ['A' - 'F']
let entier = number | (("0x" | "0X") hexa+)
let car = [' ' - '!'] | ['#' - '['] | [']' - '~']
let chaine = '\"' car* '\"'
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "/*"              { comment lexbuf; token lexbuf }
  | "//" [^'\n']*     { token lexbuf }

  | chaine as s       { STRING(chaine_to_string s)}

  | number as n     { try INT(Int64.of_string n)
                   with _ -> raise (Error "literal constant too large")}
  | entier as s  { try INT(entier_to_number s)
                   with _ -> raise (Error "literal constant too large") }
  | ident as id  { keyword_or_ident id }

  | ";"  { SEMI }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "*"  { STAR }
  | "||" { OR }
  | "==" { EQ }
  | "!=" { NEQ }
  | ">"  { GT }
  | ">=" { GE }
  | "<"  { LT }
  | "<=" { LE }
  | "+"  { ADD }
  | "-"  { SUB }
  | "/"  { DIV }
  | "%"  { REM }
  | "!"  { NOT }
  | "."  { DOT }
  | "," { COMA }
  | "++" {ADDADD}
  | "--" {SUBSUB}
  | "=" {SET}
  | ":=" {PSET}

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | '\n' { new_line lexbuf; comment lexbuf }
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
