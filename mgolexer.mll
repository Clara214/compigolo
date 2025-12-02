{

  open Lexing
  open Mgoparser

  exception Error of string

  let b = ref false

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "package",    PACKAGE;
      "import",     IMPORT;
      "type",       TYPE;      
      "struct",     STRUCT;
      "else",       ELSE;
      "for",        FOR;
      "func",       FUNC;
      "if",         IF;
      "var",        VAR;   
      "int",        TINT;
      "string",     TSTRING;
      "bool",       TBOOL;
      "fmt",        FMT;
      "Print",      PRINT;
      "new",        NEW;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> b := true ; IDENT(s)

        
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
let echap  = '\\' ['n' '\\' '"']            (*modif ici attention, fait à la main couvre suremen pas tous les cas ?*)
let chaine  = '\"' (car | echap)* '\"'

  
rule token = parse
  | [' ' '\t' '\r']* '\n'  
    { new_line lexbuf;
      if !b then 
        let () = b := false in
        SEMI 
      else token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "/*"              { comment lexbuf; token lexbuf }
  | "//" [^'\n']*     { token lexbuf }

  | chaine as s       { b := true; STRING(chaine_to_string s)}

  | entier as s  { b := true; 
                   try INT(entier_to_number s)
                   with _ -> raise (Error "literal constant too large") }
  
  | "true" {b := true; TRUE}
  | "false" {b := true; FALSE}
  | "return" {b := true; RETURN}
  | "nil" {b := true; NIL}

  | ident as id  { b := false; keyword_or_ident id }

  | ";"  { b := false; SEMI  }
  | "("  { b := false; LPAR  }
  | ")"  { b := true ; RPAR  }
  | "{"  { b := false; BEGIN }
  | "}"  { b := true ; END   }
  | "*"  { b := false; STAR  }
  | "&&" { b := false; AND   }
  | "||" { b := false; OR    }
  | "==" { b := false; EQ    }
  | "!=" { b := false; NEQ   }
  | ">"  { b := false; GT    }
  | ">=" { b := false; GE    }
  | "<"  { b := false; LT    }
  | "<=" { b := false; LE    }
  | "+"  { b := false; ADD   }
  | "-"  { b := false; SUB   }
  | "/"  { b := false; DIV   }
  | "%"  { b := false; REM   }
  | "!"  { b := false; NOT   }
  | "."  { b := false; DOT   }
  | ","  { b := false; COMA  }
  | "++" {b := true  ; ADDADD}
  | "--" {b := true  ; SUBSUB}
  | "="  {b := false; SET    }
  | ":=" {b := false; PSET   }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | '\n' { new_line lexbuf; comment lexbuf }
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
