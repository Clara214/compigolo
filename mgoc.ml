open Format
open Lexing
    
let usage = "usage: mgoc [options] file.go"

let parse_only = ref false
let type_only = ref true
    
let spec =
  [ "--parse-only", Arg.Set parse_only, "  stops after parsing";
    "--type-only", Arg.Set type_only, "  stops after typing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".go") then
      raise (Arg.Bad "no .go extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol in
  let lc = e.pos_cnum - b.pos_bol in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Mgoparser.prog Mgolexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    let f = Typechecker.prog  f in
    if !type_only then exit 0;

  with
    | Mgolexer.Error s ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s\n@." s;
	exit 1
    | Mgoparser.Error ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error\n@.";
	exit 1
    | Typechecker.Error (l, msg) ->
	report_loc l;
	eprintf "error: %s\n@." msg;
	exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2


