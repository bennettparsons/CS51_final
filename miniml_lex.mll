(* MiniML: a subset  of ML
   Lexical analyzer
   Stuart M. Shieber
   November 18, 2015
 *)

{
  open Printf ;;
  open Miniml_parse ;; (* need access to parser's token definitions *)

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keyword_table = 
    create_hashtable 8 [
		       ("if", IF);
		       ("in", IN);
		       ("then", THEN);
		       ("else", ELSE);
		       ("let", LET);
		       ("raise", RAISE);
		       ("rec", REC);
		       ("true", TRUE);
		       ("false", FALSE);
		       ("lambda", FUNCTION);
		       ("fun", FUNCTION);
		       ("function", FUNCTION);
		       ("not", NOT)
		     ]
		     
  let sym_table = 
    create_hashtable 8 [
		       ("=", EQUALS);
		       ("<", LESSTHAN);
		       (">", GREATER);
		       ("<>", EXOR);
		       (".", DOT);
		       ("->", DOT);
		       (";;", EOF);
		       ("~", NEG);
		       ("+", PLUS);
		       ("-", MINUS);
		       ("*", TIMES);
		       ("/", DIVIDE);
		       ("(", OPEN);
		       (")", CLOSE)
		     ]
}

let digit = ['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let sym = ['(' ')'] | (['+' '-' '*' '/' '.' '=' '~' ';' '<' '>' ]+)

rule token = parse
  | digit+ as inum
  	{ let num = int_of_string inum in
	  INT num
	}
  | id as word
  	{ try
	    let token = Hashtbl.find keyword_table word in
	    token 
	  with Not_found ->
	    ID word
	}
  | sym as symbol
	{ try
	    let token = Hashtbl.find sym_table symbol in
	    token
	  with Not_found ->
	    token lexbuf
	}
  | '{' [^ '\n']* '}'	{ token lexbuf }    (* skip one-line comments *)
  | [' ' '\t' '\n']	{ token lexbuf }    (* skip whitespace *)
  | _ as c                                  (* warn and skip unrecognized characters *)
  	{ printf "Unrecognized character: %c\n" c;
	  token lexbuf
	}
  | eof
        { raise End_of_file }
