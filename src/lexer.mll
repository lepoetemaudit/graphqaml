{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

rule read =
    parse [' ' '\t' '\n'] { read lexbuf }
    | '{'   { LEFT_BRACE }
    | '}'    { RIGHT_BRACE }
    | ['a'-'z']* { IDENTIFIER (Lexing.lexeme lexbuf) }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
