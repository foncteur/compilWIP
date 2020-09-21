{ (* -*- tuareg -*- *)
  open Lexing
  open Error
  open Position
  open HopixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)


}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']
let number = "-"? ['0'-'9']+


(** A lower_identifier is an identifier beginning with a lowercase letter *)
let lower_identifier = ['a'-'z']['A'-'Z' '0'-'9' 'a'-'z' '_']*

(** An upper_identifier is an identifier beginning with an uppercase letter *)
let upper_identifier = ['A'-'Z']['A'-'Z' '0'-'9' 'a'-'z' '_']*


rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | eof             { EOF       }

  (** Identifiers *)
  | lower_identifier as s {IDLOW s}
  | upper_identifier as s {IDUP s}

  (** Keywords *)
  | "fun" {FUN}
  | "and" {AND}

  (** Operators *)
  | "=" {EQUAL}
  | "_" {UNDERSCORE}
  | "'" {QUOTE}
  | ":" {COLON}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "[" {LBRACKET}
  | "]" {RBRACKET}
  | "<" {LT}
  | ">" {GT}
  | "," {COMMA}
  | "->" {ARROW}
  | "*" {STAR}

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }
