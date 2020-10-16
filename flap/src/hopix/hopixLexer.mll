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


  let dec_of_char c =
    if '0' <= c && c <= '9' then
      int_of_char c - int_of_char '0'
    else assert false

  let hex_of_char c =
    if '0' <= c && c <= '9' then
      int_of_char c - int_of_char '0'
    else if 'a' <= c && c <= 'f' then
      int_of_char c - int_of_char 'a' + 10
    else if 'A' <= c && c <= 'F' then
      int_of_char c - int_of_char 'A' + 10
    else assert false

  let unescape s i =
    if s.[i] <> '\\' then s.[i], i + 1 else
    match s.[i + 1] with
    | 'r' -> '\r', i + 2
    | 'b' -> '\b', i + 2
    | 't' -> '\t', i + 2
    | 'n' -> '\n', i + 2
    | '\'' -> '\'', i + 2
    | '\"' -> '\"', i + 2
    | '\\' -> '\\', i + 2
    | _ ->
      if s.[i + 2] = 'x' then
        char_of_int (16 * hex_of_char s.[i + 3] + hex_of_char s.[i + 4]), i + 5
      else
        char_of_int (100 * dec_of_char s.[i + 1] +
          10 * dec_of_char s.[i + 2] + dec_of_char s.[i + 3]), i + 4

   let unescape_char c = fst (unescape c 0)

   let unescape_string s =
     let rec aux i r =
       if i >= String.length s then r
       else let (c, i) = unescape s i in aux i (c :: r)
     in
     let r = aux 0 [] in
     String.of_seq (List.to_seq (List.rev r))
}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']
let number_dec = "-"? ['0'-'9']+
let number_hexa = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let number_bin = "0b" ['0'-'1']+
let number_oct = "0o" ['0'-'7']+


(** A lower_identifier is an identifier beginning with a lowercase letter *)
let lower_identifier = ['a'-'z']['A'-'Z' '0'-'9' 'a'-'z' '_']*

(** An upper_identifier is an identifier beginning with an uppercase letter *)
let upper_identifier = ['A'-'Z']['A'-'Z' '0'-'9' 'a'-'z' '_']*

let atom = "\\" ['0'-'9'] ['0'-'9'] ['0'-'9']
  | "\\0x" ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
  | [' '-'!']
  | ['#'-'&']
  | ['('-'~']
  | "\\\\"
  | "\\'"
  | "\\n"
  | "\\t"
  | "\\b"
  | "\\r"

rule comment = parse
  | newline       { next_line_and comment lexbuf }
  | eof           { Error.error "during lexing" (Position.cpos lexbuf) "Unterminated comment." }
  | "*/"          { () }
  | "/*"          { comment lexbuf; comment lexbuf }
  | _             { comment lexbuf }

and token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | eof             { EOF       }

  | "//" [^ '\010' '\013']* newline   { next_line_and token lexbuf }
  | "/*"                              { comment lexbuf; token lexbuf }

  (** Keywords *)
  | "fun"     { FUN     }
  | "and"     { AND     }
  | "type"    { TYPE    }
  | "extern"  { EXTERN  }
  | "let"     { LET     }
  | "switch"  { SWITCH  }
  | "if"      { IF      }
  | "else"    { ELSE    }
  | "ref"     { REF     }
  | "while"   { WHILE   }
  | "for"     { FOR     }
  | "in"      { IN      }
  | "to"      { TO      }
  | "do"      { DO      }

  (** Identifiers *)
  | lower_identifier as s       { IDLOW s }
  | ("`" lower_identifier) as s { IDQUOTE s }
  | upper_identifier as s       { IDUP s  }

  (** Integers *)
  | number_dec as s     { INT s }
  | number_hexa as s    { INT s }
  | number_bin as s     { INT s }
  | number_oct as s     { INT s }

  | "'" ((atom | "\"") as c) "'"
    {
      try CHAR (unescape_char c)
      with _ ->
          Error.error "during lexing" (Position.cpos lexbuf) ""
    }
  | "\"" ((atom | "'" | "\\\"")* as s) "\""
    {
      try STRING (unescape_string s)
      with _ ->
          Error.error "during lexing" (Position.cpos lexbuf) ""
    }
  | "\"" ((atom | "'" | "\\\"")* as s) eof
    {
      (* [let pos = Position.cpos lexbuf] gives more precisely located
         information, but is not what is expected by the tests *)
      let pos = Position.lex_join
        (Lexing.lexeme_end_p lexbuf) (Lexing.lexeme_end_p lexbuf) in
      Error.error "during lexing" pos "Unterminated string."
    }

  (** Operators *)
  | "="   { EQUAL       }
  | "*"   { STAR        }
  | "->"  { ARROW       }
  | "."   { DOT         }
  | "+"   { PLUS        }
  | "-"   { MINUS       }
  | "/"   { DIVIDE      }
  | ":="  { ASSIGN      }
  | "&&"  { ANDOP       }
  | "||"  { OROP        }
  | "!"   { READ        }

  (** Comparison *)
  | "=?"  { ISEQUAL     }
  | "<=?" { ISLEQ       }
  | ">=?" { ISGEQ       }
  | "<?"  { ISLT        }
  | ">?"  { ISGT        }

  (** Delimiters *)
  | "("   { LPAREN      }
  | ")"   { RPAREN      }
  | "["   { LBRACKET    }
  | "]"   { RBRACKET    }
  | "{"   { LBRACE      }
  | "}"   { RBRACE      }
  | "<"   { LT          }
  | ">"   { GT          }

  (** Special characters *)
  | "_"   { UNDERSCORE  }
  | ":"   { COLON       }
  | ";"   { SEMICOLON   }
  | ","   { COMMA       }
  | "|"   { BAR         }
  | "&"   { AMPERSAND   }
  | "\\"  { BACKSLASH   }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }
