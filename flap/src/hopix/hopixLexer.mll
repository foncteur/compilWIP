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
let number_dec = "-"? ['0'-'9']+
let number_hexa = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let number_bin = "Ob" ['0'-'1']+
let number_oct = "0o" ['0'-'7']+


(** A lower_identifier is an identifier beginning with a lowercase letter *)
let lower_identifier = ['a'-'z']['A'-'Z' '0'-'9' 'a'-'z' '_']*

(** An upper_identifier is an identifier beginning with an uppercase letter *)
let upper_identifier = ['A'-'Z']['A'-'Z' '0'-'9' 'a'-'z' '_']*

let atom = "\\" ['0'-'1'] ['0'-'9'] ['0'-'9']
  | "\\2" ['0'-'4'] ['0'-'9']
  | "\\25" ['0'-'5']
  | "\\0x" ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
  | [' '-'!']
  | ['#'-'~']
  | "\\\\"
  | "\\'"
  | "\\n"
  | "\\t"
  | "\\b"
  | "\\r"

let char = "'" (atom | "\"") "'"
let string = "\"" (atom | "'" | "\\\"")* "\""


rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | eof             { EOF       }

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

  (** Identifiers *)
  | lower_identifier as s   { IDLOW s }    
  | upper_identifier as s   { IDUP s  }

  (** Integers *)
  | number_dec as s     { INT s }
  | number_hexa as s    { INT s }
  | number_bin as s     { INT s }
  | number_oct as s     { INT s }

  | char as c     { CHAR c    }
  | string as s   { STRING s  }

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

  
  | "_"   { UNDERSCORE  }
  | "`"   { QUOTE       }
  | ":"   { COLON       }
  | ";"   { SEMICOLON   }
  | ","   { COMMA       }   
  | "|"   { BAR         }
  | "&"   { AMPERSAND   }
  | "\\"  { BACKSLASH   }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }
