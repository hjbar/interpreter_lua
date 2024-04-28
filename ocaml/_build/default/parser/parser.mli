
(* The type of tokens. *)

type token = 
  | WHILE
  | VERTICALBAR
  | UNTIL
  | TRUE
  | TRIPLEDOT
  | TILDAEQUAL
  | TILDA
  | THEN
  | SLASH
  | SHARP
  | SEMICOLON
  | RPAREN
  | RETURN
  | REPEAT
  | RBRACKET
  | RBRACE
  | PLUS
  | PERCENT
  | OR
  | NOT
  | NIL
  | NAME of (Internalast.name)
  | LTEQ
  | LT
  | LPAREN
  | LOCAL
  | LITERALSTRING of (string)
  | LBRACKET
  | LBRACE
  | INTEGER of (Int64.t)
  | IN
  | IF
  | HYPHEN
  | HAT
  | GTEQ
  | GT
  | GOTO
  | FUNCTION
  | FOR
  | FLOAT of (float)
  | FALSE
  | EQUAL
  | EOF
  | END
  | ELSEIF
  | ELSE
  | DOUBLESLASH
  | DOUBLELT
  | DOUBLEGT
  | DOUBLEEQUAL
  | DOUBLEDOT
  | DOUBLECOLON
  | DOT
  | DO
  | COMMA
  | COLON
  | BREAK
  | ASTERISK
  | AND
  | AMPERSAND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val chunk: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Internalast.block)
