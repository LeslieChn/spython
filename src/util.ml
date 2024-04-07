type tokenseq = string list

let string_of_program l =
  "\n\nScanned program: \n" ^ (List.fold_left (fun s e -> s ^ "\n" ^ e) "" l)

/* token stream start */
token_stream:
  tokens EOF { $1 }

tokens:
   /* nothing */ { [] }
 | one_token tokens { $1 :: $2 }

one_token:
 | SPACE { "SPACE" }
 | TAB { "TAB" }
 | EOL { "EOL" }
 | ASSIGN { "ASSIGN" }
 | PLUS { "PLUS" }
 | MINUS { "MINUS" }
 | TIMES { "TIMES" }
 | DIVIDE { "DIVIDE" }
 | MOD { "MOD" }
 | EXP { "EXP" }
 | PLUSEQ { "PLUSEQ" }
 | MINUSEQ { "MINUSEQ" }
 | TIMESEQ { "TIMESEQ" }
 | DIVIDEEQ { "DIVIDEEQ" }
 | MODEQ { "MODEQ" }
 | EXPEQ { "EXPEQ" }
 | LPAREN { "LPAREN" }
 | RPAREN { "RPAREN" }
 | LBRACK { "LBRACK" }
 | RBRACK { "RBRACK" }
 | LBRACE { "LBRACE" }
 | RBRACE { "RBRACE" }
 | SEMI { "SEMI" }
 | COMMA { "COMMA" }
 | EQ { "EQ" }
 | NEQ { "NEQ" }
 | LT { "LT" }
 | LEQ { "LEQ" }
 | GT { "GT" }
 | GEQ { "GEQ" }
 | AND { "AND" }
 | OR { "OR" }
 | NOT { "NOT" }
 | IF { "IF" }
 | ELSE { "ELSE" }
 | ELIF { "ELIF" }
 | WHILE { "WHILE" }
 | FOR { "FOR" }
 | RETURN { "RETURN" }
 | BREAK { "BREAK" }
 | CONTINUE { "CONTINUE" }
 | DEF { "DEF" }
 | ARROW { "ARROW" }
 | COLON { "COLON" }
 | PRINT { "PRINT" }
 | RANGE { "RANGE" }
 | PASS { "PASS" }
 | ASSERT { "ASSERT" }
 | BOOL { "BOOL" }
 | INT { "INT" }
 | STRING { "STR" } | VARIABLE { "VARIABLE: " ^ $1} | BLIT { "BOOL: " ^ string_of_bool $1} | INT_LITERAL { "INT_LITERAL: " ^ string_of_int $1} | FLOAT_LITERAL { "FLOAT_LITERAL: " ^ string_of_float $1}
 | STRING_LITERAL { "STRING_LITERAL: " ^ $1}
/* token stream end */
