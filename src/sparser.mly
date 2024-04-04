%{ open Ast %}

%token SPACE TAB EOL
%token ASSIGN PLUS MINUS TIMES DIVIDE MOD
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE SEMI COMMA
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token IF ELSE ELIF WHILE FOR RETURN BREAK CONTINUE 
%token DEF PRINT RANGE ASSERT
%token INT BOOL
%token <bool> BLIT
%token <int> INT_LITERAL
%token <string> VARIABLE
%token EOF

%start program
%type <Ast.tokenseq> program 

%%
program:
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
 | PRINT { "PRINT" }
 | RANGE { "RANGE" }
 | ASSERT { "ASSERT" }
 | BOOL { "BOOL" }
 | INT { "INT" }
 | VARIABLE { "VARIABLE: " ^ $1}
 | BLIT { "BOOL: " ^ string_of_bool $1}
 | INT_LITERAL { "INT_LITERAL: " ^ string_of_int $1}