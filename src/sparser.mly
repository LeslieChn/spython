%{ open Ast %}

%token INT ASSIGN SPACE TAB EOL
%token <int> INT_LITERAL
%token <string> VARIABLE
%token EOF

%start program
%type <Ast.tokenseq> program 

%%
program:
  tokens EOF { $1}

tokens:
   /* nothing */ { [] }
 | one_token tokens { $1 :: $2 }
 
one_token:
 | INT { "INT" }
 | ASSIGN { "ASSIGN" } 
 | VARIABLE { "VARIABLE" ^ $1}
 | INT_LITERAL { "INT_LITERAL" ^ string_of_int $1}