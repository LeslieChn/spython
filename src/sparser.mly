%{ open Ast %}

%token SPACE TAB EOL
%token ASSIGN PLUS MINUS TIMES DIVIDE MOD EXP PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODEQ EXPEQ
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE SEMI COMMA
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token IF ELSE ELIF WHILE FOR RETURN BREAK CONTINUE 
%token DEF ARROW COLON PRINT RANGE PASS ASSERT
%token INT BOOL
%token <bool> BLIT
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> VARIABLE
%token EOF

%start token_stream
%type <Ast.tokenseq> token_stream

%start program_rule
%type <Ast.program> program_rule

%right ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODEQ 
%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right EXP EXPEQ
%right NOT

%%

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
 | VARIABLE { "VARIABLE: " ^ $1}
 | BLIT { "BOOL: " ^ string_of_bool $1}
 | INT_LITERAL { "INT_LITERAL: " ^ string_of_int $1}
 | FLOAT_LITERAL { "FLOAT_LITERAL: " ^ string_of_float $1}
 | STRING_LITERAL { "STRING_LITERAL: " ^ $1}

/* token stream end */

/* ast start */
program_rule:
  decls EOF { $1 }

decls:
 /* nothing */ { ([], [])                 }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

/* ast end */
