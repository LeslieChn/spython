%{ 
    open Ast
%}

%token SPACE TAB EOL
%token ASSIGN PLUS MINUS TIMES DIVIDE MOD EXP PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODEQ EXPEQ
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE SEMI COMMA
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token IF ELSE ELIF WHILE FOR RETURN BREAK CONTINUE 
%token DEF ARROW COLON PRINT RANGE PASS ASSERT
%token BOOL INT FLOAT STRING
%token <bool> BOOL_LITERAL
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> VARIABLE
%token EOF

%start token_seq
%type <token list> token_seq

%start program_rule
%type <Ast.stmt list> program_rule

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
token_seq:
  tokens EOF { $1 }

tokens:
   /* nothing */ { [] }
 | token tokens { $1 :: $2 }

token:
   SPACE { SPACE }
 | TAB { TAB }
 | EOL { EOL }
 | ASSIGN { ASSIGN }
 | PLUS { PLUS }
 | MINUS { MINUS }
 | TIMES { TIMES }
 | DIVIDE { DIVIDE }
 | MOD { MOD }
 | EXP { EXP }
 | PLUSEQ { PLUSEQ }
 | MINUSEQ { MINUSEQ }
 | TIMESEQ { TIMESEQ }
 | DIVIDEEQ { DIVIDEEQ }
 | MODEQ { MODEQ }
 | EXPEQ { EXPEQ }
 | LPAREN { LPAREN }
 | RPAREN { RPAREN }
 | LBRACK { LBRACK }
 | RBRACK { RBRACK }
 | LBRACE { LBRACE }
 | RBRACE { RBRACE }
 | SEMI { SEMI }
 | COMMA { COMMA }
 | EQ { EQ }
 | NEQ { NEQ }
 | LT { LT }
 | LEQ { LEQ }
 | GT { GT }
 | GEQ { GEQ }
 | AND { AND }
 | OR { OR }
 | NOT { NOT }
 | IF { IF }
 | ELSE { ELSE }
 | ELIF { ELIF }
 | WHILE { WHILE }
 | FOR { FOR }
 | RETURN { RETURN }
 | BREAK { BREAK }
 | CONTINUE { CONTINUE }
 | DEF { DEF }
 | ARROW { ARROW }
 | COLON { COLON }
 | PRINT { PRINT }
 | RANGE { RANGE }
 | PASS { PASS }
 | ASSERT { ASSERT }
 | BOOL { BOOL }
 | INT { INT }
 | FLOAT { FLOAT }
 | STRING { STRING } 
 | VARIABLE { VARIABLE $1 } 
 | BOOL_LITERAL { BOOL_LITERAL $1 }
 | INT_LITERAL { INT_LITERAL $1}
 | FLOAT_LITERAL { FLOAT_LITERAL $1}
 | STRING_LITERAL { STRING_LITERAL $1}

/* token stream end */

/* ast start */

program_rule:
  stmt_list EOF { List.rev $1 }

stmt_list:
  | { [] }
  | stmt_list stmt_rule { $2 :: $1 }

stmt_rule:
  | expr_rule SEMI				{ Expr $1 }
  | PRINT LPAREN expr_rule RPAREN SEMI		{ Print($3) }

expr_rule:
  | FLOAT_LITERAL { Lit(FloatLit($1)) }
  | BOOL_LITERAL { Lit(BoolLit($1)) }
  | INT_LITERAL { Lit(IntLit($1)) }
  | STRING_LITERAL { Lit (StringLit $1) }
  | expr_rule EQ expr_rule { Binop($1, Eq, $3) }
  | expr_rule NEQ expr_rule { Binop($1, Neq, $3) }
  | expr_rule LT expr_rule { Binop($1, Less, $3) }
  | expr_rule GT expr_rule { Binop($1, Greater, $3) }
  | expr_rule LEQ expr_rule { Binop($1, Leq, $3) }
  | expr_rule GEQ expr_rule { Binop($1, Geq, $3) }
  | expr_rule AND expr_rule { Binop($1, And, $3) }
  | expr_rule OR expr_rule { Binop($1, Or, $3) }
  | expr_rule PLUS expr_rule { Binop($1, Add, $3) }
  | expr_rule MINUS expr_rule { Binop($1, Sub, $3) }
  | expr_rule TIMES expr_rule { Binop($1, Mul, $3) }
  | expr_rule DIVIDE expr_rule { Binop($1, Div, $3) }
  | expr_rule EXP expr_rule { Binop($1, Exp, $3) }

/* ast end */

