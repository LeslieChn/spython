%{ 
    open Ast
%}

%token SPACE TAB EOL
%token ASSIGN PLUS MINUS TIMES DIVIDE MOD EXP PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODEQ EXPEQ
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE SEMI COMMA
%token EQ NEQ LT LEQ GT GEQ AND OR NOT NEG
%token IF ELSE ELIF WHILE FOR RETURN BREAK CONTINUE 
%token DEF ARROW COLON PRINT RANGE PASS ASSERT
%token BOOL INT FLOAT STRING
%token <bool> BOOL_LITERAL
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> VARIABLE
%token EOF

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
%right NOT NEG

%%

program_rule:
  stmt_list EOF { List.rev $1 }

stmt_list:
  | { [] }
  | stmt_list stmt_rule { $2 :: $1 }

stmt_rule:
  | expr_rule            				{ Expr $1 }
  | PRINT LPAREN expr_rule RPAREN 		{ Print($3) }

expr_rule:
  | FLOAT_LITERAL { Lit(FloatLit($1)) }
  | BOOL_LITERAL { Lit(BoolLit($1)) }
  | INT_LITERAL { Lit(IntLit($1)) }
  | STRING_LITERAL { Lit (StringLit $1) }
  | MINUS expr_rule %prec NEG { Unop(Neg, $2) }
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
