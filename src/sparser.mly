%{ open Ast %}

%token SPACE TAB EOL
%token ASSIGN PLUS MINUS TIMES DIVIDE MOD EXP PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODEQ EXPEQ
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE SEMI COMMA
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token IF ELSE ELIF WHILE FOR RETURN BREAK CONTINUE 
%token DEF ARROW COLON PRINT RANGE PASS ASSERT
%token INT BOOL STRING
%token <bool> BLIT
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> VARIABLE
%token EOF

/*%start token_stream
%type <Ast.tokenseq> token_stream
*/

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

/* ast start */

program_rule:
  stmt_list EOF { List.rev $1 }

stmt_list:
  | { [] }
  | stmt_list stmt_rule { $2 :: $1 }

stmt_rule:
  | expr_rule                     { Expr $1 }
  | PRINT LPAREN expr_rule RPAREN { Print($3) }

expr_rule:
  | STRING_LITERAL { Lit(StringLit($1)) }

/* ast end */
