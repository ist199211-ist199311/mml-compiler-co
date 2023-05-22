%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;    /* integer value */
  std::string          *s;    /* symbol name or string literal */
  cdk::basic_node      *node; /* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
  mml::block_node      *block;
};

%token <i> tINTEGER
%token <s> tIDENTIFIER tSTRING
%token tWHILE tIF tPRINT tPRINTLN tINPUT tBEGIN tEND tRETURN

%nonassoc tIFX
%nonassoc tELSE

%right '='
%left tOR
%left tAND
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY

%type <node> stmt program
%type <sequence> list exprs
%type <expression> expr
%type <lvalue> lval
%type <block> blk
%type <s> string

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

/* FIXME: block should split declarations and instructions */
program   : tBEGIN list tEND { compiler->ast(new mml::function_node(
                    LINE,
                    new mml::block_node(LINE, new cdk::sequence_node(LINE), $2)
               )); }
          ;

/* FIXME: block should split declarations and instructions */
blk : '{' list '}' { $$ = new mml::block_node(LINE, new cdk::sequence_node(LINE), $2); }
    ;

list : stmt      { $$ = new cdk::sequence_node(LINE, $1); }
     | list stmt { $$ = new cdk::sequence_node(LINE, $2, $1); }
     ;

stmt : expr ';'                         { $$ = new mml::evaluation_node(LINE, $1); }
     | exprs tPRINT                     { $$ = new mml::print_node(LINE, $1, false); }
     | exprs tPRINTLN                   { $$ = new mml::print_node(LINE, $1, true); }
     | tWHILE '(' expr ')' stmt         { $$ = new mml::while_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt %prec tIFX { $$ = new mml::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt tELSE stmt { $$ = new mml::if_else_node(LINE, $3, $5, $7); }
     | tRETURN expr ';'                 { $$ = new mml::return_node(LINE, $2); }
     | blk                              { $$ = $1; }
     ;

expr : tINTEGER                { $$ = new cdk::integer_node(LINE, $1); }
     | string                  { $$ = new cdk::string_node(LINE, $1); }
     | '-' expr %prec tUNARY   { $$ = new cdk::neg_node(LINE, $2); }
     | '~' expr %prec tUNARY   { $$ = new cdk::not_node(LINE, $2); }
     | expr '+' expr           { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr           { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr           { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr           { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr           { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr           { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr           { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr           { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr           { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr           { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr           { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tOR expr           { $$ = new cdk::or_node(LINE, $1, $3); }
     | expr tAND expr          { $$ = new cdk::and_node(LINE, $1, $3); }
     | '(' expr ')'            { $$ = $2; }
     | lval                    { $$ = new cdk::rvalue_node(LINE, $1); }  //FIXME
     | lval '=' expr           { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | tINPUT                  { $$ = new mml::input_node(LINE); }
     ;

exprs   : expr                 { $$ = new cdk::sequence_node(LINE, $1);     }
        | exprs ',' expr       { $$ = new cdk::sequence_node(LINE, $3, $1); }
        ;

lval : tIDENTIFIER             { $$ = new cdk::variable_node(LINE, $1); }
     ;

string : tSTRING               { $$ = $1; }
       | string tSTRING        { $$ = $1; $$->append(*$2); delete $2; }

%%
