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
  double                d;    /* double value */
  std::string          *s;    /* symbol name or string literal */
  cdk::basic_node      *node; /* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
  mml::block_node      *block;
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING
%token tTYPE_INT tTYPE_DOUBLE tTYPE_STRING tTYPE_VOID
%token tFOREIGN tFORWARD tPUBLIC tPRIVATE tAUTO
%token tWHILE tSTOP tNEXT tRETURN
%token tIF tELIF // TODO: review if ELIF should have precedence
%token tINPUT tNULL tSIZEOF
%token tBEGIN tEND
%token tPRINT tPRINTLN
%token tAND tOR // TODO: review if these should have precedence

%nonassoc tIFX
%nonassoc tELSE

// TODO: review precedences; see expressions table on ref manual
%right '='
%nonassoc '~'
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY

%type <sequence> fdecls decls instrs exprs
%type <node> fdecl program decl instr
%type <type> type
%type <block> decls_instrs blk
%type <expression> expr
%type <lvalue> lval
%type <s> string
%type <i> qual

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : fdecls program    { compiler->ast(new cdk::sequence_node(LINE, $2, $1)); }
     | fdecls            { compiler->ast($1); }
     |        program    { compiler->ast(new cdk::sequence_node(LINE, $1)); }
     | /* empty */       { compiler->ast(new cdk::sequence_node(LINE)); }
     ;

fdecls : fdecls fdecl    { $$ = new cdk::sequence_node(LINE, $2, $1); }
       |        fdecl    { $$ = new cdk::sequence_node(LINE, $1); }
       ;

// FIXME: possibly extract decl?
fdecl : qual    type  tIDENTIFIER          ';'    { $$ = new mml::declaration_node(LINE, $1, $2, *$3, nullptr); delete $3; }
      | qual    type  tIDENTIFIER '=' expr ';'    { $$ = new mml::declaration_node(LINE, $1, $2, *$3, $5); delete $3; }
      | qual    tAUTO tIDENTIFIER '=' expr ';'    { $$ = new mml::declaration_node(LINE, $1, nullptr, *$3, $5); delete $3; }
      | tPUBLIC       tIDENTIFIER '=' expr ';'    { $$ = new mml::declaration_node(LINE, tPUBLIC, nullptr, *$2, $4); delete $2; }
      |         decl /* no qual; "private" */     { $$ = $1; }
      ;

qual : tFOREIGN    { $$ = tFOREIGN; }
     | tFORWARD    { $$ = tFORWARD; }
     | tPUBLIC     { $$ = tPUBLIC; }
     ;

// TODO: reference types and functional types
type : tTYPE_INT       { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
     | tTYPE_DOUBLE    { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
     | tTYPE_STRING    { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
     ;

program : tBEGIN decls_instrs tEND    { $$ = new mml::function_node(LINE, $2); }
        ;

decls_instrs : decls instrs    { $$ = new mml::block_node(LINE, $1, $2); }
             | decls           { $$ = new mml::block_node(LINE, $1, new cdk::sequence_node(LINE)); }
             |       instrs    { $$ = new mml::block_node(LINE, new cdk::sequence_node(LINE), $1); }
             ;

decls : decls decl    { $$ = new cdk::sequence_node(LINE, $2, $1); }
      |       decl    { $$ = new cdk::sequence_node(LINE, $1); }
      ;

decl : type  tIDENTIFIER          ';'    { $$ = new mml::declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); delete $2; }
     | type  tIDENTIFIER '=' expr ';'    { $$ = new mml::declaration_node(LINE, tPRIVATE, $1, *$2, $4); delete $2; }
     | tAUTO tIDENTIFIER '=' expr ';'    { $$ = new mml::declaration_node(LINE, tPRIVATE, nullptr, *$2, $4); delete $2; }
     ;

instrs : instrs instr    { $$ = new cdk::sequence_node(LINE, $2, $1); }
       |        instr    { $$ = new cdk::sequence_node(LINE, $1); }
       ;

// TODO: add remaining instructions
instr : expr ';'                              { $$ = new mml::evaluation_node(LINE, $1); }
      | exprs tPRINT                          { $$ = new mml::print_node(LINE, $1, false); }
      | exprs tPRINTLN                        { $$ = new mml::print_node(LINE, $1, true); }
      | tIF '(' expr ')' instr %prec tIFX     { $$ = new mml::if_node(LINE, $3, $5); }
      | tIF '(' expr ')' instr tELSE instr    { $$ = new mml::if_else_node(LINE, $3, $5, $7); }
      | tWHILE '(' expr ')' instr             { $$ = new mml::while_node(LINE, $3, $5); }
      | tRETURN expr ';'                      { $$ = new mml::return_node(LINE, $2); }
      | blk                                   { $$ = $1; }
      ;

blk : '{' decls_instrs '}'    { $$ = $2; }
    ;

exprs : exprs ',' expr    { $$ = new cdk::sequence_node(LINE, $3, $1); }
      |           expr    { $$ = new cdk::sequence_node(LINE, $1); }
      ;

// TODO: add remaining expressions
expr : tINTEGER                 { $$ = new cdk::integer_node(LINE, $1); }
     | tDOUBLE                  { $$ = new cdk::double_node(LINE, $1); }
     | string                   { $$ = new cdk::string_node(LINE, *$1); delete $1; }
     | '-' expr %prec tUNARY    { $$ = new cdk::neg_node(LINE, $2); }
     | '~' expr                 { $$ = new cdk::not_node(LINE, $2); }
     | expr '+' expr            { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr            { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr            { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr            { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr            { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr            { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr            { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr            { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr            { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr            { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr            { $$ = new cdk::eq_node(LINE, $1, $3); }
     | '(' expr ')'             { $$ = $2; }
     | lval                     { $$ = new cdk::rvalue_node(LINE, $1); }
     | lval '=' expr            { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | tINPUT                   { $$ = new mml::input_node(LINE); }
     ;

lval : tIDENTIFIER    { $$ = new cdk::variable_node(LINE, $1); }
     ;

string : string tSTRING    { $$ = $1; $$->append(*$2); delete $2; }
       | tSTRING           { $$ = $1; }
       ;

%%
