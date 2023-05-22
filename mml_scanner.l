%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

// don't change this
#define yyerror LexerError
%}

OCTAL          [0-7]{1,3}

%x X_STRING X_STRING_NULL X_COMMENT
%%
  yydebug=1; set_debug(1);

"//".*                 ; /* ignore comments */

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>"/*"        yy_push_state(X_COMMENT);
<X_COMMENT>.|\n        ; /* ignore comments */

">="                   return tGE;
"<="                   return tLE;
"=="                   return tEQ;
"!="                   return tNE;

"||"                   return tOR;
"&&"                   return tAND;

"while"                return tWHILE;
"if"                   return tIF;
"else"                 return tELSE;
"input"                return tINPUT;
"begin"                return tBEGIN;
"end"                  return tEND;
"return"               return tRETURN;

"!"                    return tPRINT;
"!!"                   return tPRINTLN;

[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

\"                     yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\"           yy_pop_state(); return tSTRING;
<X_STRING>\\t          *yylval.s += '\t';
<X_STRING>\\n          *yylval.s += '\n';
<X_STRING>\\r          *yylval.s += '\r';
<X_STRING>\\\"         *yylval.s += yytext + 1;
<X_STRING>\\\\         *yylval.s += yytext + 1;
<X_STRING>\\0          yy_push_state(X_STRING_NULL);
%{
/* TODO error handling? */
%}
<X_STRING>\\{OCTAL}    *yylval.s += (char) std::stoi(yytext + 1, NULL, 8);
<X_STRING>\n           yyerror("newline in string");
<X_STRING>\0           yyerror("ASCII NULL in string");
<X_STRING>.            *yylval.s += yytext;

<X_STRING_NULL>\n      yyerror("newline in string");
<X_STRING_NULL>\0      yyerror("ASCII NULL in string");
<X_STRING_NULL>\"      yy_pop_state(); yy_pop_state(); return tSTRING;
<X_STRING_NULL>\\\"    /* ignore everything after ASCII NULL */
<X_STRING_NULL>\\\\    /* ignore everything after ASCII NULL */
<X_STRING_NULL>.       /* ignore everything after ASCII NULL */

{OCTAL}                yylval.i = strtol(yytext, nullptr, 8); return tINTEGER;
0[0-9]+                yyerror("invalid octal (base 8) number");

[0-9]+                 yylval.i = strtol(yytext, nullptr, 10); return tINTEGER;

[-~()<>=+*/%;{}.,]      return *yytext;

[ \t\n]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
