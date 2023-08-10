%{
/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>
#include "error.h"
#include "lexer.h"
#include "pascalscript.tab.h"

int yyerror(char *message);

%}

%option yylineno

/* cf.   // https://github.com/TimD1/DragonCompiler/blob/master/compiler.l */
/* define exclusive comment states */
%x            COMMENT1
%x            COMMENT2
%x            COMMENT3

identifier    [a-zA-Z][a-zA-Z0-9]*
integer_value [0-9]+
whitespace    [ \t]+
eol           [\r\n|\r|\n]
A             [Aa]
B             [Bb]
C             [Cc]
D             [Dd]
E             [Ee]
F             [Ff]
G             [Gg]
H             [Hh]
I             [Ii]
J             [Jj]
K             [Kk]
L             [Ll]
M             [Mm]
N             [Nn]
O             [Oo]
P             [Pp]
Q             [Qq]
R             [Rr]
S             [Ss]
T             [Tt]
U             [Uu]
V             [Vv]
W             [Ww]
X             [Xx]
Y             [Yy]
Z             [Zz]

%%

"{"                     { BEGIN COMMENT1; }
<COMMENT1>[^}]          { ; }
<COMMENT1>"}"           { fprintf(stderr, "{COMMENT}"); BEGIN INITIAL; }
"(*"                    { BEGIN COMMENT2; }

<COMMENT2>.             |
<COMMENT2>\n            { ; }
<COMMENT2>"*)"          { fprintf(stderr, "(*COMMENT*)"); BEGIN INITIAL; }

"//"                    { BEGIN COMMENT3; }
<COMMENT3>.             { ; }
<COMMENT3>\n            { fprintf(stderr, "//COMMENT\n"); BEGIN INITIAL; }

"+"                     { fprintf(stderr, " +"       ); return T_PLUS;              }
"-"                     { fprintf(stderr, " -"       ); return T_MINUS;             }
"*"                     { fprintf(stderr, " *"       ); return T_STAR;              }
"/"                     { fprintf(stderr, " /"       ); return T_SLASH;             }
{D}{I}{V}               { fprintf(stderr, " DIV"     ); return T_DIV;               }
{M}{O}{D}               { fprintf(stderr, " MOD"     ); return T_MOD;               }
":="                    { fprintf(stderr, " :="      ); return T_ASSIGN;            }
"("                     { fprintf(stderr, " ("       ); return T_LEFT_PARENTHESIS;  }
")"                     { fprintf(stderr, " )"       ); return T_RIGHT_PARENTHESIS; }
":"                     { fprintf(stderr, " :"       ); return T_COLON;             }
";"                     { fprintf(stderr, " ;"       ); return T_SEMICOLON;         }
"."                     { fprintf(stderr, " ."       ); return T_DOT;               }
"="                     { fprintf(stderr, " ="       ); return T_EQUALS;            }

{P}{R}{O}{G}{R}{A}{M}   { fprintf(stderr, " PROGRAM" ); return T_PROGRAM;           }
{C}{O}{N}{S}{T}         { fprintf(stderr, " CONST"   ); return T_CONST;             }
{V}{A}{R}               { fprintf(stderr, " VAR"     ); return T_VAR;               }
{B}{E}{G}{I}{N}         { fprintf(stderr, " BEGIN"   ); return T_BEGIN;             }
{E}{N}{D}               { fprintf(stderr, " END"     ); return T_END;               }
{W}{R}{I}{T}{E}{L}{N}   { fprintf(stderr, " WRITELN" ); return T_WRITELN;           }
{I}{N}{T}{E}{G}{E}{R}   { fprintf(stderr, " INTEGER" ); return T_INTEGER;           }

{integer_value}         { int zzerror = copy_integer_value(yytext);
                          if (zzerror == LEXER_ERROR_NONE)
                          {
                            fprintf(stderr, " T_INTEGER_VALUE:[%s]", yytext);
                            return T_INTEGER_VALUE;
                          }
                          yyerror("Invalid integer value");
                          yyterminate();
                        }

{identifier}            { int zzerror = copy_identifier(yytext);
                          if (zzerror == LEXER_ERROR_NONE)
                          {
                            fprintf(stderr, " T_IDENTIFIER:[%s]", yytext);
                            return T_IDENTIFIER;
                          }
                          yyerror("Invalid identifier");
                          yyterminate();
                        }

{whitespace}            { ; }

{eol}                   { fprintf(stderr, "\n%d ", yylineno ); }

.                       { fprintf(stderr, " {%c}", yytext[0] );
                          yyerror("Invalid source code");
                          yyterminate();
                        }

%%

int yyerror(char *message) 
{
  fprintf(stderr, "LEX Error: %s\n", message);
}

int yywrap()
{
  return 1;
}

/* EOF */
