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

"+"                   { return T_PLUS;              }
"-"                   { return T_MINUS;             }
"*"                   { return T_STAR;              }
"/"                   { return T_SLASH;             }
{D}{I}{V}             { return T_DIV;               }
{M}{O}{D}             { return T_MOD;               }
":="                  { return T_ASSIGN;            }
"("                   { return T_LEFT_PARENTHESIS;  }
")"                   { return T_RIGHT_PARENTHESIS; }
":"                   { return T_COLON;             }
";"                   { return T_SEMICOLON;         }
"."                   { return T_DOT;               }
"="                   { return T_EQUALS;            }
{P}{R}{O}{G}{R}{A}{M} { return T_PROGRAM;           }
{C}{O}{N}{S}{T}       { return T_CONST;             }
{V}{A}{R}             { return T_VAR;               }
{B}{E}{G}{I}{N}       { return T_BEGIN;             }
{E}{N}{D}             { return T_END;               }
{W}{R}{I}{T}{E}{L}{N} { return T_WRITELN;           }
{I}{N}{T}{E}{G}{E}{R} { return T_INTEGER;           }

{integer_value} {
  int zzerror = copy_integer_value(yytext);
  if (zzerror == ERROR_NONE)
    return T_INTEGER_VALUE;
  yyerror("Invalid integer value");
}

{identifier} {
  int zzerror = copy_identifier(yytext);
  if (zzerror == ERROR_NONE)
    return T_IDENTIFIER;
  yyerror("Invalid identifier");
}

{whitespace} {
  fprintf( stderr, " [ ]");
}

{eol} {
  fprintf( stderr, "\n%d ", yylineno );
}

. {
  fprintf( stderr, " {%c}", yytext[0] );
  // return yytext[0];
  yyerror("Invalid source");
  yyterminate();
}

%%

int yyerror(char *message) 
{
  fprintf(stderr, "Error: %s\n", message);
  // exit(1);
}

int yywrap()
{
  return 1;
}

/* EOF */
