/*  This file is part of the PascalScript Pascal interpreter. */
/*  SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com> */
/*  SPDX-License-Identifier: GPL-3.0-or-later */

/* Disable Flex features we don't need, to avoid warnings. */
/* %option nodefault noinput nounput noyywrap */
%option noinput nounput

%{

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include "error.h"
#include "lexer.h"
#include "pascalscript.tab.h"

int yyerror(const char *message);

bool lexer_trace = 1;

%}

%option yylineno

/* cf. /usr/share/doc/flex/examples/manual/pascal.lex */
/* define exclusive comment states */
%x            COMMENT1
%x            COMMENT2
/* cf. https://github.com/TimD1/DragonCompiler/blob/master/compiler.l */
%x            COMMENT3

/* regular expressions */
identifier        [a-zA-Z][a-zA-Z0-9]*
integer_value     [0-9]+
real_value        [0-9+][\.][0-9]+
/* cf. /usr/share/doc/flex/examples/manual/pascal.lex */
char_value        \'([^'\n|\'\'])\'
string_value      \'([^'\n|\'\'])*\'
bad_string_value  \'([^'\n]|\'\')+
whitespace        [ \t]+
eol               [\r\n|\r|\n]
A                 [Aa]
B                 [Bb]
C                 [Cc]
D                 [Dd]
E                 [Ee]
F                 [Ff]
G                 [Gg]
H                 [Hh]
I                 [Ii]
/*J                 [Jj]*/
/*K                 [Kk]*/
L                 [Ll]
M                 [Mm]
N                 [Nn]
O                 [Oo]
P                 [Pp]
/*Q                 [Qq]*/
R                 [Rr]
S                 [Ss]
T                 [Tt]
U                 [Uu]
V                 [Vv]
W                 [Ww]
/*X                 [Xx]*/
Y                 [Yy]
/*Z                 [Zz]*/

%%

 /* COMMENTS */
"{"                     { BEGIN COMMENT1; }
<COMMENT1>[^}]          { ; }
<COMMENT1>"}"           { if (lexer_trace) fprintf(stderr, "{COMMENT}"); BEGIN INITIAL; }
"(*"                    { BEGIN COMMENT2; }
<COMMENT2>.             |
<COMMENT2>\n            { ; }
<COMMENT2>"*)"          { if (lexer_trace) fprintf(stderr, "(*COMMENT*)"); BEGIN INITIAL; }
"//"                    { BEGIN COMMENT3; }
<COMMENT3>.             { ; }
<COMMENT3>\n            { if (lexer_trace) fprintf(stderr, "//COMMENT\n"); BEGIN INITIAL; }

 /* OPERATORS */
"+"                     { if (lexer_trace) fprintf(stderr, " +"        ); return T_PLUS;              }
"-"                     { if (lexer_trace) fprintf(stderr, " -"        ); return T_MINUS;             }
"*"                     { if (lexer_trace) fprintf(stderr,  " *"       ); return T_STAR;              }
"/"                     { if (lexer_trace) fprintf(stderr,  " /"       ); return T_SLASH;             }
{D}{I}{V}               { if (lexer_trace) fprintf(stderr,  " DIV"     ); return T_DIV;               }
{M}{O}{D}               { if (lexer_trace) fprintf(stderr,  " MOD"     ); return T_MOD;               }
":="                    { if (lexer_trace) fprintf(stderr,  " :="      ); return T_ASSIGN;            }
"("                     { if (lexer_trace) fprintf(stderr,  " ("       ); return T_LEFT_PARENTHESIS;  }
")"                     { if (lexer_trace) fprintf(stderr,  " )"       ); return T_RIGHT_PARENTHESIS; }
":"                     { if (lexer_trace) fprintf(stderr,  " :"       ); return T_COLON;             }
";"                     { if (lexer_trace) fprintf(stderr,  " ;"       ); return T_SEMICOLON;         }
"."                     { if (lexer_trace) fprintf(stderr,  " ."       ); return T_DOT;               }
"="                     { if (lexer_trace) fprintf(stderr,  " ="       ); return T_EQUALS;            }
","                     { if (lexer_trace) fprintf(stderr,  " ,"       ); return T_COMMA;             }

 /* KEYWORDS */
{P}{R}{O}{G}{R}{A}{M}   { if (lexer_trace) fprintf(stderr,  " PROGRAM" ); return T_PROGRAM;           }
{C}{O}{N}{S}{T}         { if (lexer_trace) fprintf(stderr,  " CONST"   ); return T_CONST;             }
{V}{A}{R}               { if (lexer_trace) fprintf(stderr,  " VAR"     ); return T_VAR;               }
{B}{E}{G}{I}{N}         { if (lexer_trace) fprintf(stderr,  " BEGIN"   ); return T_BEGIN;             }
{E}{N}{D}               { if (lexer_trace) fprintf(stderr,  " END"     ); return T_END;               }
{I}{N}{T}{E}{G}{E}{R}   { if (lexer_trace) fprintf(stderr,  " INTEGER" ); return T_INTEGER;           }
{W}{R}{I}{T}{E}{L}{N}   { if (lexer_trace) fprintf(stderr,  " WRITELN" ); return T_WRITE;             }

 /* FROM FUTURE IMPORT ;-) */
".."                    { if (lexer_trace) fprintf(stderr,  " .."      ); return T_DOT_DOT;           }
"["                     { if (lexer_trace) fprintf(stderr,  " ["       ); return T_LEFT_BRACKET;      }
"]"                     { if (lexer_trace) fprintf(stderr,  " ]"       ); return T_RIGHT_BRACKET;     }
{W}{R}{I}{T}{E}         { if (lexer_trace) fprintf(stderr,  " WRITE"   ); return T_WRITE;             }
{B}{O}{O}{L}{E}{A}{N}   { if (lexer_trace) fprintf(stderr,  " BOOLEAN" ); return T_BOOLEAN;           }
{F}{A}{L}{S}{E}         { if (lexer_trace) fprintf(stderr,  " FALSE"   ); return T_FALSE;             }
{T}{R}{U}{E}            { if (lexer_trace) fprintf(stderr,  " TRUE"    ); return T_TRUE;              }
{R}{E}{A}{L}            { if (lexer_trace) fprintf(stderr,  " REAL"    ); return T_REAL;              }
{C}{H}{A}{R}            { if (lexer_trace) fprintf(stderr,  " CHAR"    ); return T_CHAR;              }
{S}{T}{R}{I}{N}{G}      { if (lexer_trace) fprintf(stderr,  " STRING"  ); return T_STRING;            }
{A}{R}{R}{A}{Y}         { if (lexer_trace) fprintf(stderr,  " ARRAY"   ); return T_ARRAY;             }
{O}{F}                  { if (lexer_trace) fprintf(stderr,  " OF"      ); return T_OF;                }

 /* LITTERAL VALUES */
{integer_value}         { int zzerror = lexer_copy_integer_value(yytext);
                          if (zzerror == ERROR_NONE)
                          {
                            if (lexer_trace) fprintf(stderr, " T_INTEGER_VALUE:[%s]", yytext);
                            return T_INTEGER_VALUE;
                          }
                          yyerror("Invalid integer value");
                          yyterminate();
                        }
{real_value}            { /* TODO */
                          if (lexer_trace) fprintf(stderr, " ***TODO*** T_REAL_VALUE:[%s]", yytext);
                          return T_REAL_VALUE;
                        }
{char_value}            { // TODO
                          if (lexer_trace) fprintf(stderr, " ***TODO*** T_CHAR_VALUE:[%s]", yytext);
                          return T_CHAR_VALUE;
                        }
{string_value}          { /* TODO */
                          if (lexer_trace) fprintf(stderr, " ***TODO*** T_STRING_VALUE:[%s]", yytext);
                          return T_STRING_VALUE;
                        }
{bad_string_value}      { yyerror("Unterminated string");
                          yyterminate();
                        }
{identifier}            { int zzerror = lexer_copy_identifier(yytext);
                          if (zzerror == ERROR_NONE)
                          {
                            if (lexer_trace) fprintf(stderr, " T_IDENTIFIER:[%s %s]", yytext, yylval.value.identifier);
                            return T_IDENTIFIER;
                          }
                          yyerror("Invalid identifier");
                          yyterminate();
                        }

{whitespace}            { ; }
{eol}                   { if (lexer_trace) fprintf(stderr,  "\n%d ", yylineno ); }
.                       { if (lexer_trace) fprintf(stderr,  " {%c} ", yytext[0] );
                          yyerror("Invalid source code");
                          yyterminate();
                        }

%%

int yyerror(const char *message) 
{
  return fprintf(stderr, "LEX Error: %s\n", message);
}

int yywrap()
{
  return 1;
}

/* EOF */
