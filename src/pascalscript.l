/*  This file is part of the PascalScript Pascal interpreter. */
/*  SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com> */
/*  SPDX-License-Identifier: GPL-3.0-or-later */

/* Disable Flex features we don't need, to avoid warnings. */
/* %option nodefault noinput nounput noyywrap */
%option noinput nounput caseless

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
identifier        [a-zA-Z][a-zA-Z0-9_]*
integer_value     [0-9]+
real_value        [0-9+][\.][0-9]+
/* cf. /usr/share/doc/flex/examples/manual/pascal.lex */
char_value        \'([^'\n|\'\'])\'
string_value      \'([^'\n]|\'\')+\'
empty_string_value \'\'
bad_string_value  \'([^'\n]|\'\')+
whitespace        [ \t]+
eol               [\r\n|\r|\n]

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
div                     { if (lexer_trace) fprintf(stderr,  " DIV"     ); return T_DIV;               }
mod                     { if (lexer_trace) fprintf(stderr,  " MOD"     ); return T_MOD;               }
":="                    { if (lexer_trace) fprintf(stderr,  " :="      ); return T_ASSIGN;            }
"("                     { if (lexer_trace) fprintf(stderr,  " ("       ); return T_LEFT_PARENTHESIS;  }
")"                     { if (lexer_trace) fprintf(stderr,  " )"       ); return T_RIGHT_PARENTHESIS; }
":"                     { if (lexer_trace) fprintf(stderr,  " :"       ); return T_COLON;             }
";"                     { if (lexer_trace) fprintf(stderr,  " ;"       ); return T_SEMICOLON;         }
"."                     { if (lexer_trace) fprintf(stderr,  " ."       ); return T_DOT;               }
"="                     { if (lexer_trace) fprintf(stderr,  " ="       ); return T_EQUALS;            }
","                     { if (lexer_trace) fprintf(stderr,  " ,"       ); return T_COMMA;             }

 /* KEYWORDS */
"program"               { if (lexer_trace) fprintf(stderr,  " PROGRAM" ); return T_PROGRAM;           }
"const"                 { if (lexer_trace) fprintf(stderr,  " CONST"   ); return T_CONST;             }
"var"                   { if (lexer_trace) fprintf(stderr,  " VAR"     ); return T_VAR;               }
"begin"                 { if (lexer_trace) fprintf(stderr,  " BEGIN"   ); return T_BEGIN;             }
"end"                   { if (lexer_trace) fprintf(stderr,  " END"     ); return T_END;               }
"integer"               { if (lexer_trace) fprintf(stderr,  " INTEGER" ); return T_INTEGER;           }
"writeln"               { if (lexer_trace) fprintf(stderr,  " WRITELN" ); return T_WRITE;             }

 /* FROM FUTURE IMPORT ;-) */
".."                    { if (lexer_trace) fprintf(stderr,  " .."      ); return T_DOT_DOT;           }
"["                     { if (lexer_trace) fprintf(stderr,  " ["       ); return T_LEFT_BRACKET;      }
"]"                     { if (lexer_trace) fprintf(stderr,  " ]"       ); return T_RIGHT_BRACKET;     }
write                   { if (lexer_trace) fprintf(stderr,  " WRITE"   ); return T_WRITE;             }
boolean                 { if (lexer_trace) fprintf(stderr,  " BOOLEAN" ); return T_BOOLEAN;           }
false                   { if (lexer_trace) fprintf(stderr,  " FALSE"   ); return T_FALSE;             }
true                    { if (lexer_trace) fprintf(stderr,  " TRUE"    ); return T_TRUE;              }
real                    { if (lexer_trace) fprintf(stderr,  " REAL"    ); return T_REAL;              }
char                    { if (lexer_trace) fprintf(stderr,  " CHAR"    ); return T_CHAR;              }
string                  { if (lexer_trace) fprintf(stderr,  " STRING"  ); return T_STRING;            }
array                   { if (lexer_trace) fprintf(stderr,  " ARRAY"   ); return T_ARRAY;             }
of                      { if (lexer_trace) fprintf(stderr,  " OF"      ); return T_OF;                }

 /* LITTERAL VALUES */
{integer_value}         { int err = lexer_copy_integer_value(yytext);
                          if (err == ERROR_NONE)
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
{char_value}            { int err = lexer_copy_char_value(yytext);
                          if (err == ERROR_NONE)
                          {
                              if (lexer_trace) fprintf(stderr, " T_CHAR_VALUE:[%s]", yytext);
                              return T_CHAR_VALUE;
                          }
                          yyerror("Invalid char value");
                          yyterminate();
                        }
{empty_string_value}    { yylval.type = STRING_VAL;
                          strcpy(yylval.value.string_val, "");
                          if (lexer_trace) fprintf(stderr, " T_STRING_VALUE:[%s]", yytext);
                          return T_STRING_VALUE;

                        }
{string_value}          { int err = lexer_copy_string_value();
                          if (err == ERROR_NONE)
                          {
                              if (lexer_trace) fprintf(stderr, " T_STRING_VALUE:[%s]", yytext);
                              return T_STRING_VALUE;
                          }
                          yyerror("Invalid string value");
                          yyterminate();
                        }
{bad_string_value}      { yyerror("Unterminated string");
                          yyterminate();
                        }
{identifier}            { int err = lexer_copy_identifier(yytext);
                          if (err == ERROR_NONE)
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
