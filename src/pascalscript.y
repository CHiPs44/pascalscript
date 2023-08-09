%{
/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include "lexer.h"

int yyerror(const char *message);
extern int yylex (void);

%}

%define parse.error detailed
/* %define api.pure */
%define api.value.type { token_t }

/* operators */
%token T_PLUS T_MINUS T_STAR T_SLASH T_DIV T_MOD
/* assignment */
%token T_ASSIGN
/* parenthesis */
%token T_LEFT_PARENTHESIS
%token T_RIGHT_PARENTHESIS
/* other punctuation */
%token T_COLON
%token T_SEMICOLON
%token T_DOT
%token T_EQUALS
/* "real" keywords */
%token T_PROGRAM T_CONST T_VAR T_BEGIN T_END
/* %token T_TYPE
%token T_PROCEDURE T_FUNCTION */
/* standard library */
%token T_WRITELN
/* type(s) */
%token T_INTEGER
/* values / constants */
%token T_INTEGER_VALUE
/* identifier */
%token T_IDENTIFIER

/* reserved / not implemented */
/* %token T_RESERVED */

/* boolean */
/* %token T_BOOLEAN
%token T_BOOLEAN_VALUE
%token T_FALSE
%token T_TRUE */

/* real */
/* %token T_REAL
%token T_REAL_VALUE */

/* char/string */
/* %token T_QUOTE
%token T_CHAR
%token T_STRING
%token T_CHAR_VALUE
%token T_STRING_VALUE */

/* array */
/* %token T_ARRAY
%token T_OF
%token T_DOT_DOT */

%start program

%%

program:
    program_declaration
    constant_declaration_block
    variable_declaration_block
    T_BEGIN
        statement_list
    T_END T_DOT
;

program_declaration
    : T_PROGRAM T_IDENTIFIER T_SEMICOLON

constant_declaration_block
    : T_CONST constant_declaration_list
    |
;
constant_declaration_list
    : constant_declaration
    | constant_declaration_list constant_declaration
;
constant_declaration
    : T_IDENTIFIER T_EQUALS constant_literal T_SEMICOLON
;
constant_literal
    : T_INTEGER_VALUE
;

variable_declaration_block
    : T_VAR variable_declaration_list
    |
;
variable_declaration_list
    : variable_declaration
    | variable_declaration_list variable_declaration
;
variable_declaration
    : T_IDENTIFIER T_COLON T_INTEGER T_SEMICOLON
;

statement_list
    : statement
    | statement_list statement
;
statement
    : assignement
    | writeln
;
assignement
    : T_IDENTIFIER T_ASSIGN expression T_SEMICOLON
;
writeln
    : T_WRITELN T_LEFT_PARENTHESIS T_IDENTIFIER T_RIGHT_PARENTHESIS T_SEMICOLON
;

expression
    : expression T_PLUS term
    | expression T_MINUS term
    | term
;
term
    : term T_STAR factor
    | term T_SLASH factor
    | term T_DIV factor
    | term T_MOD factor
    | factor
;
factor
    : T_LEFT_PARENTHESIS expression T_RIGHT_PARENTHESIS
    | T_MINUS factor
    | scalar
    | T_IDENTIFIER
;
scalar
    : T_INTEGER_VALUE
;

%%

/*
int yyerror(char *message)
{
    fprintf(stderr, "Syntax error: %s\n", message); 
    return 1;
}
*/

/* EOF */
