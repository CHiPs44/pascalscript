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

/*
%define parse.error detailed
*/
%define parse.trace
%define api.value.type { token_t }

/* operators */
%token T_PLUS T_MINUS T_STAR T_SLASH T_DIV T_MOD
/* assignment */
%token T_ASSIGN
/* parenthesis */
%token T_LEFT_PARENTHESIS T_RIGHT_PARENTHESIS
/* other punctuation */
%token T_COLON T_SEMICOLON T_DOT T_EQUALS T_COMMA
/* "real" keywords */
%token T_PROGRAM T_CONST T_VAR T_BEGIN T_END
/* %token T_TYPE T_PROCEDURE T_FUNCTION */
/* standard library */
%token T_WRITE T_WRITELN
/* type(s) */
%token T_INTEGER
/* values / constants */
%token T_INTEGER_VALUE
/* identifier */
%token T_IDENTIFIER

/* boolean */
%token T_BOOLEAN T_BOOLEAN_VALUE T_FALSE T_TRUE

/* real */
%token T_REAL T_REAL_VALUE

/* char/string */
%token T_CHAR T_CHAR_VALUE
%token T_STRING T_STRING_VALUE

/* array */
%token T_ARRAY T_OF T_DOT_DOT T_LEFT_BRACKET T_RIGHT_BRACKET

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
    | T_PROGRAM T_IDENTIFIER T_LEFT_PARENTHESIS program_parameter_list T_RIGHT_PARENTHESIS T_SEMICOLON

program_parameter_list
    : T_IDENTIFIER
    | T_IDENTIFIER T_COMMA program_parameter_list

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
    | T_MINUS T_INTEGER_VALUE
    | T_FALSE
    | T_TRUE
    | T_REAL_VALUE
    | T_CHAR_VALUE
    | T_STRING_VALUE
;
/* constant_boolean
    : T_FALSE
    | T_TRUE
; */

variable_declaration_block
    : T_VAR variable_declaration_list
    |
;
variable_declaration_list
    : variable_declaration
    | variable_declaration_list variable_declaration
;
variable_declaration
    : T_IDENTIFIER T_COLON variable_type T_SEMICOLON
;

variable_type
    : T_INTEGER
    | T_BOOLEAN
    | T_REAL
    | T_CHAR
    | T_STRING
    | array_type_declaration

array_type_declaration
    : T_ARRAY T_LEFT_BRACKET T_INTEGER_VALUE T_DOT_DOT T_INTEGER_VALUE T_RIGHT_BRACKET T_OF variable_type

statement_list
    : statement
    | statement_list statement
;
statement
    : assignement
    | write
    | writeln
;
assignement
    : T_IDENTIFIER T_ASSIGN expression T_SEMICOLON
;
write
    : T_WRITE T_LEFT_PARENTHESIS T_IDENTIFIER T_RIGHT_PARENTHESIS T_SEMICOLON
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
    | value
    | T_IDENTIFIER
;
value
    : T_INTEGER_VALUE
    | T_REAL_VALUE
    | T_BOOLEAN_VALUE
    | T_CHAR_VALUE
    | T_STRING_VALUE
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
