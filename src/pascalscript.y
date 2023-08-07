%{
    /* SPDX-License-Identifier: GPL-3.0-or-later */

    #include <stdio.h>
    #include "lexer.h"

    // extern void yyerror(string *message);
    int yyerror(char *message);
%}

/* %define parse.error detailed */
/* %define api.pure */
%define api.value.type { token_t }

/* operators */
%token T_PLUS T_MINUS T_STAR T_SLASH T_DIV T_MOD
/* assignment */
%token T_ASSIGN
/* parenthesis */
%token T_LEFT_PARENTHESIS
%token T_RIGHT_PARENTHESIS
%token T_COLON
%token T_SEMICOLON
%token T_DOT
%token T_EQUALS
/* "real" keywords */
%token T_PROGRAM T_CONST T_VAR T_BEGIN T_END
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

%start program

%%

program:
    program_declaration
    constant_declaration_list
    variable_declaration_list
    T_BEGIN
        statement_list
    T_END T_DOT
;

program_declaration
    : T_PROGRAM T_IDENTIFIER T_SEMICOLON

constant_declaration_list
    : constant_declaration
    | constant_declaration_list constant_declaration
;
constant_declaration
    : T_CONST T_IDENTIFIER T_EQUALS T_INTEGER_VALUE T_SEMICOLON
;

variable_declaration_list
    : variable_declaration
    | variable_declaration_list variable_declaration
;
variable_declaration
    : T_VAR T_IDENTIFIER T_COLON T_INTEGER T_SEMICOLON
;

statement_list
    : statement
    | statement_list statement
;
statement
    : T_IDENTIFIER T_ASSIGN expression T_SEMICOLON
    | T_WRITELN T_LEFT_PARENTHESIS T_IDENTIFIER T_RIGHT_PARENTHESIS T_SEMICOLON
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
