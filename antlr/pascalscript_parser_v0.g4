/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

/* ******************** PascalScript V0 ANTLR4 ******************** */

/*
npm i -g --save-dev antlr-format-cli
antlr-format -v pascalscript_parser_v0.g4
antlr4-parse pascalscript_parser_v0.g4 pascalscript_lexer_v0.g4 pascalFile -tokens -trace < examples/01-first.pas
antlr4 -o /tmp -long-messages -listener -visitor -Dlanguage=Cpp pascalscript_lexer_v0.g4 pascalscript_parser_v0.g4
*/

// $antlr-format alignTrailingComments true
// $antlr-format columnLimit 150
// $antlr-format minEmptyLines 1
// $antlr-format maxEmptyLinesToKeep 1
// $antlr-format reflowComments false
// $antlr-format useTab false
// $antlr-format allowShortRulesOnASingleLine true
// $antlr-format allowShortBlocksOnASingleLine true
// $antlr-format alignSemicolons ownLine
// $antlr-format alignColons hanging

parser grammar pascalscript_parser_v0;

options {
    tokenVocab = pascalscript_lexer_v0;
    // needs antlr4 >= 4.10
    caseInsensitive = true;
}

/* ******************** PROGRAM ******************** */

pascalFile: pascalProgram;

pascalProgram: PROGRAM IDENTIFIER SEMI_COLON pascalHeader instructionBlock DOT EOF;

pascalHeader: pascalBlock*;

pascalBlock: constBlock | varBlock;

instructionBlock: BEGIN (instruction)* END;

instruction: assignment | procedureCall;

assignment: variableReference DOT_COLON expression SEMI_COLON;

procedureCall: (WRITE | WRITELN) (LEFT_PARENTHESIS parameterList RIGHT_PARENTHESIS)? SEMI_COLON;

parameterList: parameter (COMMA parameter)*;

parameter: expression | variableReference | constantReference;

/* ******************** CONSTANTS ******************** */

constBlock: CONST constantDeclaration+;

constantDeclaration: IDENTIFIER EQUAL expression SEMI_COLON;

/* ******************** VARIABLES ******************** */

varBlock: VAR variableDeclaration+;

variableDeclaration: identifierList COLON (REAL | INTEGER | UNSIGNED) SEMI_COLON;

identifierList: IDENTIFIER (COMMA IDENTIFIER)*;

/* ******************** EXPRESSIONS ******************** */

expression
    : unaryOperator expression
    | expression multiplicativeOperator expression
    | expression additiveOperator expression
    | UNSIGNED_INTEGER_VALUE
    | UNSIGNED_REAL_VALUE
    | variableReference
    | constantReference
    | LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
;

unaryOperator: PLUS | MINUS;

additiveOperator: PLUS | MINUS;

multiplicativeOperator: STAR | SLASH | DIV | MOD;

variableReference: IDENTIFIER;

constantReference: IDENTIFIER;

// EOF