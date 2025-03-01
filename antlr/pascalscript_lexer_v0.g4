/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

/* ******************** PascalScript V0 ANTLR4 ******************** */

// antlr-format -v pascalscript_lexer_v0.g4

// $antlr-format alignTrailingComments true
// $antlr-format columnLimit 80
// $antlr-format minEmptyLines 0
// $antlr-format maxEmptyLinesToKeep 1
// $antlr-format reflowComments false
// $antlr-format useTab false
// $antlr-format allowShortRulesOnASingleLine true
// $antlr-format allowShortBlocksOnASingleLine true
// $antlr-format alignSemicolons ownLine
// $antlr-format alignColons hanging

lexer grammar pascalscript_lexer_v0;

options {
    // needs antlr4 >= 4.10
    caseInsensitive = true;
}

/* ******************** WHITESPACE ******************** */

WHITESPACE: [ \n\t\r]+ -> skip;

/* ******************** LEXEMES ******************** */

// NB: lexer rules begin with / are uppercase

// Keywords
PROGRAM: 'PROGRAM';
BEGIN: 'BEGIN';
END: 'END';
INTEGER: 'INTEGER';
CARDINAL: 'CARDINAL';
REAL: 'REAL';
CONST: 'CONST';
VAR: 'VAR';
WRITE: 'WRITE';
WRITELN: 'WRITELN';

// Operators
DIV: 'DIV'; // division (integer)
MOD: 'MOD';

// Arithmetic operators
PLUS: '+';  // addition
MINUS: '-'; // substraction / negation
STAR: '*';  // multiplication
SLASH: '/'; // division (real)

// Symbols
DOT_COLON: ':=';        // assignment
COLON: ':';             // various uses
COMMA: ',';             // various uses
DOT: '.';               // various uses
EQUAL: '=';             // various uses
LEFT_PARENTHESIS: '(';  // various uses
RIGHT_PARENTHESIS: ')'; // various uses
SEMI_COLON: ';';        // various uses
UNDERSCORE: '_';        // identifier part

/* ******************** NUMBERS ******************** */

UNSIGNED_INTEGER_VALUE: DECIMAL_DIGIT_SEQUENCE;

UNSIGNED_REAL_VALUE
    : DECIMAL_DIGIT_SEQUENCE DOT DECIMAL_DIGIT_SEQUENCE (
        'E'? SIGN? DECIMAL_DIGIT_SEQUENCE
    )?
;

DECIMAL_DIGIT_SEQUENCE: DECIMAL_DIGIT+;

SIGN: PLUS | MINUS;

DECIMAL_DIGIT: '0' ..'9';

/* ******************** IDENTIFIER ******************** */

IDENTIFIER
    : IDENTIFIER_PREFIX
    | IDENTIFIER_PREFIX (
        LETTER
        | DECIMAL_DIGIT
        | UNDERSCORE
    )+
;

IDENTIFIER_PREFIX: LETTER | UNDERSCORE;

LETTER: 'A' ..'Z';

/* ******************** COMMENTS ******************** */

COMMENT1: '(*' ANY_CHAR* '*)' -> skip;

COMMENT2: '{' ANY_CHAR* '}' -> skip;

COMMENT3: '//' ANY_CHAR* '\n' -> skip;

ANY_CHAR: .;

// EOF