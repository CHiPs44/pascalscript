/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _KEYWORDS_H
#define _KEYWORDS_H

#include "lexer.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef MAX_IDENTIFIER
#define MAX_IDENTIFIER 31
#endif

/*
    -absolute    +and         +array       -asm             +begin       +case        +const       -constructor
    -destructor  +div         +do          +downto          +else        +end         -file        +for
    +function    +goto        +if          -implementation  +in          -inherited   -inline      -interface
    +label       +mod         -nil         +not             -object      +of          -operator    +or
    -packed      +procedure   +program     -record          -reintroduce +repeat      -self        -set
    +shl         +shr         +string      +then            +to          +type        -unit        +until
    -uses        +var         +while       -with            +xor
*/

    typedef enum _token_type_t
    {
        TOKEN_NONE = -1,
        TOKEN_END_OF_FILE = 0,
        // Number values                      Size in bytes
        TOKEN_SHORT_INTEGER_VALUE = 1,     // 1   -128..127
        TOKEN_BYTE_VALUE = 2,              // 1   0..255
        TOKEN_SMALL_INTEGER = 4,           // 2   -32,768..32,767
        TOKEN_WORD_VALUE = 8,              // 2   0..65,535
        TOKEN_INTEGER_VALUE = 16,          // 4   -2,147,483,648..2,147,483,647
        TOKEN_UNSIGNED_INTEGER_VALUE = 32, // 4   0..4,294,967,295
        TOKEN_LONG_INTEGER_VALUE = 64,     // 4   -2,147,483,648..2,147,483,647
        TOKEN_LONG_WORD_VALUE = 128,       // 4   0..4,294,967,295
        TOKEN_REAL_VALUE = 256,            // 8   5.0E-324..1.7E308
        // Other value types
        TOKEN_BOOLEAN_VALUE = 1024,
        TOKEN_CHAR_VALUE,
        TOKEN_STRING_VALUE,
        // Identifier
        TOKEN_IDENTIFIER,
        // Reserved words
        TOKEN_RESERVED_WORDS = 2047,
        TOKEN_PROGRAM,
        TOKEN_CONST,
        TOKEN_VAR,
        TOKEN_TYPE,
        TOKEN_BEGIN,
        TOKEN_END,
        TOKEN_ARRAY,
        TOKEN_INTEGER,
        TOKEN_BOOLEAN,
        TOKEN_CHAR,
        TOKEN_STRING,
        TOKEN_FALSE,
        TOKEN_TRUE,
        TOKEN_FUNCTION,
        TOKEN_PROCEDURE,
        TOKEN_IF,
        TOKEN_THEN,
        TOKEN_ELSE,
        TOKEN_DO,
        TOKEN_WHILE,
        TOKEN_REPEAT,
        TOKEN_UNTIL,
        TOKEN_FOR,
        TOKEN_TO,
        TOKEN_DOWNTO,
        TOKEN_IN,
        TOKEN_CASE,
        TOKEN_OF,
        TOKEN_OTHERWISE,
        TOKEN_GOTO,
        TOKEN_LABEL,
        // Ponctuation
        TOKEN_ASSIGN,              // :=
        TOKEN_CARET,               // ^
        TOKEN_COLON,               // :
        TOKEN_COMMA,               // ,
        TOKEN_DOT_DOT,             // ..
        TOKEN_DOT,                 // .
        TOKEN_SEMI_COLON,          // ;
        TOKEN_LEFT_PARENTHESIS,    // (
        TOKEN_RIGHT_PARENTHESIS,   // )
        TOKEN_LEFT_BRACKET,        // [
        TOKEN_RIGHT_BRACKET,       // ]
        TOKEN_LEFT_CURLY_BRACKET,  // {
        TOKEN_RIGHT_CURLY_BRACKET, // }
        TOKEN_DOLLAR,              // $
        TOKEN_LEFT_COMMENT,        // (*
        TOKEN_RIGHT_COMMENT,       // *)
        // Operators
        TOKEN_ADD,      // +
        TOKEN_SUB,      // -
        TOKEN_MUL,      // *
        TOKEN_DIV_REAL, // /
        TOKEN_DIV,      // DIV
        TOKEN_MOD,      // MOD
        // Comparison operators
        TOKEN_EQUALS,                // =
        TOKEN_NOT_EQUALS,            // <>
        TOKEN_LESS_THAN,             // <
        TOKEN_LESS_OR_EQUAL_THAN,    // <=
        TOKEN_GREATER_THAN,          // >
        TOKEN_GREATER_OR_EQUAL_THAN, // >=
        // Logical operators
        TOKEN_AND, // AND
        TOKEN_OR,  // OR
        TOKEN_XOR, // XOR
        TOKEN_NOT, // NOT
        TOKEN_SHL, // SHL
        TOKEN_SHR, // SHR
    } token_type_t;

    typedef struct _token_t
    {
        token_type_t type;
        union
        {
            char identifier[MAX_IDENTIFIER + 1];
            PS_INTEGER i;
            PS_REAL r;
            PS_CHAR c;
            PS_CHAR s[PS_STRING_MAX + 1];
        } value;
    } token_t;

    typedef struct _keyword_t
    {
        int token;
        char *keyword;
    } keyword_t;

    extern keyword_t keywords[];

#ifdef __cplusplus
}
#endif

#endif /* _KEYWORDS_H */
