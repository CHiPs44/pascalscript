/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _LEXER_H
#define _LEXER_H

#include <stdbool.h>

#include "pascalscript.h"
#include "error.h"

#ifdef __cplusplus
extern "C"
{
#endif

#define MAX_IDENTIFIER 31

    typedef enum _token_type_t
    {
        TOKEN_NONE = -1,
        TOKEN_EOF = 0,
        // Number types                       Size in bytes
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
        TOKEN_INTEGER,
        TOKEN_BOOLEAN,
        TOKEN_CHAR,
        TOKEN_STRING,
        TOKEN_FALSE,
        TOKEN_TRUE,
        TOKEN_FUNCTION,
        TOKEN_PROCEDURE,
        // Operators
        TOKEN_ASSIGN,   // :=
        TOKEN_ADD,      // +
        TOKEN_SUB,      // -
        TOKEN_MUL,      // *
        TOKEN_DIV_REAL, // /
        TOKEN_DIV,      // DIV
        TOKEN_MOD,      // MOD
        // Comparison operators
        TOKEN_EQ, // =
        TOKEN_NE, // <>
        TOKEN_LT, // <
        TOKEN_LE, // <=
        TOKEN_GT, // >
        TOKEN_GE, // >=
        // Logical operators
        TOKEN_AND,    // AND
        TOKEN_OR,     // OR
        TOKEN_XOR,    // XOR
        TOKEN_NOT,    // NOT
        TOKEN_LSHIFT, // <<
        TOKEN_RSHIFT, // >>
    } token_type_t;

    typedef struct _keyword_t
    {
        int token;
        char *keyword;
        bool symbolic;
    } keyword_t;

    typedef struct _token_t
    {
        token_type_t type;
        union
        {
            char identifier[MAX_IDENTIFIER + 1];
            PS_INTEGER int_val;
            PS_REAL real_val;
            PS_CHAR char_val;
            PS_CHAR string_val[PS_STRING_MAX + 1];
        } value;
    } token_t;

    extern void lexer_dump_token(token_t *token);

    extern error_t lexer_copy_identifier();
    extern error_t lexer_copy_integer_value();
    extern error_t lexer_copy_real_value();
    extern error_t lexer_copy_char_value();
    extern error_t lexer_copy_string_value();

#ifdef __cplusplus
}
#endif

#endif /* _LEXER_H */
