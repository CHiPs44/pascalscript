/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_ERROR_H
#define _PS_ERROR_H

#include <stdarg.h>

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum _error_t
    {
        /* general */
        ERROR_ZERO = 0,
        ERROR_NOT_IMPLEMENTED,
        /* buffer*/
        BUFFER_ERROR_NONE = 1000,
        BUFFER_ERROR_OPENING_FILE,
        BUFFER_ERROR_READING_FILE,
        BUFFER_ERROR_OUT_OF_MEMORY,
        /* lexer */
        LEXER_ERROR_NONE = 2000,
        LEXER_ERROR_UNEXPECTED_CHARACTER,
        LEXER_ERROR_UNEXPECTED_EOF,
        LEXER_ERROR_EXPECTED_TOKEN,
        LEXER_ERROR_UNEXPECTED_TOKEN,
        LEXER_ERROR_BUFFER_OVERFLOW,
        LEXER_ERROR_IDENTIFIER_TOO_LONG,
        LEXER_ERROR_OVERFLOW,
        LEXER_ERROR_STRING_TOO_LONG,
        /* parser */
        PARSER_ERROR_NONE = 3000,
        PARSER_ERROR_SYNTAX,
        PARSER_ERROR_UNEXPECTED,
        PARSER_ERROR_UNKOWN_IDENTIFIER,
        PARSER_ERROR_CONSTANT_VALUE,
        /* runtime */
        RUNTIME_ERROR_NONE = 4000,
        RUNTIME_ERROR_STACK_EMPTY,
        RUNTIME_ERROR_STACK_OVERFLOW,
        RUNTIME_ERROR_GLOBAL_TABLE_OVERFLOW,
        RUNTIME_ERROR_GLOBAL_TABLE_NOT_FOUND,
        RUNTIME_ERROR_UNKNOWN_UNARY_OPERATOR,
        RUNTIME_ERROR_UNKNOWN_BINARY_OPERATOR,
        RUNTIME_ERROR_EXPECTED_NUMBER,
        RUNTIME_ERROR_EXPECTED_INTEGER,
        RUNTIME_ERROR_EXPECTED_REAL,
        RUNTIME_ERROR_EXPECTED_BOOLEAN,
        RUNTIME_ERROR_EXPECTED_CHAR,
        RUNTIME_ERROR_EXPECTED_STRING,
        RUNTIME_ERROR_ASSIGN_TO_CONST,
        RUNTIME_ERROR_EXPECTED_VARIABLE,
        RUNTIME_ERROR_TYPE_MISMATCH,
        RUNTIME_ERROR_DIVISION_BY_ZERO,
    } error_t;

#define error_is_from_buffer(code) (code >= 1000 && code <= 1999)
#define error_is_from_lexer(code) (code >= 2000 && code <= 2999)
#define error_is_from_parser(code) (code >= 3000 && code <= 3999)
#define error_is_from_runtime(code) (code >= 4000 && code <= 4999)

#define ERROR_UNKNOWN_MESSAGE_LENGTH 31

    char *error_get_message(error_t code);

    void error_print_message(error_t code, const char *format, ...);

#ifdef __cplusplus
}
#endif

#endif /* _PS_ERROR_H */
