/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _ERROR_H
#define _ERROR_H

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum _error_t
    {
        ERROR_NONE = 0,
        /* general */
        ERROR_OUT_OF_MEMORY,
        ERROR_NOT_IMPLEMENTED,
        /* lexer */
        LEXER_ERROR_UNEXPECTED_CHARACTER = 1000,
        LEXER_ERROR_UNEXPECTED_EOF,
        LEXER_ERROR_EXPECTED_TOKEN,
        LEXER_ERROR_UNEXPECTED_TOKEN,
        LEXER_ERROR_BUFFER_OVERFLOW,
        LEXER_ERROR_IDENTIFIER_TOO_LONG,
        LEXER_ERROR_OVERFLOW,
        LEXER_ERROR_STRING_TOO_LONG,
        /* parser */
        PARSER_ERROR_SYNTAX = 2000,
        PARSER_ERROR_UNEXPECTED,
        PARSER_ERROR_UNKOWN_IDENTIFIER,
        PARSER_ERROR_CONSTANT_VALUE,
        /* runtime */
        RUNTIME_ERROR_STACK_EMPTY = 3000,
        RUNTIME_ERROR_STACK_OVERFLOW,
        RUNTIME_ERROR_GLOBAL_TABLE_FULL,
        RUNTIME_ERROR_UNKNOWN_UNARY_OPERATOR,
        RUNTIME_ERROR_UNKNOWN_BINARY_OPERATOR,
        RUNTIME_ERROR_EXPECTED_NUMBER,
        RUNTIME_ERROR_ASSIGN_TO_CONST,
        RUNTIME_ERROR_EXPECTED_VARIABLE,
        RUNTIME_ERROR_TYPE_MISMATCH,
        RUNTIME_ERROR_DIVISION_BY_ZERO,
    } error_t;

#define error_is_from_lexer(code) (code >= 1000 && code <= 1999)
#define error_is_from_parser(code) (code >= 2000 && code <= 2999)
#define error_is_from_runtime(code) (code >= 3000 && code <= 3999)

#define ERROR_UNKNOWN_MESSAGE_LENGTH 31

    extern char *error_get_message(error_t code);

#ifdef __cplusplus
}
#endif

#endif /* _ERROR_H */
