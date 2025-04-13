/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_ERROR_H
#define _PS_ERROR_H

#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum e_ps_error
    {
        /* -------------------- GENERAL -------------------- */
        PS_ERROR_ZERO = 0,
        PS_ERROR_NOT_IMPLEMENTED,
        /* -------------------- BUFFER  -------------------- */
        PS_BUFFER_ERROR_NONE = 0x10,
        PS_BUFFER_ERROR_EOF,
        PS_BUFFER_ERROR_OPENING_FILE,
        PS_BUFFER_ERROR_READING_FILE,
        PS_BUFFER_ERROR_OUT_OF_MEMORY,
        PS_BUFFER_ERROR_OVERFLOW,
        PS_BUFFER_ERROR_OVERFLOW_COLUMNS,
        PS_BUFFER_ERROR_OVERFLOW_LINES,
        /* --------------------  LEXER  -------------------- */
        PS_LEXER_ERROR_NONE = 0x20,
        PS_LEXER_ERROR_UNEXPECTED_CHARACTER,
        PS_LEXER_ERROR_UNEXPECTED_EOF,
        PS_LEXER_ERROR_IDENTIFIER_TOO_LONG,
        PS_LEXER_ERROR_OVERFLOW,
        PS_LEXER_ERROR_STRING_TOO_LONG,
        PS_LEXER_ERROR_STRING_NOT_MULTI_LINE,
        /* -------------------- PARSER  -------------------- */
        PS_PARSER_ERROR_NONE = 0x30,
        PS_PARSER_ERROR_SYNTAX,
        PS_PARSER_ERROR_UNEXPECTED_TOKEN,
        PS_PARSER_ERROR_UNKOWN_IDENTIFIER,
        PS_PARSER_ERROR_CONSTANT_VALUE,
        /* -------------------- RUNTIME -------------------- */
        PS_RUNTIME_ERROR_NONE = 0x80,
        PS_RUNTIME_ERROR_OUT_OF_MEMORY,
        PS_RUNTIME_ERROR_STACK_UNDERFLOW,
        PS_RUNTIME_ERROR_STACK_OVERFLOW,
        PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED,
        PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND,
        PS_RUNTIME_ERROR_UNKNOWN_UNARY_OPERATOR,
        PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE,
        PS_RUNTIME_ERROR_UNEXPECTED_TYPE,
        PS_RUNTIME_ERROR_EXPECTED_VALUE,
        PS_RUNTIME_ERROR_EXPECTED_NUMBER,
        PS_RUNTIME_ERROR_EXPECTED_INTEGER,
        PS_RUNTIME_ERROR_EXPECTED_UNSIGNED,
        PS_RUNTIME_ERROR_EXPECTED_INTEGER_OR_UNSIGNED,
        PS_RUNTIME_ERROR_EXPECTED_REAL,
        PS_RUNTIME_ERROR_EXPECTED_ORDINAL,
        PS_RUNTIME_ERROR_EXPECTED_BOOLEAN,
        PS_RUNTIME_ERROR_EXPECTED_CHAR,
        PS_RUNTIME_ERROR_EXPECTED_STRING,
        PS_RUNTIME_ERROR_ASSIGN_TO_CONST,
        PS_RUNTIME_ERROR_EXPECTED_VARIABLE,
        PS_RUNTIME_ERROR_EXPECTED_CONSTANT,
        PS_RUNTIME_ERROR_TYPE_MISMATCH,
        PS_RUNTIME_ERROR_DIVISION_BY_ZERO,
        PS_RUNTIME_ERROR_OUT_OF_RANGE,
        PS_RUNTIME_ERROR_INVALID_PARAMETERS,
        PS_PARSER_ERROR_TOO_MANY_VARIABLES,
        // ...
        PS_ERROR_MAX = UINT8_MAX
    } __attribute__((__packed__)) ps_error;

#define PS_ERROR_SIZE sizeof(ps_error)

#define ps_error_is_from_buffer(error) (error >= PS_BUFFER_ERROR_NONE && error <= PS_BUFFER_ERROR_NONE + 0xf)
#define ps_error_is_from_lexer(error) (error >= PS_LEXER_ERROR_NONE && error <= PS_LEXER_ERROR_NONE + 0xf)
#define ps_error_is_from_parser(error) (error >= PS_PARSER_ERROR_NONE && error <= PS_PARSER_ERROR_NONE + 0xf)
#define ps_error_is_from_runtime(error) (error >= PS_RUNTIME_ERROR_NONE && error <= PS_RUNTIME_ERROR_NONE + 0x7f)

    char *ps_error_get_message(ps_error error);

    int ps_error_sprintf(char *buffer, size_t len, ps_error error, const char *format, ...);
    int ps_error_fprintf(FILE *output, ps_error error, const char *format, ...);

#ifdef __cplusplus
}
#endif

#endif /* _PS_ERROR_H */
