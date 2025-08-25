/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_ERROR_H
#define _PS_ERROR_H

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum e_ps_error
    {
        /* -------------------- GENERAL -------------------- */
        PS_ERROR_NONE = 0,
        PS_ERROR_GENERIC,
        PS_ERROR_NOT_IMPLEMENTED,
        PS_ERROR_OUT_OF_MEMORY,
        PS_ERROR_OVERFLOW,
        /* -------------------- BUFFER  -------------------- */
        PS_ERROR_EOF,
        PS_ERROR_OPENING_FILE,
        PS_ERROR_READING_FILE,
        /* --------------------  LEXER  -------------------- */
        PS_ERROR_UNEXPECTED_CHARACTER,
        PS_ERROR_UNEXPECTED_EOF,
        PS_ERROR_IDENTIFIER_TOO_LONG,
        PS_ERROR_STRING_TOO_LONG,
        PS_ERROR_STRING_NOT_MULTI_LINE,
        /* -------------------- PARSER  -------------------- */
        PS_ERROR_SYNTAX,
        PS_ERROR_UNEXPECTED_TOKEN,
        PS_ERROR_UNKOWN_IDENTIFIER,
        PS_ERROR_CONSTANT_VALUE,
        /* -------------------- RUNTIME -------------------- */
        PS_ERROR_ENVIRONMENT_UNDERFLOW,
        PS_ERROR_ENVIRONMENT_OVERFLOW,
        PS_ERROR_STACK_UNDERFLOW,
        PS_ERROR_STACK_OVERFLOW,
        PS_ERROR_SYMBOL_NOT_ADDED,
        PS_ERROR_SYMBOL_NOT_FOUND,
        PS_ERROR_SYMBOL_EXISTS,
        PS_ERROR_OPERATOR_NOT_APPLICABLE,
        PS_ERROR_UNEXPECTED_TYPE,
        PS_ERROR_EXPECTED_VALUE,
        PS_ERROR_EXPECTED_NUMBER,
        PS_ERROR_EXPECTED_INTEGER,
        PS_ERROR_EXPECTED_UNSIGNED,
        PS_ERROR_EXPECTED_INTEGER_OR_UNSIGNED,
        PS_ERROR_EXPECTED_REAL,
        PS_ERROR_EXPECTED_ORDINAL,
        PS_ERROR_EXPECTED_BOOLEAN,
        PS_ERROR_EXPECTED_CHAR,
        PS_ERROR_EXPECTED_STRING,
        PS_ERROR_EXPECTED_STRING_LENGTH,
        PS_ERROR_EXPECTED_TYPE,
        PS_ERROR_EXPECTED_VARIABLE,
        PS_ERROR_EXPECTED_CONSTANT,
        PS_ERROR_ASSIGN_TO_CONST,
        PS_ERROR_TYPE_MISMATCH,
        PS_ERROR_DIVISION_BY_ZERO,
        PS_ERROR_OUT_OF_RANGE,
        PS_ERROR_INVALID_PARAMETERS,
        PS_ERROR_TOO_MANY_VARIABLES,
        PS_ERROR_PARAMETER_COUNT_MISMATCH,
        // ...
        PS_ERROR_MAX = UINT8_MAX
    } __attribute__((__packed__)) ps_error;

#define PS_ERROR_SIZE sizeof(ps_error)

    char *ps_error_get_message(ps_error error);

    int ps_error_sprintf(char *buffer, size_t len, ps_error error, const char *format, ...);
    int ps_error_fprintf(FILE *output, ps_error error, const char *format, ...);

#ifdef __cplusplus
}
#endif

#endif /* _PS_ERROR_H */
