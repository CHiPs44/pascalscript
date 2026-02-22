/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>

#include "ps_error.h"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>

#include "ps_error.h"

typedef struct
{
    ps_error error;
    const char *message;
} ps_error_message;

static const ps_error_message ps_error_messages[] = {
    /* ==================== GENERAL  ==================== */
    {PS_ERROR_NONE, "None"},
    {PS_ERROR_GENERIC, "Generic"},
    {PS_ERROR_NOT_IMPLEMENTED, "Not implemented"},
    {PS_ERROR_OUT_OF_MEMORY, "Out of memory"},
    {PS_ERROR_OVERFLOW, "Overflow"},
    {PS_ERROR_UNDERFLOW, "Underflow"},
    /* ==================== BUFFER  ==================== */
    {PS_ERROR_EOF, "Unexpected end of file"},
    {PS_ERROR_OPENING_FILE, "Opening file"},
    {PS_ERROR_READING_FILE, "Reading file"},
    /* ====================LEXER  ==================== */
    {PS_ERROR_UNEXPECTED_CHARACTER, "Unexpected character"},
    {PS_ERROR_UNEXPECTED_EOF, "Unexpected end of file"},
    {PS_ERROR_IDENTIFIER_TOO_LONG, "Identifier too long"},
    {PS_ERROR_STRING_TOO_LONG, "String too long"},
    {PS_ERROR_STRING_NOT_MULTI_LINE, "String not multiline"},
    /* ==================== PARSER  ==================== */
    {PS_ERROR_SYNTAX, "Syntax"},
    {PS_ERROR_UNEXPECTED_TOKEN, "Unexpected"},
    {PS_ERROR_UNKOWN_IDENTIFIER, "Unknown identifier"},
    {PS_ERROR_CONSTANT_VALUE, "Constant value"},
    {PS_ERROR_INVALID_SUBRANGE, "Invalid subrange"},
    /* ==================== RUNTIME  ==================== */
    {PS_ERROR_ENVIRONMENT_UNDERFLOW, "Environment underflow"},
    {PS_ERROR_ENVIRONMENT_OVERFLOW, "Environment overflow"},
    {PS_ERROR_STACK_UNDERFLOW, "Stack empty"},
    {PS_ERROR_STACK_OVERFLOW, "Stack overflow"},
    {PS_ERROR_SYMBOL_NOT_ADDED, "Symbol not added"},
    {PS_ERROR_SYMBOL_NOT_FOUND, "Symbol not found"},
    {PS_ERROR_SYMBOL_EXISTS, "Symbol already exists"},
    {PS_ERROR_OPERATOR_NOT_APPLICABLE, "Operator not applicable"},
    {PS_ERROR_UNEXPECTED_TYPE, "Unexpected type"},
    {PS_ERROR_EXPECTED_VARIABLE, "Variable expected"},
    {PS_ERROR_EXPECTED_CONSTANT, "Constant expected"},
    {PS_ERROR_EXPECTED_VALUE, "Value expected"},
    {PS_ERROR_EXPECTED_NUMBER, "Number expected"},
    {PS_ERROR_EXPECTED_ORDINAL, "Scalar expected"},
    {PS_ERROR_EXPECTED_INTEGER, "Integer expected"},
    {PS_ERROR_EXPECTED_UNSIGNED, "Unsigned expected"},
    {PS_ERROR_EXPECTED_INTEGER_OR_UNSIGNED, "Integer or unsigned expected"},
    {PS_ERROR_EXPECTED_REAL, "Real expected"},
    {PS_ERROR_EXPECTED_BOOLEAN, "Boolean expected"},
    {PS_ERROR_EXPECTED_CHAR, "Char expected"},
    {PS_ERROR_EXPECTED_STRING, "String expected"},
    {PS_ERROR_EXPECTED_STRING_LENGTH, "String length expected"},
    {PS_ERROR_EXPECTED_TYPE, "Type expected"},
    {PS_ERROR_ASSIGN_TO_CONST, "Constants can't be changed"},
    {PS_ERROR_TYPE_MISMATCH, "Type mismatch"},
    {PS_ERROR_DIVISION_BY_ZERO, "Division by zero"},
    {PS_ERROR_OUT_OF_RANGE, "Out of range"},
    {PS_ERROR_INVALID_PARAMETERS, "Invalid parameters"},
    {PS_ERROR_TOO_MANY_VARIABLES, "Too many variables in declaration"},
    {PS_ERROR_PARAMETER_COUNT_MISMATCH, "Parameter count mismatch"},
    {PS_ERROR_MATH_NAN_INF, "Math result is NaN or Infinity"},
    {PS_ERROR_MAX, "MAX error?"},
};

static const size_t ps_error_messages_size = sizeof(ps_error_messages) / sizeof(ps_error_messages[0]);

const char *ps_error_get_message(ps_error error)
{
    static char message[16];

    for (size_t i = 0; i < ps_error_messages_size; i++)
    {
        if (ps_error_messages[i].error == error)
        {
            return ps_error_messages[i].message;
        }
    }

    snprintf(message, sizeof(message) - 1, "Unknown %d", error);
    return message;
}

int ps_error_sprintf(char *buffer, size_t len, ps_error error, const char *format, ...) // NOSONAR
{
    assert(buffer != NULL);
    assert(len > 0);
    int n = snprintf(buffer, len, "ERROR: %s ", ps_error_get_message(error));
    va_list args;
    va_start(args, format);
    n += vsnprintf(buffer + n, len - n, format, args); // NOSONAR
    va_end(args);
    return n;
}

int ps_error_fprintf(FILE *output, ps_error error, const char *format, ...) // NOSONAR
{
    int n = 0;
    if (output == NULL)
        output = stderr;
    va_list args;
    va_start(args, format);
    n += fprintf(output, "ERROR: %s ", ps_error_get_message(error));
    n += vfprintf(output, format, args); // NOSONAR
    n += fprintf(output, "\n");
    va_end(args);
    return n;
}

/* EOF */
