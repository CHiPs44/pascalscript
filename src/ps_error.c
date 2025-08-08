/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdarg.h>
#include <stdio.h>

#include "ps_error.h"

char *ps_error_get_message(ps_error error)
{
    static char message[16];
    switch (error)
    {
    /* ==================== GENERAL  ==================== */
    case PS_ERROR_NONE:
        return "None";
    case PS_ERROR_GENERIC:
        return "Generic";
    case PS_ERROR_NOT_IMPLEMENTED:
        return "Not implemented";
    case PS_ERROR_OUT_OF_MEMORY:
        return "Out of memory";
    case PS_ERROR_OVERFLOW:
        return "Overflow";
    /* ==================== BUFFER  ==================== */
    case PS_ERROR_EOF:
        return "Unexpected end of file";
    case PS_ERROR_OPENING_FILE:
        return "Opening file";
    case PS_ERROR_READING_FILE:
        return "Reading file";
    /* ====================LEXER  ==================== */
    case PS_ERROR_UNEXPECTED_CHARACTER:
        return "Unexpected character";
    case PS_ERROR_UNEXPECTED_EOF:
        return "Unexpected end of file";
    case PS_ERROR_IDENTIFIER_TOO_LONG:
        return "Identifier too long";
    case PS_ERROR_STRING_TOO_LONG:
        return "String too long";
    case PS_ERROR_STRING_NOT_MULTI_LINE:
        return "String not multiline";
    /* ==================== PARSER  ==================== */
    case PS_ERROR_SYNTAX:
        return "Syntax";
    case PS_ERROR_UNEXPECTED_TOKEN:
        return "Unexpected";
    case PS_ERROR_UNKOWN_IDENTIFIER:
        return "Unknown identifier";
    case PS_ERROR_CONSTANT_VALUE:
        return "Constant value";
    /* ==================== RUNTIME  ==================== */
    case PS_ERROR_ENVIRONMENT_UNDERFLOW:
        return "Environment underflow";
    case PS_ERROR_ENVIRONMENT_OVERFLOW:
        return "Environment overflow";
    case PS_ERROR_STACK_UNDERFLOW:
        return "Stack empty";
    case PS_ERROR_STACK_OVERFLOW:
        return "Stack overflow";
    case PS_ERROR_SYMBOL_NOT_ADDED:
        return "Symbol not added";
    case PS_ERROR_SYMBOL_NOT_FOUND:
        return "Symbol not found";
    case PS_ERROR_SYMBOL_EXISTS:
        return "Symbol alreayd exists";
    case PS_ERROR_OPERATOR_NOT_APPLICABLE:
        return "Operator not applicable";
    case PS_ERROR_UNEXPECTED_TYPE:
        return "Unexpected type";
    case PS_ERROR_EXPECTED_VARIABLE:
        return "Variable expected";
    case PS_ERROR_EXPECTED_CONSTANT:
        return "Constant expected";
    case PS_ERROR_EXPECTED_VALUE:
        return "Value expected";
    case PS_ERROR_EXPECTED_NUMBER:
        return "Number expected";
    case PS_ERROR_EXPECTED_ORDINAL:
        return "Scalar expected";
    case PS_ERROR_EXPECTED_INTEGER:
        return "Integer expected";
    case PS_ERROR_EXPECTED_UNSIGNED:
        return "Unsigned expected";
    case PS_ERROR_EXPECTED_INTEGER_OR_UNSIGNED:
        return "Integer or unsigned expected";
    case PS_ERROR_EXPECTED_REAL:
        return "Real expected";
    case PS_ERROR_EXPECTED_BOOLEAN:
        return "Boolean expected";
    case PS_ERROR_EXPECTED_CHAR:
        return "Char expected";
    case PS_ERROR_EXPECTED_STRING:
        return "String expected";
    case PS_ERROR_EXPECTED_STRING_LENGTH:
        return "String length expected";
    case PS_ERROR_ASSIGN_TO_CONST:
        return "Constants can't be changed";
    case PS_ERROR_TYPE_MISMATCH:
        return "Type mismatch";
    case PS_ERROR_DIVISION_BY_ZERO:
        return "Division by zero";
    case PS_ERROR_OUT_OF_RANGE:
        return "Out of range";
    case PS_ERROR_INVALID_PARAMETERS:
        return "Invalid parameters";
    case PS_ERROR_TOO_MANY_VARIABLES:
        return "Too many variables in declaration";
    case PS_ERROR_MAX:
        return "MAX error?";
    }
    snprintf(message, sizeof(message) - 1, "Unknown %d", error);
    return message;
}

int ps_error_sprintf(char *buffer, size_t len, ps_error error, const char *format, ...)
{
    int n = snprintf(buffer, len, "ERROR: %s ", ps_error_get_message(error));
    va_list args;
    va_start(args, format);
    n += vsnprintf(buffer + n, len - n, format, args);
    va_end(args);
    return n;
}

int ps_error_fprintf(FILE *output, ps_error error, const char *format, ...)
{
    int n = 0;
    if (output == NULL)
        output = stderr;
    va_list args;
    va_start(args, format);
    n += fprintf(output, "ERROR: %s ", ps_error_get_message(error));
    n += vfprintf(output, format, args);
    n += fprintf(output, "\n");
    va_end(args);
    return n;
}

/* EOF */
