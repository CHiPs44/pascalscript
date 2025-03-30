/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <stdarg.h>

#include "ps_error.h"

char *ps_error_get_message(ps_error error)
{
    static char message[16];
    switch (error)
    {
    /* ==================== GENERAL  ==================== */
    case PS_ERROR_ZERO:
        return "None";
    case PS_ERROR_NOT_IMPLEMENTED:
        return "Not implemented";
    /* ==================== BUFFER  ==================== */
    case PS_BUFFER_ERROR_NONE:
        return "None";
    case PS_BUFFER_ERROR_OPENING_FILE:
        return "Opening file";
    case PS_BUFFER_ERROR_READING_FILE:
        return "Reading file";
    case PS_BUFFER_ERROR_OUT_OF_MEMORY:
        return "Out of memory";
    case PS_BUFFER_ERROR_OVERFLOW:
        return "Overflow: too much text";
    case PS_BUFFER_ERROR_OVERFLOW_COLUMNS:
        return "Overflow: line too long";
    case PS_BUFFER_ERROR_OVERFLOW_LINES:
        return "Overflow: too much lines";
    case PS_BUFFER_ERROR_EOF:
        return "Unexpected end of file";
    /* ====================LEXER  ==================== */
    case PS_LEXER_ERROR_NONE:
        return "None";
    case PS_LEXER_ERROR_UNEXPECTED_CHARACTER:
        return "Unexpected character";
    case PS_LEXER_ERROR_UNEXPECTED_EOF:
        return "Unexpected end of file";
    case PS_LEXER_ERROR_IDENTIFIER_TOO_LONG:
        return "Identifier too long";
    case PS_LEXER_ERROR_OVERFLOW:
        return "Overflow";
    case PS_LEXER_ERROR_STRING_TOO_LONG:
        return "String too long";
    case PS_LEXER_ERROR_STRING_NOT_MULTI_LINE:
        return "String not multiline";
    /* ==================== PARSER  ==================== */
    case PS_PARSER_ERROR_NONE:
        return "None";
    case PS_PARSER_ERROR_SYNTAX:
        return "Syntax";
    case PS_PARSER_ERROR_UNEXPECTED_TOKEN:
        return "Unexpected";
    case PS_PARSER_ERROR_UNKOWN_IDENTIFIER:
        return "Unknown identifier";
    case PS_PARSER_ERROR_CONSTANT_VALUE:
        return "Constant value";
    /* ==================== RUNTIME  ==================== */
    case PS_RUNTIME_ERROR_NONE:
        return "None";
    case PS_RUNTIME_ERROR_OUT_OF_MEMORY:
        return "Out of memory";
    case PS_RUNTIME_ERROR_STACK_UNDERFLOW:
        return "Stack empty";
    case PS_RUNTIME_ERROR_STACK_OVERFLOW:
        return "Stack overflow";
    case PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED:
        return "Symbol not added";
    case PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND:
        return "Symbol not found";
    case PS_RUNTIME_ERROR_UNKNOWN_UNARY_OPERATOR:
        return "Unknown unary operator";
    case PS_RUNTIME_ERROR_UNKNOWN_BINARY_OPERATOR:
        return "Unknown binary operator";
    case PS_RUNTIME_ERROR_UNEXPECTED_TYPE:
        return "Unexpected type";
    case PS_RUNTIME_ERROR_EXPECTED_VARIABLE:
        return "Variable expected";
    case PS_RUNTIME_ERROR_EXPECTED_CONSTANT:
        return "Constant expected";
    case PS_RUNTIME_ERROR_EXPECTED_VALUE:
        return "Value expected";
    case PS_RUNTIME_ERROR_EXPECTED_NUMBER:
        return "Number expected";
    case PS_RUNTIME_ERROR_EXPECTED_ORDINAL:
        return "Scalar expected";
    case PS_RUNTIME_ERROR_EXPECTED_INTEGER:
        return "Integer expected";
    case PS_RUNTIME_ERROR_EXPECTED_UNSIGNED:
        return "Unsigned expected";
    case PS_RUNTIME_ERROR_EXPECTED_INTEGER_OR_UNSIGNED:
        return "Integer or unsigned expected";
    case PS_RUNTIME_ERROR_EXPECTED_REAL:
        return "Real expected";
    case PS_RUNTIME_ERROR_EXPECTED_BOOLEAN:
        return "Boolean expected";
    case PS_RUNTIME_ERROR_EXPECTED_CHAR:
        return "Char expected";
    case PS_RUNTIME_ERROR_EXPECTED_STRING:
        return "String expected";
    case PS_RUNTIME_ERROR_ASSIGN_TO_CONST:
        return "Constant can't be changed";
    case PS_RUNTIME_ERROR_TYPE_MISMATCH:
        return "Type mismatch";
    case PS_RUNTIME_ERROR_DIVISION_BY_ZERO:
        return "Division by zero";
    case PS_RUNTIME_ERROR_OUT_OF_RANGE:
        return "Out of range";
    case PS_RUNTIME_ERROR_INVALID_PARAMETERS:
        return "Invalid parameters";
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
