/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <stdarg.h>

#include "ps_error.h"

static char error_unknown[ERROR_UNKNOWN_MESSAGE_LENGTH + 1];

char *ps_error_get_message(ps_error_t code)
{
    switch (code)
    {
    /* GENERAL ERRORS */
    case ERROR_ZERO:
        return "None";
    case ERROR_NOT_IMPLEMENTED:
        return "Not implemented";
    /* BUFFER ERRORS */
    case BUFFER_ERROR_OPENING_FILE:
        return "Opening file";
    case BUFFER_ERROR_READING_FILE:
        return "Reading file";
    case BUFFER_ERROR_OUT_OF_MEMORY:
        return "Out of memory";
    /* LEXER ERRORS */
    case LEXER_ERROR_NONE:
        return "None";
    case LEXER_ERROR_UNEXPECTED_CHARACTER:
        return "Unexpected character";
    case LEXER_ERROR_UNEXPECTED_EOF:
        return "Unexpected end of file";
    case LEXER_ERROR_EXPECTED_TOKEN:
        return "Expected token";
    case LEXER_ERROR_UNEXPECTED_TOKEN:
        return "Unexpected token";
    case LEXER_ERROR_BUFFER_OVERFLOW:
        return "Buffer overflow";
    case LEXER_ERROR_IDENTIFIER_TOO_LONG:
        return "Identifier too long";
    case LEXER_ERROR_OVERFLOW:
        return "Overflow";
    case LEXER_ERROR_STRING_TOO_LONG:
        return "String too long";
    /* PARSER ERRORS */
    case PARSER_ERROR_NONE:
        return "None";
    case PARSER_ERROR_SYNTAX:
        return "Syntax";
    case PARSER_ERROR_UNEXPECTED:
        return "Unexpected";
    case PARSER_ERROR_UNKOWN_IDENTIFIER:
        return "Unknown identifier";
    case PARSER_ERROR_CONSTANT_VALUE:
        return "Constant value";
    /* RUNTIME ERRORS */
    case RUNTIME_ERROR_NONE:
        return "None";
    case RUNTIME_ERROR_STACK_EMPTY:
        return "Stack empty";
    case RUNTIME_ERROR_STACK_OVERFLOW:
        return "Stack overflow";
    case RUNTIME_ERROR_GLOBAL_TABLE_OVERFLOW:
        return "Global table full";
    case RUNTIME_ERROR_GLOBAL_TABLE_NOT_FOUND:
        return "Symbol not foun in global table";
    case RUNTIME_ERROR_UNKNOWN_UNARY_OPERATOR:
        return "Unknown unary operator";
    case RUNTIME_ERROR_UNKNOWN_BINARY_OPERATOR:
        return "Unknown binary operator";
    case RUNTIME_ERROR_ASSIGN_TO_CONST:
        return "Constant can't be changed";
    case RUNTIME_ERROR_EXPECTED_VARIABLE:
        return "Variable expected";
    case RUNTIME_ERROR_EXPECTED_INTEGER_OR_REAL:
        return "Number expected";
    case RUNTIME_ERROR_EXPECTED_INTEGER:
        return "Integer expected";
    case RUNTIME_ERROR_EXPECTED_REAL:
        return "Real expected";
    case RUNTIME_ERROR_EXPECTED_BOOLEAN:
        return "Boolean expected";
    case RUNTIME_ERROR_EXPECTED_CHAR:
        return "Char expected";
    case RUNTIME_ERROR_EXPECTED_STRING:
        return "String expected";
    case RUNTIME_ERROR_TYPE_MISMATCH:
        return "Type mismatch";
    case RUNTIME_ERROR_DIVISION_BY_ZERO:
        return "Division by zero";
    default:
        snprintf(error_unknown, ERROR_UNKNOWN_MESSAGE_LENGTH, "Unknown error %d", code);
        return error_unknown;
    }
}

void ps_error_print_message(ps_error_t code, const char *format, ...)
{
    va_list args;
    va_start(args, format);
    char *message = ps_error_get_message(code);
    fprintf(stderr, "ERROR: %s ", message);
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
    va_end(args);
}

/* EOF */
