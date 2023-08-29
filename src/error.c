/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include "error.h"

static char error_unknown[32];

char *error_get_message(error_t code)
{
    // char *from = error_is_from_lexer(code)     ? "LEXER"
    //              : error_is_from_parser(code)  ? "PARSER"
    //              : error_is_from_runtime(code) ? "RUNTIME"
    //                                            : "???";
    switch (code)
    {
    case ERROR_NONE:
        return "None";
    /*
     * LEXER ERRORS
     */
    case LEXER_ERROR_IDENTIFIER_TOO_LONG:
        return "Identifier too long";
    case LEXER_ERROR_OVERFLOW:
        return "Overflow";
    case LEXER_ERROR_STRING_TOO_LONG:
        return "String too long";
    /*
     * PARSER ERRORS
     */
    case PARSER_ERROR_SYNTAX:
        return "Syntax";
    case PARSER_ERROR_UNEXPECTED:
        return "Unexpected";
    case PARSER_ERROR_UNKOWN_IDENTIFIER:
        return "Unknown identifier";
    case PARSER_ERROR_CONSTANT_VALUE:
        return "Constant value";
    /*
     * RUNTIME ERRORS
     */
    case RUNTIME_STACK_EMPTY:
        return "Stack empty";
    case RUNTIME_STACK_OVERFLOW:
        return "Stack overflow";
    case RUNTIME_GLOBAL_TABLE_FULL:
        return "Global table full";
    case RUNTIME_UNKNOWN_UNARY_OPERATOR:
        return "Unknown unary operator";
    case RUNTIME_UNKNOWN_BINARY_OPERATOR:
        return "Unknown binary operator";
    case RUNTIME_ASSIGN_TO_CONST:
        return "Constant can't be changed";
    case RUNTIME_EXPECTED_VARIABLE:
        return "Variable expected";
    case RUNTIME_EXPECTED_NUMBER:
        return "Number expected";
    case RUNTIME_TYPE_MISMATCH:
        return "Type mismatch";
    case RUNTIME_DIVISION_BY_ZERO:
        return "Division by zero";
    default:
        snprintf(error_unknown, 31, "Unknown error code %d", code);
        return error_unknown;
    }
}

/* EOF */
