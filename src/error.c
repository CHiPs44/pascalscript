/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include "error.h"

static char error_unknown[32];

char *error_get_message(error_code_t code)
{
    switch (code)
    {
    case LEXER_ERROR_NONE:
        return "None";
    case LEXER_ERROR_IDENTIFIER_TOO_LONG:
        return "Identifier too long";
    case LEXER_ERROR_OVERFLOW:
        return "Overflow";
    case PARSER_ERROR_SYNTAX:
        return "Syntax";
    case PARSER_ERROR_UNEXPECTED:
        return "Unexpected";
    case PARSER_ERROR_UNKOWN_IDENTIFIER:
        return "Unknown identifier";
    case PARSER_ERROR_CONSTANT_VALUE:
        return "Constant value";
    default:
        snprintf(error_unknown, 31, "Unknown error code %d", code);
        return error_unknown;
    }
}

/* EOF */
