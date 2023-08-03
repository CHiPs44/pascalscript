#include <stdio.h>
#include "error.h"

static char error_unknown[31];

char *error_get_message(error_code_t code)
{
    switch (code)
    {
    case ERROR_NONE:
        return "No";
    case ERROR_IDENTIFIER_TOO_LONG:
        return "Identifier too long";
    case ERROR_OVERFLOW:
        return "Overflow";
    case ERROR_SYNTAX:
        return "Syntax";
    case ERROR_UNEXPECTED:
        return "Unexpected";
    case ERROR_UNKOWN_IDENTIFIER:
        return "Unknown identifier";
    case ERROR_CONSTANT_VALUE:
        return "Constant value";
    default:
        snprintf(error_unknown, 31, "Unknown error code %d", code);
        return error_unknown;
    }
}

/* EOF */
