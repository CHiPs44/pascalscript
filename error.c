#include "error.h"

char *error_get_message(error_t code)
{
    switch (code)
    {
    case ERROR_NONE:
        return "";
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
        return "Unknown error code";
    }
}

/* EOF */
