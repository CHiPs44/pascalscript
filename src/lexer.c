#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>

#include "error.h"
#include "lexer.h"

token_t yylval;
token_t *token = &yylval;

/**
 * @brief Copy current identifier into current token
 *
 * @param buffer
 * @return int ERROR_NONE | ERROR_IDENTIFIER_TOO_LONG
 */
error_code_t copy_identifier(const char *buffer)
{
    size_t length = strlen(buffer);
    if (length > MAX_IDENTIFIER)
    {
        return ERROR_IDENTIFIER_TOO_LONG;
    }
    token->type = IDENTIFIER;
    strcpy(token->value.identifier, buffer);
    return ERROR_NONE;
}

/**
 * @brief Parse current integer value into current token
 *
 * @param buffer
 * @return int  ERROR_NONE | ERROR_OVERFLOW
 */
error_code_t copy_integer_value(const char *buffer)
{
    int val = strtoi(buffer, 0, 10);
    if (val == INT_MAX && errno == ERANGE)
    {
        return ERROR_OVERFLOW;
    }
    token->type = INT_VAL;
    token->value.int_val = val;
    return ERROR_NONE;
}

/* EOF */
