/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>

#include "error.h"
#include "lexer.h"
#include "symbol.h"

void zzzdump(void *p)
{
    void *q = NULL;
    q = p;
    fprintf(stderr, "%p\n", q);
}

void lexer_dump_token(token_t *token)
{
    char *type;
    char value[256];

    switch (token->type)
    {
    case IDENTIFIER:
        type = "IDENTIFIER";
        snprintf(value, 255, "%s", token->value.identifier);
        break;
    case INTEGER:
        type = "INTEGER";
        snprintf(value, 255, "%d", token->value.int_val);
        break;
    case REAL:
        type = "REAL";
        snprintf(value, 255, "%f", token->value.real_val);
        break;
    case CHAR:
        type = "CHAR";
        snprintf(value, 255, "%c", token->value.char_val);
        break;
    case STRING:
        type = "STRING";
        snprintf(value, 255, "%s", token->value.string_val);
        break;
    default:
        break;
    }
    fprintf(stderr, "TOKEN: type=%s, value=%s\n", type, value);
}

/**
 * @brief Copy current identifier into current token
 *
 * @param text
 * @return int ERROR_NONE | LEXER_ERROR_IDENTIFIER_TOO_LONG
 */
error_t lexer_copy_identifier(char *text, token_t *token)
{
    char identifier[MAX_IDENTIFIER + 1];
    size_t length = strlen(text);
    if (length > MAX_IDENTIFIER)
    {
        return LEXER_ERROR_IDENTIFIER_TOO_LONG;
    }
    token->type = IDENTIFIER;
    strcpy(identifier, text);
    symbol_normalize_name(identifier);
    strcpy(token->value.identifier, identifier);
    fprintf(stderr, "\tlexer_copy_identifier: %s\n", token->value.identifier);
    return ERROR_NONE;
}

/**
 * @brief Parse current integer value into current token
 *
 * @return error_t ERROR_NONE | LEXER_ERROR_OVERFLOW
 */
error_t lexer_copy_integer_value(char *text, token_t *token)
{
    long val = strtoul(text, 0, 10);
    fprintf(stderr, " [lexer_copy_integer_value %s %ld %d %d]", text, val, errno, INT_MAX);
    if (errno == ERANGE || val > INT_MAX)
    {
        fprintf(stderr, "LEXER_ERROR_OVERFLOW %s %ld", text, val);
        return LEXER_ERROR_OVERFLOW;
    }
    token->type = INTEGER;
    token->value.int_val = (int)val;
    return ERROR_NONE;
}

/**
 * @brief Parse current real value into current token
 *
 * @return error_t ERROR_NONE | LEXER_ERROR_OVERFLOW
 */
error_t lexer_copy_real_value(char *text, token_t *token)
{
    PS_REAL val = strtod(text, NULL);
    fprintf(stderr, " [lexer_copy_real_value %s %f %d]", text, val, errno);
    if (errno == ERANGE)
    {
        fprintf(stderr, "LEXER_ERROR_OVERFLOW %s %f", text, val);
        return LEXER_ERROR_OVERFLOW;
    }
    token->type = REAL;
    token->value.real_val = val;
    return ERROR_NONE;
}

/**
 * @brief Parse current char value into current token
 *
 * @return error_t ERROR_NONE
 */
error_t lexer_copy_char_value(char *text, token_t *token)
{
    // TODO? "'X'" or "''''"
    PS_CHAR val = text[1];
    // fprintf(stderr, " [lexer_copy_char_value %s %c]", text, val);
    token->type = CHAR;
    token->value.char_val = val;
    return ERROR_NONE;
}

/**
 * @brief Parse current string value into current token
 *
 * @return error_t ERROR_NONE | LEXER_ERROR_STRING_TOO_LONG
 */
error_t lexer_copy_string_value(char *text, token_t *token)
{
    // TODO replace "''" with "'"
    size_t len = strlen(text) - 2;
    // fprintf(stderr, " [lexer_copy_string_value l=%ld s=|%s|]\n", len, text);
    // fprintf(stderr, " [lexer_copy_string_value]\n");
    if (len > PS_STRING_MAX)
    {
        fprintf(stderr, "LEXER_ERROR_STRING_TOO_LONG %ld |%s|", len, text);
        return LEXER_ERROR_STRING_TOO_LONG;
    }
    token->type = STRING;
    if (len == 0)
        strcpy(token->value.string_val, "");
    else
        strncpy(token->value.string_val, &text[1], len);
    // fprintf(stderr, " [lexer_copy_string_value l=%ld s=|%s|]\n", strlen(token->value.string_val), token->value.string_val);
    return ERROR_NONE;
}

/* EOF */
