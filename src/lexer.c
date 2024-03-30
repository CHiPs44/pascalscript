/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
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
        snprintf(value, 256, "%s", token->value.identifier);
        break;
    case INTEGER:
        type = "INTEGER";
        snprintf(value, 256, "%d", token->value.int_val);
        break;
    case REAL:
        type = "REAL";
        snprintf(value, 256, "%f", token->value.real_val);
        break;
    case CHAR:
        type = "CHAR";
        snprintf(value, 256, "%c", token->value.char_val);
        break;
    case STRING:
        type = "STRING";
        snprintf(value, 256, "%s", token->value.string_val);
        break;
    default:
        break;
    }
    fprintf(stderr, "TOKEN: type=%s, value=%s", type, value);
}

/**
 * @brief Copy current identifier into current yylval
 *
 * @param yytext
 * @return int ERROR_NONE | LEXER_ERROR_IDENTIFIER_TOO_LONG
 */
error_t lexer_copy_identifier()
{
    char identifier[MAX_IDENTIFIER + 1];
    size_t length = strlen(yytext);
    if (length > MAX_IDENTIFIER)
    {
        return LEXER_ERROR_IDENTIFIER_TOO_LONG;
    }
    yylval.type = IDENTIFIER;
    strcpy(identifier, yytext);
    symbol_normalize_name(identifier);
    strcpy(yylval.value.identifier, identifier);
    fprintf(stderr, "\tlexer_copy_identifier: %s\n", yylval.value.identifier);
    return ERROR_NONE;
}

/**
 * @brief Parse current integer value into current yylval
 *
 * @return error_t ERROR_NONE | LEXER_ERROR_OVERFLOW
 */
error_t lexer_copy_integer_value()
{
    long val = strtoul(yytext, 0, 10);
    fprintf(stderr, " [lexer_copy_integer_value %s %ld %d %d]", yytext, val, errno, INT_MAX);
    if (errno == ERANGE || val > INT_MAX)
    {
        fprintf(stderr, "LEXER_ERROR_OVERFLOW %s %ld", yytext, val);
        return LEXER_ERROR_OVERFLOW;
    }
    yylval.type = INTEGER;
    yylval.value.int_val = (int)val;
    return ERROR_NONE;
}

/**
 * @brief Parse current real value into current yylval
 *
 * @return error_t ERROR_NONE | LEXER_ERROR_OVERFLOW
 */
error_t lexer_copy_real_value()
{
    PS_REAL val = strtod(yytext, NULL);
    fprintf(stderr, " [lexer_copy_real_value %s %f %d]", yytext, val, errno);
    if (errno == ERANGE)
    {
        fprintf(stderr, "LEXER_ERROR_OVERFLOW %s %f", yytext, val);
        return LEXER_ERROR_OVERFLOW;
    }
    yylval.type = REAL;
    yylval.value.real_val = val;
    return ERROR_NONE;
}

/**
 * @brief Parse current char value into current yylval
 *
 * @return error_t ERROR_NONE
 */
error_t lexer_copy_char_value()
{
    // "'X'" or "''''"
    PS_CHAR val = yytext[1];
    // fprintf(stderr, " [lexer_copy_char_value %s %c]", yytext, val);
    yylval.type = CHAR;
    yylval.value.char_val = val;
    return ERROR_NONE;
}

/**
 * @brief Parse current string value into current yylval
 *
 * @return error_t ERROR_NONE | LEXER_ERROR_STRING_TOO_LONG
 */
error_t lexer_copy_string_value()
{
    // TODO replace "''" with "'"
    size_t len = strlen(yytext) - 2;
    // fprintf(stderr, " [lexer_copy_string_value l=%ld s=|%s|]\n", len, yytext);
    // fprintf(stderr, " [lexer_copy_string_value]\n");
    if (len > PS_STRING_MAX)
    {
        fprintf(stderr, "LEXER_ERROR_STRING_TOO_LONG %ld |%s|", len, yytext);
        return LEXER_ERROR_STRING_TOO_LONG;
    }
    yylval.type = STRING;
    if (len == 0)
        strcpy(yylval.value.string_val, "");
    else
        strncpy(yylval.value.string_val, &yytext[1], len);
    // fprintf(stderr, " [lexer_copy_string_value l=%ld s=|%s|]\n", strlen(yylval.value.string_val), yylval.value.string_val);
    return ERROR_NONE;
}

/* EOF */
