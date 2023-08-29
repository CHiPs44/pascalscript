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
    yylval.type = INT_VAL;
    yylval.value.int_val = (int)val;
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
    fprintf(stderr, " [lexer_copy_char_value %s %c]", yytext, val);
    yylval.type = CHAR_VAL;
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
    size_t len = strlen(yytext) - 2;
    fprintf(stderr, " [lexer_copy_string_value %s]", yytext);
    if (len > PS_STRING_MAX)
    {
        fprintf(stderr, "LEXER_ERROR_STRING_TOO_LONG %s %ld", yytext, len);
        return LEXER_ERROR_STRING_TOO_LONG;
    }
    yylval.type = STRING_VAL;
    strncpy(yylval.value.string_val, &yytext[1],len - 1);
    return ERROR_NONE;
}

/* EOF */
