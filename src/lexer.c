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

keyword_t keywords[] = {
    {TOKEN_PROGRAM, "PROGRAM", false},
    {TOKEN_CONST, "CONST", false},
    {TOKEN_VAR, "VAR", false},
    {TOKEN_TYPE, "TYPE", false},
    {TOKEN_BEGIN, "BEGIN", false},
    {TOKEN_END, "END", false},
    {TOKEN_INTEGER, "INTEGER", false},
    {TOKEN_BOOLEAN, "BOOLEAN", false},
    {TOKEN_CHAR, "CHAR", false},
    {TOKEN_STRING, "STRING", false},
    {TOKEN_FALSE, "FALSE", false},
    {TOKEN_TRUE, "TRUE", false},
    {TOKEN_FUNCTION, "FUNCTION", false},
    {TOKEN_PROCEDURE, "PROCEDURE", false},
    // Ponctuation
    {TOKEN_ASSIGN, ":=", false},
    {TOKEN_CARET, "^", true},
    {TOKEN_COLON, ":", true},
    {TOKEN_COMMA, ","},
    {TOKEN_DOT_DOT, "..", false},
    {TOKEN_DOT, ".", true},
    {TOKEN_SEMI_COLON, ";", true},
    // Operators
    {TOKEN_ADD, "+", true},
    {TOKEN_SUB, "-", true},
    {TOKEN_MUL, "*", true},
    {TOKEN_DIV_REAL, "/", true},
    {TOKEN_DIV, "DIV", false},
    {TOKEN_MOD, "MOD", false},
    // Comparison operators
    {TOKEN_EQ, "=", true},
    {TOKEN_NE, "<>", true},
    {TOKEN_LT, "<", true},
    {TOKEN_LE, "<=", true},
    {TOKEN_GT, ">", true},
    {TOKEN_GE, ">=", true},
    // Logical/binary operators
    {TOKEN_AND, "AND", false},
    {TOKEN_OR, "OR", false},
    {TOKEN_XOR, "XOR", false},
    {TOKEN_NOT, "NOT", false},
    {TOKEN_LSHIFT, "<<", true},
    {TOKEN_RSHIFT, ">>", true}};

void lexer_dump_token(token_t *token)
{
    char *type;
    char value[256];

    switch (token->type)
    {
    case TOKEN_IDENTIFIER:
        type = "IDENTIFIER";
        snprintf(value, 255, "%s", token->value.identifier);
        break;
    case TOKEN_INTEGER_VALUE:
        type = "INTEGER";
        snprintf(value, 255, "%d", token->value.int_val);
        break;
    case TOKEN_REAL_VALUE:
        type = "REAL";
        snprintf(value, 255, "%f", token->value.real_val);
        break;
    case TOKEN_CHAR_VALUE:
        type = "CHAR";
        snprintf(value, 255, "%c", token->value.char_val);
        break;
    case TOKEN_STRING_VALUE:
        type = "STRING";
        snprintf(value, 256, "%s", token->value.string_val);
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
    token->type = TOKEN_IDENTIFIER;
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
    token->type = TOKEN_INTEGER_VALUE;
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
    token->type = TOKEN_REAL_VALUE;
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
    token->type = TOKEN_CHAR_VALUE;
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
    token->type = TOKEN_STRING_VALUE;
    if (len == 0)
        strcpy(token->value.string_val, "");
    else
        strncpy(token->value.string_val, &text[1], len);
    // fprintf(stderr, " [lexer_copy_string_value l=%ld s=|%s|]\n", strlen(token->value.string_val), token->value.string_val);
    return ERROR_NONE;
}

/* EOF */
