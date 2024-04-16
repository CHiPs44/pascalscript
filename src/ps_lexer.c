/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_error.h"
#include "ps_lexer.h"
#include "ps_buffer.h"
#include "ps_symbol.h"

void lexer_dump_token(token_t *token)
{
    char *type;
    char value[256];

    fprintf(stderr, "TOKEN: %d\n", token->type);

    switch (token->type)
    {
    case TOKEN_IDENTIFIER:
        type = "IDENTIFIER";
        snprintf(value, 256, "%s", token->value.identifier);
        break;
    case TOKEN_INTEGER_VALUE:
        type = "INTEGER";
        snprintf(value, 256, "%d", token->value.i);
        break;
    case TOKEN_REAL_VALUE:
        type = "REAL";
        snprintf(value, 256, "%f", token->value.r);
        break;
    case TOKEN_CHAR_VALUE:
        type = "CHAR";
        snprintf(value, 256, "%c", token->value.c);
        break;
    case TOKEN_STRING_VALUE:
        type = "STRING";
        snprintf(value, 256, "%s", token->value.s);
        break;
    default:
        type = "UNKNOWN";
        snprintf(value, 256, "%s", "?");
        break;
    }
    fprintf(stderr, "TOKEN: type=%s, value=%s\n", type, value);
}

void lexer_reset_cursor(lexer_t *lexer)
{
    lexer->current_line = 0;
    lexer->current_column = 0;
    lexer->current_char = '\0';
}

char lexer_peek_char(lexer_t *lexer)
{
    return lexer->current_char;
}

char buffer_read_next_char(lexer_t *lexer)
{
    if (lexer->current_line >= 0 && lexer->current_line < lexer->buffer.line_count)
    {
        if (lexer->current_column >= 0 &&
            lexer->current_column <= lexer->buffer.line_lengths[lexer->current_line])
        {
            lexer->current_char = lexer->buffer.line_starts[lexer->current_line][lexer->current_column];
            // Advance to next char
            lexer->current_column += 1;
            if (lexer->current_column > lexer->buffer.line_lengths[lexer->current_line])
            {
                lexer->current_line += 1;
                lexer->current_column = 0;
            }
        }
    }
    return lexer->current_char;
}

char buffer_peek_next_char(lexer_t *lexer)
{
    char next_char = '\0';
    if (lexer->current_line >= 0 && lexer->current_line < lexer->buffer.line_count)
    {
        if (lexer->current_column >= 0 && lexer->current_column <= lexer->buffer.line_lengths[lexer->current_line])
        {
            next_char = lexer->buffer.line_starts[lexer->current_line][lexer->current_column];
        }
    }
    return next_char;
}

error_t lexer_skip_whitespace(vm_t *vm, bool *changed)
{
    char c;
    c = lexer_peek_char(vm);
    *changed = false;
    while (isspace(c))
    {
        c = buffer_read_next_char(vm);
        *changed = true;
    }
    return LEXER_ERROR_NONE;
}

error_t lexer_skip_comment1(vm_t *vm, bool *changed)
{
    char c;
    c = lexer_peek_char(vm);
    *changed = false;
    if (c == '{')
    {
        while (c != '}')
        {
            c = buffer_read_next_char(vm);
            *changed = true;
            if (c == '\0')
            {
                return LEXER_ERROR_UNEXPECTED_EOF;
            }
        }
    }
    return LEXER_ERROR_NONE;
}

error_t lexer_skip_comment2(vm_t *vm, bool *changed)
{
    char c1, c2;
    c1 = lexer_peek_char(vm);
    c2 = buffer_peek_next_char(vm);
    *changed = false;
    if (c1 == '(' && c2 == '*')
    {
        while (c1 != '*' && c2 != ')')
        {
            c1 = buffer_read_next_char(vm);
            *changed = true;
            if (c1 == '\0')
            {
                return LEXER_ERROR_UNEXPECTED_EOF;
            }
            c2 = buffer_peek_next_char(vm);
        }
    }
    return LEXER_ERROR_NONE;
}

error_t lexer_skip_whitespace_and_comments(vm_t *vm)
{
    error_t error;
    bool changed1, changed2, changed3 = true;
    while (changed1 || changed2 || changed3)
    {
        error = lexer_skip_whitespace(vm, &changed1);
        if (error != LEXER_ERROR_NONE)
            return error;
        error = lexer_skip_comment1(vm, &changed2);
        if (error != LEXER_ERROR_NONE)
            return error;
        error = lexer_skip_comment2(vm, &changed3);
        if (error != LEXER_ERROR_NONE)
            return error;
    }
    return error;
}

bool lexer_read_identifier_or_keyword(vm_t *vm)
{
    char buffer[BUFFER_MAX_COLUMNS];
    char c;
    int pos = 0;

    c = lexer_peek_char(vm);
    if (isalpha(c))
    {
        do
        {
            buffer[pos] = toupper(c);
            if (pos > MAX_IDENTIFIER)
            {
                vm->current_token.type = TOKEN_NONE;
                vm->error = LEXER_ERROR_IDENTIFIER_TOO_LONG;
                return false;
            }
            c = buffer_read_next_char(vm);
        } while (isalnum(vm));
        vm->current_token.type = TOKEN_IDENTIFIER;
        strcpy(vm->current_token.value.identifier, buffer);
        // TODO check for keyword
        return true;
    }
    vm->current_token.type = TOKEN_NONE;
    return false;
}

bool lexer_read_number(vm_t *vm)
{
    char buffer[BUFFER_MAX_COLUMNS];
    char c;
    int pos = 0;

    c = lexer_peek_char(vm);
    if (isdigit(c))
    {
        do
        {
            buffer[pos] = toupper(c);
            if (pos >= 9)
            {
                vm->current_token.type = TOKEN_NONE;
                vm->error = LEXER_ERROR_OVERFLOW;
                return false;
            }
            c = buffer_read_next_char(vm);
        } while (isdigit(vm));
        vm->current_token.type = TOKEN_INTEGER_VALUE;
        // TODO use better conversion from string to integer
        vm->current_token.value.i = atoi(buffer);
        vm->error = LEXER_ERROR_NONE;
        return true;
    }
    vm->current_token.type = TOKEN_NONE;
    return LEXER_ERROR_UNEXPECTED_CHARACTER;
}

bool parser_expect_token_type(vm_t *vm, token_type_t token_type)
{
    if (vm->current_token.type != token_type)
    {
        vm->error = LEXER_ERROR_UNEXPECTED_TOKEN;
        return false;
    }
    return true;
}

// error_t lexer_expect_token_types(vm_t *vm, size_t token_type_count, token_type_t token_type[])

/* EOF */
