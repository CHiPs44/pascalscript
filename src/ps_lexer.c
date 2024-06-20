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
#include "ps_buffer.h"
#include "ps_token.h"
#include "ps_lexer.h"

void ps_lexer_init(lexer_t *lexer)
{
    ps_buffer_init(&lexer->buffer);
    ps_lexer_reset_cursor(lexer);
    lexer->error = LEXER_ERROR_NONE;
    lexer->current_token.type = TOKEN_NONE;
}

void ps_token_dump(token_t *token)
{
    char *type;
    char buffer[256];

    switch (token->type)
    {
    case TOKEN_IDENTIFIER:
        type = "IDENTIFIER";
        snprintf(buffer, 256, "%s", token->value.identifier);
        break;
    case TOKEN_INTEGER_VALUE:
        type = "INTEGER";
        snprintf(buffer, 256, "%d", token->value.i);
        break;
    case TOKEN_CARDINAL_VALUE:
        type = "CARDINAL";
        snprintf(buffer, 256, "%u", token->value.u);
        break;
    case TOKEN_REAL_VALUE:
        type = "REAL";
        snprintf(buffer, 256, "%f", token->value.r);
        break;
    case TOKEN_CHAR_VALUE:
        type = "CHAR";
        snprintf(buffer, 256, "%c", token->value.c);
        break;
    case TOKEN_STRING_VALUE:
        type = "STRING";
        snprintf(buffer, 256, "%s", token->value.s);
        break;
    default:
        type = "UNKNOWN";
        snprintf(buffer, 256, "%s", "?");
        break;
    }
    fprintf(stderr, "TOKEN: type=%s, value=%s\n", type, buffer);
}

void ps_lexer_reset_cursor(lexer_t *lexer)
{
    lexer->current_line = 0;
    lexer->current_column = 0;
    lexer->current_char = '\0';
}

char ps_lexer_peek_char(lexer_t *lexer)
{
    // printf("lexer_peek_char: char=%c alpha=%d, alnum=%d, digit=%d\n", lexer->current_char, isalpha(lexer->current_char), isalnum(lexer->current_char), isdigit(lexer->current_char));
    return lexer->current_char;
}

char ps_lexer_peek_next_char(lexer_t *lexer)
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

char ps_lexer_read_next_char(lexer_t *lexer)
{
    // printf("ps_lexer_read_next_char: line=%d, col=%d\n", lexer->current_line, lexer->current_column);
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
                if (lexer->current_line > lexer->buffer.line_count)
                {
                    lexer->error = LEXER_ERROR_UNEXPECTED_EOF;
                    lexer->current_char = '\0';
                }
            }
        }
    }
    // printf("ps_lexer_read_next_char: char=%c\n", lexer->current_char);
    return lexer->current_char;
}

bool ps_lexer_skip_whitespace(lexer_t *lexer, bool *changed)
{
    char c;
    c = ps_lexer_peek_char(lexer);
    printf("ps_lexer_skip_whitespace: c='%c'\n", c);
    *changed = false;
    while (isspace(c))
    {
        c = ps_lexer_read_next_char(lexer);
        printf("ps_lexer_skip_whitespace: c='%c'\n", c);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
        *changed = true;
    }
    return true;
}

bool ps_lexer_skip_comment1(lexer_t *lexer, bool *changed)
{
    char c;
    c = ps_lexer_peek_char(lexer);
    *changed = false;
    if (c == '{')
    {
        *changed = true;
        while (c != '}')
        {
            c = ps_lexer_read_next_char(lexer);
            if (lexer->error != LEXER_ERROR_NONE)
                return false;
            if (c == '\0')
            {
                lexer->error = LEXER_ERROR_UNEXPECTED_EOF;
                return false;
            }
            printf("ps_lexer_skip_comment1: c='%c'\n", c);
        }
        ps_lexer_read_next_char(lexer);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
    }
    return true;
}

bool ps_lexer_skip_comment2(lexer_t *lexer, bool *changed)
{
    char c1, c2;
    c1 = ps_lexer_peek_char(lexer);
    c2 = ps_lexer_peek_next_char(lexer);
    if (lexer->error != LEXER_ERROR_NONE)
        return false;
    *changed = false;
    if (c1 == '(' && c2 == '*')
    {
        // printf("ps_lexer_skip_comment2: BEGIN c1=%c, c2=%c\n", c1, c2);
        *changed = true;
        do
        {
            c1 = ps_lexer_read_next_char(lexer);
            c2 = ps_lexer_peek_next_char(lexer);
            if (c1 == '\0' || c2 == '\0')
            {
                lexer->error = LEXER_ERROR_UNEXPECTED_EOF;
                return false;
            }
            printf("ps_lexer_skip_comment2: c1='%c', c2='%c'\n", c1, c2);
            if (c1 == '*' && c2 == ')')
            {
                // printf("ps_lexer_skip_comment2: END c1='%c', c2='%c' %d %d\n", c1, c2, c1 == '*', c2 == ')');
                break;
            }
        } while (true);
        // Skip )
        ps_lexer_read_next_char(lexer);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
        // Advance after )
        ps_lexer_read_next_char(lexer);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
    }
    return true;
}

bool ps_lexer_skip_whitespace_and_comments(lexer_t *lexer)
{
    bool changed1 = true;
    bool changed2 = true;
    bool changed3 = true;
    printf("ps_lexer_skip_whitespace_and_comments: BEGIN\n");
    while (changed1 || changed2 || changed3)
    {
        printf("ps_lexer_skip_whitespace_and_comments: 1\n");
        ps_lexer_skip_whitespace(lexer, &changed1);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
        printf("ps_lexer_skip_whitespace_and_comments: 2\n");
        ps_lexer_skip_comment1(lexer, &changed2);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
        printf("ps_lexer_skip_whitespace_and_comments: 3\n");
        ps_lexer_skip_comment2(lexer, &changed3);
        if (lexer->error != LEXER_ERROR_NONE)
            return false;
    }
    printf("ps_lexer_skip_whitespace_and_comments: END\n");
    return true;
}

bool ps_lexer_read_identifier_or_keyword(lexer_t *lexer)
{
    printf("ps_lexer_read_identifier_or_keyword\n");
    char buffer[BUFFER_MAX_COLUMNS];
    char c;
    int pos = 0;

    c = ps_lexer_peek_char(lexer);
    if (lexer->error != LEXER_ERROR_NONE)
        return false;
    if (isalpha(c))
    {
        do
        {
            printf("%c\n", c);
            buffer[pos] = toupper(c);
            if (pos > MAX_IDENTIFIER)
            {
                // lexer->current_token.type = TOKEN_NONE;
                lexer->error = LEXER_ERROR_IDENTIFIER_TOO_LONG;
                return false;
            }
            c = ps_lexer_read_next_char(lexer);
            if (lexer->error != LEXER_ERROR_NONE)
                return false;
            pos += 1;
        } while (isalnum(c));
        buffer[pos] = '\0';
        printf("ps_lexer_read_identifier_or_keyword: \"%s\" ('%c')\n", buffer, c);
        lexer->current_token.type = ps_token_is_keyword(buffer);
        strcpy(lexer->current_token.value.identifier, buffer);
        return true;
    }
    lexer->current_token.type = TOKEN_NONE;
    return false;
}

bool ps_lexer_read_number(lexer_t *lexer)
{
    char buffer[BUFFER_MAX_COLUMNS];
    char c;
    int pos = 0;

    c = ps_lexer_peek_char(lexer);
    if (isdigit(c))
    {
        do
        {
            buffer[pos] = c;
            if (pos > 9)
            {
                lexer->current_token.type = TOKEN_NONE;
                lexer->error = LEXER_ERROR_OVERFLOW;
                return false;
            }
            c = ps_lexer_read_next_char(lexer);
            pos += 1;
        } while (isdigit(c));
        buffer[pos] = '\0';
        lexer->current_token.type = TOKEN_INTEGER_VALUE;
        // TODO use better conversion from string to integer
        lexer->current_token.value.i = atoi(buffer);
        lexer->error = LEXER_ERROR_NONE;
        return true;
    }
    lexer->current_token.type = TOKEN_NONE;
    lexer->error = LEXER_ERROR_UNEXPECTED_CHARACTER;
    return false;
}

bool ps_lexer_read_next_token(lexer_t *lexer)
{
    char c;

    if (!ps_lexer_skip_whitespace_and_comments(lexer))
    {
        printf("ps_lexer_skip_whitespace_and_comments: error=%d\n", lexer->error);
        return false;
    }
    else
    {
        c = ps_lexer_peek_char(lexer);
        if (isdigit(c))
        {
            printf("digit!\n");
            if (!ps_lexer_read_number(lexer))
                return false;
        }
        else if (isalnum(c))
        {
            printf("alnum!\n");
            if (!ps_lexer_read_identifier_or_keyword(lexer))
                return false;
            lexer->current_token.type = ps_token_is_keyword(lexer->current_token.value.identifier);
            printf("identifier: \"%s\" %d\n", lexer->current_token.value.identifier, lexer->current_token.type);
        }
        else
        {
            printf("%c\n", c);
            sprintf(lexer->current_token.value.identifier, "%c", c);
            switch (c)
            {
            case ':':
                lexer->current_token.type = TOKEN_COLON;
                ps_lexer_read_next_char(lexer);
                break;
            case ',':
                lexer->current_token.type = TOKEN_COMMA;
                ps_lexer_read_next_char(lexer);
                break;
            case '.':
                lexer->current_token.type = TOKEN_DOT;
                ps_lexer_read_next_char(lexer);
                break;
            case '(':
                lexer->current_token.type = TOKEN_LEFT_PARENTHESIS;
                ps_lexer_read_next_char(lexer);
                break;
            case ')':
                lexer->current_token.type = TOKEN_RIGHT_PARENTHESIS;
                ps_lexer_read_next_char(lexer);
                break;
            case ';':
                lexer->current_token.type = TOKEN_SEMI_COLON;
                ps_lexer_read_next_char(lexer);
                break;
            case '=':
                lexer->current_token.type = TOKEN_OP_EQ;
                ps_lexer_read_next_char(lexer);
                break;
            default:
                lexer->error = LEXER_ERROR_UNEXPECTED_CHARACTER;
            }
        }
    }
    printf("ps_lexer_read_next_token: END\n");
    return true;
}

// error_t ps_lexer_expect_token_types(lexer_t *lexer, size_t token_type_count, token_type_t token_type[])

/* EOF */
