/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_buffer.h"
#include "ps_error.h"
#include "ps_lexer.h"
#include "ps_token.h"

bool ps_lexer_skip_whitespace_and_comments(ps_lexer *lexer);
bool ps_lexer_read_identifier_or_keyword(ps_lexer *lexer);
bool ps_lexer_read_number(ps_lexer *lexer);
bool ps_lexer_read_char_or_string_value(ps_lexer *lexer);

#define READ_NEXT_CHAR                                                                                                 \
    if (!ps_lexer_read_next_char(lexer))                                                                               \
        return false;

ps_lexer *ps_lexer_init()
{
    ps_lexer *lexer = calloc(1, sizeof(ps_lexer));
    if (lexer == NULL)
        return NULL;
    lexer->buffer = ps_buffer_init();
    if (lexer->buffer == NULL)
    {
        ps_lexer_done(lexer);
        return NULL;
    }
    ps_lexer_reset(lexer);
    return lexer;
}

ps_lexer *ps_lexer_done(ps_lexer *lexer)
{
    if (lexer->buffer != NULL)
        ps_buffer_done(lexer->buffer);
    free(lexer);
    return NULL;
}

void ps_lexer_reset(ps_lexer *lexer)
{
    ps_buffer_reset(lexer->buffer);
    lexer->error = PS_ERROR_NONE;
    lexer->current_token.type = PS_TOKEN_NONE;
    memset(&lexer->current_token.value, 0, sizeof(lexer->current_token.value));
}

char *ps_lexer_show_error(ps_lexer *lexer)
{
    static char message[128];
    snprintf(message, sizeof(message) - 1, "LEXER: Error %d %s, Line %d, Column %d", lexer->error,
             ps_error_get_message(lexer->error), lexer->buffer->current_line, lexer->buffer->current_column);
    return message;
}

bool ps_lexer_return_error(ps_lexer *lexer, ps_error error, char *message)
{
    if (message != NULL)
        fprintf(stderr, "%s: %d %s, Line %d, column %d %c\n", message, error, ps_error_get_message(error),
                lexer->buffer->current_line, lexer->buffer->current_column,
                lexer->buffer->current_char >= ' ' ? lexer->buffer->current_char : '_');
    lexer->current_token.type = PS_TOKEN_NONE;
    lexer->error = error;
    return false;
}

bool ps_lexer_read_next_char(ps_lexer *lexer)
{
    if (ps_buffer_read_next_char(lexer->buffer))
        return true;
    return ps_lexer_return_error(lexer, lexer->buffer->error, "ps_lexer_read_next_char");
}

bool ps_lexer_skip_whitespace(ps_lexer *lexer, bool *changed)
{
    char c = ps_buffer_peek_char(lexer->buffer);
    *changed = false;
    while (isspace(c) || c == '\n')
    {
        READ_NEXT_CHAR;
        *changed = true;
        c = ps_buffer_peek_char(lexer->buffer);
    }
    return true;
}

/**
 * Skip comment #1: '{' ... comment ... '}'
 */
bool ps_lexer_skip_comment1(ps_lexer *lexer, bool *changed)
{
    char c = ps_buffer_peek_char(lexer->buffer);
    *changed = false;
    if (c == '{')
    {
        *changed = true;
        while (c != '}')
        {
            READ_NEXT_CHAR;
            c = lexer->buffer->current_char;
            if (c == '\0')
                return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_EOF, "ps_lexer_skip_comment1");
        }
        READ_NEXT_CHAR;
    }
    return true;
}

/**
 * Skip comment #2: '(*' ... comment ... '*)'
 */
bool ps_lexer_skip_comment2(ps_lexer *lexer, bool *changed)
{
    char c1 = ps_buffer_peek_char(lexer->buffer);
    char c2 = ps_buffer_peek_next_char(lexer->buffer);
    *changed = false;
    if (c1 == '(' && c2 == '*')
    {
        *changed = true;
        do
        {
            READ_NEXT_CHAR;
            c1 = ps_buffer_peek_char(lexer->buffer);
            c2 = ps_buffer_peek_next_char(lexer->buffer);
            if (c1 == '\0' || c2 == '\0')
                return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_EOF, "ps_lexer_skip_comment2");
            if (c1 == '*' && c2 == ')')
            {
                break;
            }
        } while (true);
        // Skip )
        READ_NEXT_CHAR;
        // Advance after )
        READ_NEXT_CHAR;
    }
    return true;
}

/**
 * Skip comment #1: '//' ... comment ... '\n' or end of file
 */
bool ps_lexer_skip_comment3(ps_lexer *lexer, bool *changed)
{
    char c1 = ps_buffer_peek_char(lexer->buffer);
    char c2 = ps_buffer_peek_next_char(lexer->buffer);
    *changed = false;
    if (c1 == '/' && c2 == '/')
    {
        *changed = true;
        while (c1 != '\n')
        {
            READ_NEXT_CHAR;
            c1 = ps_buffer_peek_char(lexer->buffer);
            if (c1 == '\0')
                return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_EOF, "ps_lexer_skip_comment3");
        }
        READ_NEXT_CHAR;
    }
    return true;
}

/**
 * Skip whitespace, comments and read next token.
 */
bool ps_lexer_skip_whitespace_and_comments(ps_lexer *lexer)
{
    bool changed1 = true;
    bool changed2 = true;
    bool changed3 = true;
    bool changed4 = true;
    while (changed1 || changed2 || changed3 || changed4)
    {
        if (!ps_lexer_skip_whitespace(lexer, &changed1))
            return false;
        if (!ps_lexer_skip_comment1(lexer, &changed2))
            return false;
        if (!ps_lexer_skip_comment2(lexer, &changed3))
            return false;
        if (!ps_lexer_skip_comment3(lexer, &changed4))
            return false;
    }
    return true;
}

/**
 * Read an identifier or a keyword.
 */
bool ps_lexer_read_identifier_or_keyword(ps_lexer *lexer)
{
    char buffer[PS_IDENTIFIER_MAX + 1];
    char c = ps_buffer_peek_char(lexer->buffer);
    int pos = 0;
    if (isalpha(c))
    {
        do
        {
            if (pos > PS_IDENTIFIER_MAX)
            {
                return ps_lexer_return_error(lexer, PS_ERROR_IDENTIFIER_TOO_LONG,
                                             "ps_lexer_read_identifier_or_keyword");
            }
            buffer[pos] = toupper(c);
            READ_NEXT_CHAR;
            c = ps_buffer_peek_char(lexer->buffer);
            pos += 1;
            buffer[pos] = '\0';
        } while (isalnum(c) || c == '_');
        lexer->current_token.type = ps_token_is_keyword(buffer);
        strncpy(lexer->current_token.value.identifier, buffer, PS_IDENTIFIER_SIZE);
        return true;
    }
    lexer->current_token.type = PS_TOKEN_NONE;
    return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "ps_lexer_read_identifier_or_keyword");
}

/**
 * Read a number, either integer or real.
 * The number can be in decimal, binary, octal or hexadecimal format.
 */
bool ps_lexer_read_number(ps_lexer *lexer)
{
    char buffer[PS_BUFFER_MAX_COLUMNS + 1];
    char c = ps_buffer_peek_char(lexer->buffer);
    int pos = 0;
    int base = 10;
    bool is_real = false;
    bool has_exp = false;
    // Decimal
    char *digits = "0123456789";
    // Binary?
    if (c == '%')
    {
        base = 2;
        digits = "01";
        READ_NEXT_CHAR;
        c = ps_buffer_peek_char(lexer->buffer);
    }
    // Octal?
    else if (c == '&')
    {
        base = 8;
        digits = "01234567";
        READ_NEXT_CHAR;
        c = ps_buffer_peek_char(lexer->buffer);
    }
    // Hexadecimal?
    else if (c == '$')
    {
        base = 16;
        digits = "01234567abcdefABCDEF";
        READ_NEXT_CHAR;
        c = ps_buffer_peek_char(lexer->buffer);
    }
    if (strchr(digits, c) != NULL)
    {
        do
        {
            if (pos > PS_BUFFER_MAX_COLUMNS)
                return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "ps_lexer_read_number");
            buffer[pos++] = c;
            READ_NEXT_CHAR;
            c = ps_buffer_peek_char(lexer->buffer);
            if (base == 10)
            {
                if (c == '.' && !is_real)
                {
                    is_real = true;
                    buffer[pos++] = c;
                    READ_NEXT_CHAR;
                    c = ps_buffer_peek_char(lexer->buffer);
                }
                else if ((c == 'e' || c == 'E'))
                {
                    if (has_exp)
                        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "ps_lexer_read_number");
                    has_exp = true;
                    is_real = true;
                    buffer[pos++] = c;
                    READ_NEXT_CHAR;
                    c = ps_buffer_peek_char(lexer->buffer);
                    if (c == '+' || c == '-')
                    {
                        buffer[pos++] = c;
                        READ_NEXT_CHAR;
                        c = ps_buffer_peek_char(lexer->buffer);
                    }
                }
            }
        } while (strchr(digits, c) != NULL || strchr(".eE+-", c) != NULL);
        buffer[pos] = '\0';
        // TODO? use even better conversion from string to real or unsigned integer
        char *end;
        if (is_real)
        {
            double d = strtod(buffer, &end);
            if (errno == ERANGE || end == buffer || d > PS_REAL_MAX)
                return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "ps_lexer_read_number");
            lexer->current_token.type = PS_TOKEN_REAL_VALUE;
            lexer->current_token.value.r = (ps_real)d;
        }
        else
        {
            unsigned long u = strtoul(buffer, &end, base);
            if (errno == ERANGE || end == buffer || u > PS_UNSIGNED_MAX)
                return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "ps_lexer_read_number");
            if (u > PS_INTEGER_MAX)
            {
                lexer->current_token.type = PS_TOKEN_UNSIGNED_VALUE;
                lexer->current_token.value.u = (ps_unsigned)u;
            }
            else
            {
                lexer->current_token.type = PS_TOKEN_INTEGER_VALUE;
                lexer->current_token.value.i = (ps_integer)u;
            }
        }
        lexer->error = PS_ERROR_NONE;
        return true;
    }
    return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "ps_lexer_read_number");
}

/**
 * Read a single character or a string value.
 */
bool ps_lexer_read_char_or_string_value(ps_lexer *lexer)
{
    char c;
    char buffer[PS_STRING_MAX_LEN + 1];
    int pos = 0;

    c = ps_buffer_peek_char(lexer->buffer);
    if (c != '\'')
        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "ps_lexer_read_char_or_string_value");
    // Consume the opening quote
    READ_NEXT_CHAR;
    while (true)
    {
        c = ps_buffer_peek_char(lexer->buffer);
        if (c == '\0')
            return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_EOF, "ps_lexer_read_char_or_string_value");
        if (c == '\'')
        {
            // Check for doubled single quotes (escaped single quote)
            char next_char = ps_buffer_peek_next_char(lexer->buffer);
            if (next_char == '\'')
            {
                if (pos >= PS_STRING_MAX_LEN)
                    return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "ps_lexer_read_char_or_string_value");

                buffer[pos++] = '\'';
                READ_NEXT_CHAR; // Consume the first quote
                READ_NEXT_CHAR; // Consume the second quote
            }
            else
            {
                // End of the string
                READ_NEXT_CHAR; // Consume the closing quote
                break;
            }
        }
        else
        {
            if (pos >= PS_STRING_MAX_LEN)
                return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "ps_lexer_read_char_or_string_value");

            buffer[pos++] = c;
            READ_NEXT_CHAR;
        }
    }
    buffer[pos] = '\0';
    if (pos == 1)
    {
        lexer->current_token.type = PS_TOKEN_CHAR_VALUE;
        lexer->current_token.value.c = buffer[0];
    }
    else
    {
        lexer->current_token.type = PS_TOKEN_STRING_VALUE;
        strncpy((char *)lexer->current_token.value.s, buffer, PS_STRING_MAX_LEN);
        lexer->current_token.value.s[PS_STRING_MAX_LEN] = '\0';
    }
    lexer->error = PS_ERROR_NONE;
    return true;
}

/**
 * Read the next token from the buffer.
 */
bool ps_lexer_read_token(ps_lexer *lexer)
{
    if (!ps_lexer_skip_whitespace_and_comments(lexer))
        return false;
    char current_char = ps_buffer_peek_char(lexer->buffer);
    char next_char = ps_buffer_peek_next_char(lexer->buffer);
    // fprintf(stderr, "char=%d next=%d length=%d error=%d\n", current_char, next_char, lexer->buffer->length,
    // lexer->buffer->error);
    if (isdigit(current_char) || current_char == '%' || current_char == '&' || current_char == '$')
    {
        if (!ps_lexer_read_number(lexer))
            return false;
    }
    else if (isalnum(current_char))
    {
        if (!ps_lexer_read_identifier_or_keyword(lexer))
            return false;
        lexer->current_token.type = ps_token_is_keyword(lexer->current_token.value.identifier);
    }
    else if (current_char == '\'')
    {
        if (!ps_lexer_read_char_or_string_value(lexer))
            return false;
    }
    else
    {
        sprintf(lexer->current_token.value.identifier, "%c", current_char);
        switch (current_char)
        {
        case ':':
            if (next_char == '=')
            {
                sprintf(lexer->current_token.value.identifier, ":=");
                lexer->current_token.type = PS_TOKEN_ASSIGN;
                READ_NEXT_CHAR;
            }
            else
                lexer->current_token.type = PS_TOKEN_COLON;
            READ_NEXT_CHAR;
            break;
        case '@':
            lexer->current_token.type = PS_TOKEN_AT_SIGN;
            READ_NEXT_CHAR;
            break;
        case '^':
            lexer->current_token.type = PS_TOKEN_CARET;
            READ_NEXT_CHAR;
            break;
        case ',':
            lexer->current_token.type = PS_TOKEN_COMMA;
            READ_NEXT_CHAR;
            break;
        case '.':
            if (next_char == '.')
            {
                sprintf(lexer->current_token.value.identifier, "..");
                lexer->current_token.type = PS_TOKEN_RANGE;
                READ_NEXT_CHAR;
            }
            else
                lexer->current_token.type = PS_TOKEN_DOT;
            READ_NEXT_CHAR;
            break;
        case '(':
            lexer->current_token.type = PS_TOKEN_LEFT_PARENTHESIS;
            READ_NEXT_CHAR;
            break;
        case ')':
            lexer->current_token.type = PS_TOKEN_RIGHT_PARENTHESIS;
            READ_NEXT_CHAR;
            break;
        case '[':
            lexer->current_token.type = PS_TOKEN_LEFT_BRACKET;
            READ_NEXT_CHAR;
            break;
        case ']':
            lexer->current_token.type = PS_TOKEN_RIGHT_BRACKET;
            READ_NEXT_CHAR;
            break;
        case ';':
            lexer->current_token.type = PS_TOKEN_SEMI_COLON;
            READ_NEXT_CHAR;
            break;
        case '+':
            lexer->current_token.type = PS_TOKEN_PLUS;
            READ_NEXT_CHAR;
            break;
        case '-':
            lexer->current_token.type = PS_TOKEN_MINUS;
            READ_NEXT_CHAR;
            break;
        case '*':
            if (next_char == '*')
            {
                sprintf(lexer->current_token.value.identifier, "**");
                lexer->current_token.type = PS_TOKEN_POWER;
                READ_NEXT_CHAR;
            }
            else
                lexer->current_token.type = PS_TOKEN_STAR;
            READ_NEXT_CHAR;
            break;
        case '/':
            lexer->current_token.type = PS_TOKEN_SLASH;
            READ_NEXT_CHAR;
            break;
        case '=':
            lexer->current_token.type = PS_TOKEN_EQUAL;
            READ_NEXT_CHAR;
            break;
        case '<':
            if (next_char == '>')
            {
                sprintf(lexer->current_token.value.identifier, "<>");
                lexer->current_token.type = PS_TOKEN_NOT_EQUAL;
                READ_NEXT_CHAR;
            }
            else if (next_char == '=')
            {
                sprintf(lexer->current_token.value.identifier, "<=");
                lexer->current_token.type = PS_TOKEN_LESS_OR_EQUAL;
                READ_NEXT_CHAR;
            }
            else
                lexer->current_token.type = PS_TOKEN_LESS_THAN;
            READ_NEXT_CHAR;
            break;
        case '>':
            if (next_char == '=')
            {
                sprintf(lexer->current_token.value.identifier, ">=");
                lexer->current_token.type = PS_TOKEN_GREATER_OR_EQUAL;
                READ_NEXT_CHAR;
            }
            else
                lexer->current_token.type = PS_TOKEN_GREATER_THAN;
            READ_NEXT_CHAR;
            break;
        default:
            printf("DEFAULT! %c %d at line %d column %d\n", current_char, current_char, lexer->buffer->current_line,
                   lexer->buffer->current_column);
            lexer->error = PS_ERROR_UNEXPECTED_CHARACTER;
            return false;
        }
    }
    return true;
}

/**
 * Get the current cursor position in lexer's buffer.
 */
bool ps_lexer_get_cursor(ps_lexer *lexer, uint16_t *line, uint8_t *column)
{
    if (lexer == NULL || lexer->buffer == NULL)
        return false;
    *line = lexer->buffer->current_line;
    *column = lexer->buffer->current_column;
    return true;
}

/**
 * Set the cursor position in lexer's buffer.
 */
bool ps_lexer_set_cursor(ps_lexer *lexer, uint16_t line, uint8_t column)
{
    if (lexer == NULL || lexer->buffer == NULL || line >= lexer->buffer->line_count ||
        column > lexer->buffer->line_lengths[line])
        return false;
    lexer->buffer->current_line = line;
    lexer->buffer->current_column = column;
    lexer->buffer->current_char = '\0';
    lexer->buffer->next_char = '\0';
    return ps_buffer_read_next_char(lexer->buffer);
}

/**
 * Get a debug-friendly string representation of the current token's value.
 */
char *ps_lexer_get_debug_value(ps_lexer *lexer)
{
    static char value[128] = {0};
    int len = 0;

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_NONE:
        snprintf(value, sizeof(value) - 1, "NONE");
        break;
    case PS_TOKEN_END_OF_FILE:
        snprintf(value, sizeof(value) - 1, "EOF");
        break;
    case PS_TOKEN_INTEGER_VALUE:
        snprintf(value, sizeof(value) - 1, "INTEGER %ld", lexer->current_token.value.i);
        break;
    case PS_TOKEN_UNSIGNED_VALUE:
        snprintf(value, sizeof(value) - 1, "UNSIGNED %lu", lexer->current_token.value.u);
        break;
    case PS_TOKEN_REAL_VALUE:
        snprintf(value, sizeof(value) - 1, "REAL %f", lexer->current_token.value.r);
        break;
    case PS_TOKEN_BOOLEAN_VALUE:
        snprintf(value, sizeof(value) - 1, "BOOLEAN %s", lexer->current_token.value.b ? "TRUE" : "FALSE");
        break;
    case PS_TOKEN_CHAR_VALUE:
        snprintf(value, sizeof(value) - 1, "CHAR '%c'", lexer->current_token.value.c);
        break;
    case PS_TOKEN_STRING_VALUE:
        snprintf(value, sizeof(value) - 1, "STRING \"%s\"", lexer->current_token.value.s);
        break;
    case PS_TOKEN_IDENTIFIER:
        snprintf(value, sizeof(value) - 1, "IDENTIFIER \"%s\"", lexer->current_token.value.identifier);
        break;
    default:
        snprintf(value, sizeof(value) - 1, "TOKEN %d %s", lexer->current_token.type,
                 ps_token_get_keyword(lexer->current_token.type));
        break;
    }

    return value;
}

/**
 * Dump the current state of the lexer.
 */
void ps_lexer_dump(ps_lexer *lexer)
{
    fprintf(stderr, "LEXER: token=%s, error=%d %s\n", ps_lexer_get_debug_value(lexer), lexer->error,
            ps_error_get_message(lexer->error));
}

/* EOF */
