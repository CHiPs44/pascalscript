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
#include "ps_memory.h"
#include "ps_token.h"

bool ps_lexer_skip_whitespace_and_comments(ps_lexer *lexer);
bool ps_lexer_read_identifier_or_keyword(ps_lexer *lexer);
bool ps_lexer_read_number(ps_lexer *lexer);
bool ps_lexer_read_char_or_string_value(ps_lexer *lexer);

#define ADVANCE                                                                                                        \
    if (!ps_lexer_read_next_char(lexer))                                                                               \
        return false;

#define GET_NEXT_CHAR(c)                                                                                               \
    {                                                                                                                  \
        ADVANCE                                                                                                        \
        c = ps_buffer_peek_char(lexer->buffer);                                                                        \
    }

ps_lexer *ps_lexer_alloc(void)
{
    ps_lexer *lexer = ps_memory_malloc(sizeof(ps_lexer));
    if (lexer == NULL)
        return NULL;
    lexer->buffer = ps_buffer_alloc();
    if (lexer->buffer == NULL)
    {
        ps_lexer_free(lexer);
        return NULL;
    }
    ps_lexer_reset(lexer);
    return lexer;
}

ps_lexer *ps_lexer_free(ps_lexer *lexer)
{
    if (lexer->buffer != NULL)
        lexer->buffer = ps_buffer_free(lexer->buffer);
    ps_memory_free(lexer);
    return NULL;
}

void ps_lexer_reset(ps_lexer *lexer)
{
    ps_buffer_reset(lexer->buffer);
    lexer->error = PS_ERROR_NONE;
    lexer->current_token.type = PS_TOKEN_NONE;
    memset(&lexer->current_token.value, 0, sizeof(lexer->current_token.value));
}

char *ps_lexer_get_error_message(const ps_lexer *lexer)
{
    static char message[128];
    snprintf(message, sizeof(message) - 1, "LEXER: Error %d %s, Line %d, Column %d", lexer->error,
             ps_error_get_message(lexer->error), lexer->buffer->current_line + 1, lexer->buffer->current_column + 1);
    return message;
}

bool ps_lexer_return_error(ps_lexer *lexer, ps_error error, char *message)
{
    lexer->error = error;
    if (message != NULL)
        fprintf(stderr, "%s: Char '%c', %s\n", ps_lexer_get_error_message(lexer),
                lexer->buffer->current_char >= ' ' && lexer->buffer->current_char < 0x7f ? lexer->buffer->current_char
                                                                                         : '_',
                message);
    lexer->current_token.type = PS_TOKEN_NONE;
    return false;
}

bool ps_lexer_read_next_char(ps_lexer *lexer)
{
    if (ps_buffer_read_next_char(lexer->buffer))
        return true;
    return ps_lexer_return_error(lexer, lexer->buffer->error, "Cannot read next character");
}

bool ps_lexer_skip_whitespace(ps_lexer *lexer, bool *changed)
{
    char c = ps_buffer_peek_char(lexer->buffer);
    *changed = false;
    while (isspace(c) || c == '\n')
    {
        GET_NEXT_CHAR(c);
        *changed = true;
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
            GET_NEXT_CHAR(c);
            if (c == '\0')
                return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_EOF, "Unexpected end of file in comment");
        }
        ADVANCE
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
            ADVANCE
            c1 = ps_buffer_peek_char(lexer->buffer);
            c2 = ps_buffer_peek_next_char(lexer->buffer);
            if (c1 == '\0' || c2 == '\0')
                return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_EOF, "Unexpected end of file in comment");
            if (c1 == '*' && c2 == ')')
            {
                break;
            }
        } while (true);
        // Skip )
        ADVANCE
        // Advance after )
        ADVANCE
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
            ADVANCE
            c1 = ps_buffer_peek_char(lexer->buffer);
            if (c1 == '\0')
                return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_EOF, "Unexpected end of file in comment");
        }
        ADVANCE
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
    char buffer[PS_IDENTIFIER_LEN + 1];
    char c = ps_buffer_peek_char(lexer->buffer);
    int pos = 0;
    if (isalpha(c))
    {
        do
        {
            if (pos > (int)PS_IDENTIFIER_LEN)
            {
                return ps_lexer_return_error(lexer, PS_ERROR_IDENTIFIER_TOO_LONG, "Identifier or keyword too long");
            }
            buffer[pos] = (ps_char)toupper(c);
            GET_NEXT_CHAR(c);
            pos += 1;
            buffer[pos] = '\0';
        } while (isalnum(c) || c == '_');
        lexer->current_token.type = ps_token_is_keyword(buffer);
        memcpy(lexer->current_token.value.identifier, buffer, PS_IDENTIFIER_SIZE);
        return true;
    }
    lexer->current_token.type = PS_TOKEN_NONE;
    return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "Invalid start of identifier or keyword");
}

/**
 * Read a number, either integer or real.
 * The number can be in decimal, binary, octal or hexadecimal format.
 */
bool ps_lexer_read_number(ps_lexer *lexer)
{
    char buffer[33];
    char c = ps_buffer_peek_char(lexer->buffer);
    int pos = 0;
    int base = 10;
    bool is_real = false;
    bool has_exp = false;
    char *end;
    // Decimal (default)
    const char *digits = "0123456789";
    if (c == '%') // Binary?
    {
        base = 2;
        digits = "01";
        GET_NEXT_CHAR(c);
    }
    else if (c == '&') // Octal?
    {
        base = 8;
        digits = "01234567";
        GET_NEXT_CHAR(c);
    }
    else if (c == '$') // Hexadecimal?
    {
        base = 16;
        digits = "01234567abcdefABCDEF";
        GET_NEXT_CHAR(c);
    }
    if (strchr(digits, c) != NULL)
    {
        do
        {
            if (pos > 32)
                return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Too many digits in number");
            buffer[pos++] = c;
            GET_NEXT_CHAR(c);
            if (base == 10)
            {
                // floating point or exponent part?
                if (c == '.')
                {
                    if (is_real)
                        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,
                                                     "Only one dot allowed in real constants");
                    if (has_exp)
                        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,
                                                     "no dot in exponent allowed");
                    is_real = true;
                    if (pos > 32)
                        return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Too many digits in real value");
                    buffer[pos++] = c;
                    GET_NEXT_CHAR(c);
                }
                // exponent part?
                else if (c == 'e' || c == 'E')
                {
                    if (has_exp)
                        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,
                                                     "Only one exponent allowed in real constants");
                    // 1E12 is valid
                    is_real = true;
                    has_exp = true;
                    if (pos > 32)
                        return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Too many digits in real value");
                    buffer[pos++] = c;
                    GET_NEXT_CHAR(c);
                    if (c == '+' || c == '-')
                    {
                        if (pos > 32)
                            return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Too many digits in real value");
                        buffer[pos++] = c;
                        GET_NEXT_CHAR(c);
                    }
                }
            }
        } while (strchr(digits, c) != NULL);
        buffer[pos] = '\0';
        // Use even better conversion from string to real or unsigned integer?
        if (is_real)
        {
            double d = strtod(buffer, &end);
            if (errno == ERANGE || end == buffer || d > PS_REAL_MAX)
                return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Invalid real number value");
            lexer->current_token.type = PS_TOKEN_REAL_VALUE;
            lexer->current_token.value.r = (ps_real)d;
        }
        else
        {
            unsigned long u = strtoul(buffer, &end, base);
            if (errno == ERANGE || end == buffer || u > PS_UNSIGNED_MAX)
                return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Invalid unsigned integer value");
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
    return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "Invalid start of number");
}

/**
 * Read a single character or a string value.
 */
bool ps_lexer_read_char_or_string_value(ps_lexer *lexer)
{
    char c;
    char buffer[PS_STRING_MAX_LEN + 1];
    unsigned int pos = 0;

    c = ps_buffer_peek_char(lexer->buffer);
    if (c != '\'')
        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,
                                     "Char or string value must start with a single quote");
    // Consume the opening quote
    ADVANCE
    while (true)
    {
        c = ps_buffer_peek_char(lexer->buffer);
        if (c == '\0')
            return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_EOF,
                                         "Unexpected end of file in char or string value");
        if (c == '\'')
        {
            // Check for doubled single quotes (escaped single quote)
            char next_char = ps_buffer_peek_next_char(lexer->buffer);
            if (next_char == '\'')
            {
                if (pos >= PS_STRING_MAX_LEN)
                    return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Too many characters in string value");
                buffer[pos++] = '\'';
                ADVANCE     // Consume the first quote
                    ADVANCE // Consume the second quote
            }
            else
            {
                // End of the string
                ADVANCE // Consume the closing quote
                    break;
            }
        }
        else
        {
            if (pos > PS_STRING_MAX_LEN)
                return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Too many characters in string value");
            buffer[pos++] = c;
            ADVANCE
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
                ADVANCE
            }
            else
                lexer->current_token.type = PS_TOKEN_COLON;
            ADVANCE
            break;
        case '@':
            lexer->current_token.type = PS_TOKEN_AT_SIGN;
            ADVANCE
            break;
        case '^':
            lexer->current_token.type = PS_TOKEN_CARET;
            ADVANCE
            break;
        case ',':
            lexer->current_token.type = PS_TOKEN_COMMA;
            ADVANCE
            break;
        case '.':
            if (next_char == '.')
            {
                sprintf(lexer->current_token.value.identifier, "..");
                lexer->current_token.type = PS_TOKEN_RANGE;
                ADVANCE
            }
            else
                lexer->current_token.type = PS_TOKEN_DOT;
            ADVANCE
            break;
        case '(':
            lexer->current_token.type = PS_TOKEN_LEFT_PARENTHESIS;
            ADVANCE
            break;
        case ')':
            lexer->current_token.type = PS_TOKEN_RIGHT_PARENTHESIS;
            ADVANCE
            break;
        case '[':
            lexer->current_token.type = PS_TOKEN_LEFT_BRACKET;
            ADVANCE
            break;
        case ']':
            lexer->current_token.type = PS_TOKEN_RIGHT_BRACKET;
            ADVANCE
            break;
        case ';':
            lexer->current_token.type = PS_TOKEN_SEMI_COLON;
            ADVANCE
            break;
        case '+':
            lexer->current_token.type = PS_TOKEN_PLUS;
            ADVANCE
            break;
        case '-':
            lexer->current_token.type = PS_TOKEN_MINUS;
            ADVANCE
            break;
        case '*':
            if (next_char == '*')
            {
                sprintf(lexer->current_token.value.identifier, "**");
                lexer->current_token.type = PS_TOKEN_POWER;
                ADVANCE
            }
            else
                lexer->current_token.type = PS_TOKEN_STAR;
            ADVANCE
            break;
        case '/':
            lexer->current_token.type = PS_TOKEN_SLASH;
            ADVANCE
            break;
        case '=':
            lexer->current_token.type = PS_TOKEN_EQ;
            ADVANCE
            break;
        case '<':
            if (next_char == '>')
            {
                sprintf(lexer->current_token.value.identifier, "<>");
                lexer->current_token.type = PS_TOKEN_NE;
                ADVANCE
            }
            else if (next_char == '=')
            {
                sprintf(lexer->current_token.value.identifier, "<=");
                lexer->current_token.type = PS_TOKEN_LE;
                ADVANCE
            }
            else
                lexer->current_token.type = PS_TOKEN_LT;
            ADVANCE
            break;
        case '>':
            if (next_char == '=')
            {
                sprintf(lexer->current_token.value.identifier, ">=");
                lexer->current_token.type = PS_TOKEN_GE;
                ADVANCE
            }
            else
                lexer->current_token.type = PS_TOKEN_GT;
            ADVANCE
            break;
        default:
            return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "Invalid character");
        }
    }
    return true;
}

/**
 * Get the current cursor position in lexer's buffer.
 */
bool ps_lexer_get_cursor(const ps_lexer *lexer, uint16_t *line, uint16_t *column)
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
bool ps_lexer_set_cursor(ps_lexer *lexer, uint16_t line, uint16_t column)
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

char *ps_lexer_get_debug_value(ps_lexer *lexer)
{
    static char value[128] = {0};
    static char string[96] = {0};

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_NONE:
        snprintf(value, sizeof(value) - 1, "NONE");
        break;
    case PS_TOKEN_INTEGER_VALUE:
        snprintf(value, sizeof(value) - 1, "INTEGER %" PS_INTEGER_FMT_10, lexer->current_token.value.i);
        break;
    case PS_TOKEN_UNSIGNED_VALUE:
        snprintf(value, sizeof(value) - 1, "UNSIGNED %" PS_UNSIGNED_FMT_10, lexer->current_token.value.u);
        break;
    case PS_TOKEN_REAL_VALUE:
        snprintf(value, sizeof(value) - 1, "REAL %" PS_REAL_FMT, lexer->current_token.value.r);
        break;
    case PS_TOKEN_BOOLEAN_VALUE:
        snprintf(value, sizeof(value) - 1, "BOOLEAN %s", lexer->current_token.value.b ? "TRUE" : "FALSE");
        break;
    case PS_TOKEN_CHAR_VALUE:
        snprintf(value, sizeof(value) - 1, "CHAR '%c'", lexer->current_token.value.c);
        break;
    case PS_TOKEN_STRING_VALUE:
        strlcpy(string, lexer->current_token.value.s, sizeof(string));
        snprintf(value, sizeof(value) - 1, "STRING \"%s\"", string);
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

void ps_lexer_dump(ps_lexer *lexer)
{
    fprintf(stderr, "LEXER: line=%04d, column=%04d, char=%03d/%c, token=%s, error=%d %s\n", lexer->buffer->current_line,
            lexer->buffer->current_column, lexer->buffer->current_char, lexer->buffer->current_char,
            ps_lexer_get_debug_value(lexer), lexer->error, ps_error_get_message(lexer->error));
}

/* EOF */
