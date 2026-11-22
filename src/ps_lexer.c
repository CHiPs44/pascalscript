/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "ps_buffer.h"
#include "ps_error.h"
#include "ps_lexer.h"
#include "ps_lexer_internal.h"
#include "ps_memory.h"
#include "ps_string.h"
#include "ps_token.h"
#include "ps_tools.h"

static bool ps_lexer_read_identifier_or_keyword(ps_lexer *lexer);

ps_lexer *ps_lexer_alloc(void)
{
    ps_lexer *lexer = ps_memory_malloc(PS_MEMORY_LEXER, sizeof(ps_lexer));
    if (lexer == NULL)
        return NULL;
    memset(lexer, 0, sizeof(ps_lexer));
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
    if (lexer == NULL)
        return NULL;
    ps_lexer_reset(lexer);
    if (lexer->buffer != NULL)
        lexer->buffer = ps_buffer_free(lexer->buffer);
    ps_memory_free(PS_MEMORY_LEXER, lexer);
    return NULL;
}

void ps_lexer_reset(ps_lexer *lexer)
{
    if (lexer->current_token.type == PS_TOKEN_STRING_VALUE && lexer->current_token.value.s != NULL)
        lexer->current_token.value.s = ps_string_free(lexer->current_token.value.s);
    if (lexer->buffer != NULL)
        ps_buffer_reset(lexer->buffer);
    lexer->error = PS_ERROR_NONE;
    lexer->current_token.type = PS_TOKEN_NONE;
    memset(&lexer->current_token.value, 0, sizeof(lexer->current_token.value));
}

char *ps_lexer_get_error_message(const ps_lexer *lexer)
{
    static char message[128];
    snprintf(message, sizeof(message), "LEXER: Error %d %s, Line %d, Column %d", lexer->error,
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
    if (lexer->buffer->error == PS_ERROR_EOF)
        return true;
    return ps_lexer_return_error(lexer, lexer->buffer->error, "Cannot read next character");
}

static bool ps_lexer_skip_whitespace(ps_lexer *lexer, bool *changed)
{
    int current_char = ps_buffer_peek_char(lexer->buffer);
    *changed = false;
    if (current_char == EOF)
        return true;
    while (isspace(current_char) || current_char == '\n')
    {
        GET_NEXT_CHAR(current_char)
        *changed = true;
    }
    return true;
}

/**
 * Skip comment #1: '{' ... comment ... '}'
 */
static bool ps_lexer_skip_comment1(ps_lexer *lexer, bool *changed)
{
    int current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
    {
        *changed = false;
        return true;
    }
    *changed = false;
    if (current_char == '{')
    {
        *changed = true;
        while (current_char != '}')
        {
            GET_NEXT_CHAR(current_char)
            if (current_char == EOF)
                return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_EOF, "Unexpected end of file in comment");
        }
        ADVANCE()
    }
    return true;
}

/**
 * Skip comment #2: '(*' ... comment ... '*)'
 */
static bool ps_lexer_skip_comment2(ps_lexer *lexer, bool *changed)
{
    int current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
    {
        *changed = false;
        return true;
    }
    int next_char = ps_buffer_peek_next_char(lexer->buffer);
    *changed = false;
    if (current_char == '(' && next_char == '*')
    {
        *changed = true;
        do
        {
            ADVANCE()
            current_char = ps_buffer_peek_char(lexer->buffer);
            next_char = ps_buffer_peek_next_char(lexer->buffer);
            if (current_char == EOF || next_char == EOF)
                return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_EOF, "Unexpected end of file in comment");
            if (current_char == '*' && next_char == ')')
            {
                break;
            }
        } while (true);
        // Skip )
        ADVANCE()
        // Advance after )
        ADVANCE()
    }
    return true;
}

/**
 * Skip comment #1: '//' ... comment ... '\n' or end of file
 */
static bool ps_lexer_skip_comment3(ps_lexer *lexer, bool *changed)
{
    int current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
    {
        *changed = false;
        return true;
    }
    int next_char = ps_buffer_peek_next_char(lexer->buffer);
    *changed = false;
    if (current_char == '/' && next_char == '/')
    {
        *changed = true;
        while (current_char != '\n' && current_char != EOF)
        {
            ADVANCE()
            current_char = ps_buffer_peek_char(lexer->buffer);
        }
        if (current_char == '\n')
            ADVANCE()
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
static bool ps_lexer_read_identifier_or_keyword(ps_lexer *lexer)
{
    ps_char buffer[PS_IDENTIFIER_LEN + 1] = {0};
    int current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
        return ps_lexer_return_error(lexer, PS_ERROR_EOF, "Unexpected end of file");
    int pos = 0;
    if (isalpha(current_char) || current_char == '_')
    {
        do
        {
            if (pos >= (int)PS_IDENTIFIER_LEN)
            {
                return ps_lexer_return_error(lexer, PS_ERROR_IDENTIFIER_TOO_LONG, "Identifier or keyword too long");
            }
            buffer[pos] = (ps_char)toupper(current_char);
            GET_NEXT_CHAR(current_char)
            pos += 1;
        } while (isalnum(current_char) || current_char == '_');
        buffer[pos] = '\0';
        lexer->current_token.type = ps_token_is_keyword(buffer);
        memcpy(lexer->current_token.value.identifier, buffer, PS_IDENTIFIER_SIZE);
        return true;
    }
    lexer->current_token.type = PS_TOKEN_NONE;
    return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "Invalid start of identifier or keyword");
}

static bool ps_lexer_read_other_tokens(ps_lexer *lexer, char current_char, char next_char)
{
    snprintf((char *)lexer->current_token.value.identifier, PS_IDENTIFIER_SIZE, "%c", current_char);
    switch (current_char)
    {
    case ':':
        if (next_char == '=')
        {
            snprintf((char *)lexer->current_token.value.identifier, PS_IDENTIFIER_SIZE, ":=");
            lexer->current_token.type = PS_TOKEN_ASSIGN;
            ADVANCE()
        }
        else
            lexer->current_token.type = PS_TOKEN_COLON;
        ADVANCE()
        break;
    case '@':
        lexer->current_token.type = PS_TOKEN_AT_SIGN;
        ADVANCE()
        break;
    case '^':
        lexer->current_token.type = PS_TOKEN_CARET;
        ADVANCE()
        break;
    case ',':
        lexer->current_token.type = PS_TOKEN_COMMA;
        ADVANCE()
        break;
    case '.':
        if (next_char == '.')
        {
            snprintf((char *)lexer->current_token.value.identifier, PS_IDENTIFIER_SIZE, "..");
            lexer->current_token.type = PS_TOKEN_RANGE;
            ADVANCE()
        }
        else if (isdigit(next_char))
        {
            // This is a real number starting with a dot, e.g., .123
            if (!ps_lexer_read_number(lexer))
                return false;
            return true;
        }
        else
            lexer->current_token.type = PS_TOKEN_DOT;
        ADVANCE()
        break;
    case '(':
        lexer->current_token.type = PS_TOKEN_LEFT_PARENTHESIS;
        ADVANCE()
        break;
    case ')':
        lexer->current_token.type = PS_TOKEN_RIGHT_PARENTHESIS;
        ADVANCE()
        break;
    case '[':
        lexer->current_token.type = PS_TOKEN_LEFT_BRACKET;
        ADVANCE()
        break;
    case ']':
        lexer->current_token.type = PS_TOKEN_RIGHT_BRACKET;
        ADVANCE()
        break;
    case ';':
        lexer->current_token.type = PS_TOKEN_SEMI_COLON;
        ADVANCE()
        break;
    case '+':
        lexer->current_token.type = PS_TOKEN_PLUS;
        ADVANCE()
        break;
    case '-':
        lexer->current_token.type = PS_TOKEN_MINUS;
        ADVANCE()
        break;
    case '*':
        if (next_char == '*')
        {
            snprintf((char *)lexer->current_token.value.identifier, PS_IDENTIFIER_SIZE, "**");
            lexer->current_token.type = PS_TOKEN_POWER;
            ADVANCE()
        }
        else
            lexer->current_token.type = PS_TOKEN_STAR;
        ADVANCE()
        break;
    case '/':
        lexer->current_token.type = PS_TOKEN_SLASH;
        ADVANCE()
        break;
    case '=':
        lexer->current_token.type = PS_TOKEN_EQ;
        ADVANCE()
        break;
    case '<':
        if (next_char == '>')
        {
            snprintf((char *)lexer->current_token.value.identifier, PS_IDENTIFIER_SIZE, "<>");
            lexer->current_token.type = PS_TOKEN_NE;
            ADVANCE()
        }
        else if (next_char == '=')
        {
            snprintf((char *)lexer->current_token.value.identifier, PS_IDENTIFIER_SIZE, "<=");
            lexer->current_token.type = PS_TOKEN_LE;
            ADVANCE()
        }
        else if (next_char == '<')
        {
            snprintf((char *)lexer->current_token.value.identifier, PS_IDENTIFIER_SIZE, "<<");
            lexer->current_token.type = PS_TOKEN_SHL;
            ADVANCE()
        }
        else
            lexer->current_token.type = PS_TOKEN_LT;
        ADVANCE()
        break;
    case '>':
        if (next_char == '=')
        {
            snprintf((char *)lexer->current_token.value.identifier, PS_IDENTIFIER_SIZE, ">=");
            lexer->current_token.type = PS_TOKEN_GE;
            ADVANCE()
        }
        else if (next_char == '>')
        {
            snprintf((char *)lexer->current_token.value.identifier, PS_IDENTIFIER_SIZE, ">>");
            lexer->current_token.type = PS_TOKEN_SHR;
            ADVANCE()
        }
        else
            lexer->current_token.type = PS_TOKEN_GT;
        ADVANCE()
        break;
    default:
        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "Invalid character");
    }
    return true;
}

/**
 * Read next token from buffer.
 */
bool ps_lexer_read_token(ps_lexer *lexer)
{
    // Free last token string value id needed?
    if (lexer->current_token.type == PS_TOKEN_STRING_VALUE && lexer->current_token.value.s != NULL)
    {
        lexer->current_token.value.s = ps_string_free(lexer->current_token.value.s);
    }
    lexer->current_token.type = PS_TOKEN_NONE;
    if (!ps_lexer_skip_whitespace_and_comments(lexer))
        return false;
    int current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
    {
        lexer->current_token.type = PS_TOKEN_EOF;
        return true;
    }
    int next_char = ps_buffer_peek_next_char(lexer->buffer);
    lexer->start_line = lexer->buffer->current_line;
    lexer->start_column = lexer->buffer->current_column;
    // Number: integer or real value?
    if (isdigit(current_char) || current_char == '%' || current_char == '&' || current_char == '$' ||
        (current_char == '.' && isdigit(next_char)))
    {
        if (!ps_lexer_read_number(lexer))
            return false;
    }
    // Identifier or keyword?
    else if (isalnum(current_char) || current_char == '_')
    {
        if (!ps_lexer_read_identifier_or_keyword(lexer))
            return false;
        lexer->current_token.type = ps_token_is_keyword(lexer->current_token.value.identifier);
    }
    // String or char value?
    //  '...' char or string value?
    //  ^A..^Z char value?
    //  #nnn char value?
    else if (current_char == '\'' || current_char == '^' || current_char == '#')
    {
        if (!ps_lexer_read_char_or_string_value(lexer))
            return false;
    }
    else
    {
        return ps_lexer_read_other_tokens(lexer, (char)current_char, (char)next_char);
    }
    return true;
}

char *ps_lexer_get_debug_value(ps_lexer *lexer)
{
    static char value[128] = {0};
    static char string[96] = {0};

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_NONE:
        snprintf(value, sizeof(value), "NONE");
        break;
    case PS_TOKEN_EOF:
        snprintf(value, sizeof(value), "EOF");
        break;
    case PS_TOKEN_INTEGER_VALUE:
        snprintf(value, sizeof(value), "INTEGER %" PS_INTEGER_FMT_10, lexer->current_token.value.i);
        break;
    case PS_TOKEN_UNSIGNED_VALUE:
        snprintf(value, sizeof(value), "UNSIGNED %" PS_UNSIGNED_FMT_10, lexer->current_token.value.u);
        break;
    case PS_TOKEN_REAL_VALUE:
        snprintf(value, sizeof(value), "REAL %" PS_REAL_FMT, lexer->current_token.value.r);
        break;
    case PS_TOKEN_BOOLEAN_VALUE:
        snprintf(value, sizeof(value), "BOOLEAN %s", lexer->current_token.value.b ? "TRUE" : "FALSE");
        break;
    case PS_TOKEN_CHAR_VALUE:
        snprintf(value, sizeof(value), "CHAR '%c'", lexer->current_token.value.c);
        break;
    case PS_TOKEN_STRING_VALUE:
        (void)ps_strscpy(string, (char *)lexer->current_token.value.s->str, sizeof(string));
        snprintf(value, sizeof(value), "STRING \"%s\"", string);
        break;
    case PS_TOKEN_IDENTIFIER:
        snprintf(value, sizeof(value), "IDENTIFIER \"%s\"", lexer->current_token.value.identifier);
        break;
    default:
        snprintf(value, sizeof(value), "TOKEN %d %s", lexer->current_token.type,
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
