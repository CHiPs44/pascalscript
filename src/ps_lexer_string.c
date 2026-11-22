/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>

#include "ps_config.h"
#include "ps_error.h"
#include "ps_lexer_internal.h"
#include "ps_string.h"

bool ps_lexer_append_char(ps_lexer *lexer, ps_string **string, char c)
{
    ps_string *old = string == NULL ? lexer->current_token.value.s : *string;
    errno = 0;
    ps_string *new = ps_string_append_char(old, c);
    if (new == NULL)
    {
        lexer->error = ps_error_map_errno();
        return false;
    }
    if (string == NULL)
    {
        lexer->current_token.value.s = new;
        return true;
    }
    *string = new;
    return true;
}

static bool ps_lexer_append_quoted_character(ps_lexer *lexer, ps_string **string, unsigned int *pos, int character)
{
    if (*pos >= PS_STRING_MAX_LEN)
        return ps_lexer_return_error(lexer, PS_ERROR_STRING_TOO_LONG, "Too many characters in string value");
    if (!ps_lexer_append_char(lexer, string, (char)character))
        return false;
    *pos += 1;
    return true;
}

static bool ps_lexer_validate_quoted_string_character(ps_lexer *lexer, int character)
{
    if (character == EOF)
        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_EOF, "Unexpected end of file in char or string value");
    if (character == '\n')
        return ps_lexer_return_error(lexer, PS_ERROR_STRING_NOT_MULTI_LINE, "String value cannot be multi-line");
    return true;
}

static bool ps_lexer_read_escaped_quote(ps_lexer *lexer, ps_string **string, unsigned int *pos)
{
    if (!ps_lexer_append_quoted_character(lexer, string, pos, '\''))
        return false;
    if (!ps_lexer_read_next_char(lexer))
        return false;
    if (ps_buffer_peek_char(lexer->buffer) == EOF)
        return ps_lexer_return_error(lexer, PS_ERROR_EOF, "Unexpected end of file");
    return ps_lexer_read_next_char(lexer);
}

static bool ps_lexer_read_quoted_string_character(ps_lexer *lexer, ps_string **string, unsigned int *pos, int character)
{
    if (!ps_lexer_append_quoted_character(lexer, string, pos, character))
        return false;
    if (!ps_lexer_read_next_char(lexer))
        return false;
    if (ps_buffer_peek_char(lexer->buffer) == EOF)
        return ps_lexer_return_error(lexer, PS_ERROR_EOF, "Unexpected end of file");
    return true;
}

static bool ps_lexer_read_quoted_string_content(ps_lexer *lexer, ps_string **string, unsigned int *pos)
{
    int current_char = '\0';

    while (true)
    {
        current_char = ps_buffer_peek_char(lexer->buffer);
        if (!ps_lexer_validate_quoted_string_character(lexer, current_char))
            return false;

        if (current_char == '\'')
        {
            if (ps_buffer_peek_next_char(lexer->buffer) != '\'')
                return ps_lexer_read_next_char(lexer);
            if (!ps_lexer_read_escaped_quote(lexer, string, pos))
                return false;
            continue;
        }

        if (!ps_lexer_read_quoted_string_character(lexer, string, pos, current_char))
            return false;
    }
}

static bool ps_lexer_read_char_or_string_value_quote(ps_lexer *lexer)
{
    int current_char = '\0';
    unsigned int pos = 0;
    bool success = false;
    ps_string *string = NULL;

    string = ps_string_alloc(0);
    if (string == NULL)
    {
        (void)ps_lexer_return_error(lexer, PS_ERROR_OUT_OF_MEMORY, "Cannot allocate string value");
        goto cleanup;
    }
    current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
    {
        (void)ps_lexer_return_error(lexer, PS_ERROR_EOF, "Unexpected end of file");
        goto cleanup;
    }
    if (current_char != '\'')
    {
        (void)ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,
                                    "Char or string value must start with a single quote");
        goto cleanup;
    }
    if (!ps_lexer_read_next_char(lexer))
        goto cleanup;
    if (!ps_lexer_read_quoted_string_content(lexer, &string, &pos))
        goto cleanup;
    string->str[pos] = '\0';
    if (pos == 1)
    {
        lexer->current_token.type = PS_TOKEN_CHAR_VALUE;
        lexer->current_token.value.c = string->str[0];
    }
    else
    {
        if (lexer->current_token.type == PS_TOKEN_STRING_VALUE && lexer->current_token.value.s != NULL)
            (void)ps_string_free(lexer->current_token.value.s);
        lexer->current_token.type = PS_TOKEN_STRING_VALUE;
        lexer->current_token.value.s = string;
        string = NULL;
    }
    success = true;

cleanup:
    (void)ps_string_free(string);
    return success;
}

static bool ps_lexer_read_char_value_caret(ps_lexer *lexer)
{
    int current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
        return ps_lexer_return_error(lexer, PS_ERROR_EOF, "Unexpected end of file");
    if (current_char != '^')
        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,
                                     "Expected a caret (^) character for char value");
    ADVANCE()

    current_char = ps_buffer_peek_char(lexer->buffer);

    if (lexer->current_token.type == PS_TOKEN_STRING_VALUE && lexer->current_token.value.s != NULL)
        lexer->current_token.value.s = ps_string_free(lexer->current_token.value.s);

    if (current_char >= '@' && current_char <= '_')
    {
        current_char = current_char - '@';
        lexer->current_token.type = PS_TOKEN_CHAR_VALUE;
        lexer->current_token.value.c = (ps_char)current_char;
    }
    else if (current_char >= 'a' && current_char <= 'z')
    {
        current_char = current_char - 'a' + 1;
        lexer->current_token.type = PS_TOKEN_CHAR_VALUE;
        lexer->current_token.value.c = (ps_char)current_char;
    }
    else
        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,
                                     "Invalid character after caret (^) for char value");

    ADVANCE()
    return true;
}

static bool ps_lexer_read_char_value_hash(ps_lexer *lexer)
{
    int current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
        return ps_lexer_return_error(lexer, PS_ERROR_EOF, "Unexpected end of file");
    if (current_char != '#')
        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,
                                     "Expected a hash (#) character for char value");
    ADVANCE()
    if (lexer->current_token.type == PS_TOKEN_STRING_VALUE && lexer->current_token.value.s != NULL)
        lexer->current_token.value.s = ps_string_free(lexer->current_token.value.s);
    if (!ps_lexer_read_number(lexer))
        return false;
    if (lexer->current_token.type != PS_TOKEN_INTEGER_VALUE && lexer->current_token.type != PS_TOKEN_UNSIGNED_VALUE)
        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,
                                     "Expected an integer or unsigned value after hash (#) for char value");
    if (lexer->current_token.value.u > 255)
        return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Char value must be in the range 0..255");
    lexer->current_token.type = PS_TOKEN_CHAR_VALUE;
    lexer->current_token.value.c = (ps_char)lexer->current_token.value.u;
    return true;
}

static bool ps_lexer_append_quoted_value(ps_lexer *lexer, ps_token *token, int *pos)
{
    if (token->type == PS_TOKEN_NONE)
    {
        if (lexer->current_token.type == PS_TOKEN_CHAR_VALUE)
        {
            token->type = PS_TOKEN_CHAR_VALUE;
            token->value.c = lexer->current_token.value.c;
            *pos = 1;
            return true;
        }
        token->type = PS_TOKEN_STRING_VALUE;
        if (token->value.s == NULL)
        {
            token->value.s = ps_string_alloc(252);
            if (token->value.s == NULL)
                return ps_lexer_return_error(lexer, PS_ERROR_OUT_OF_MEMORY, "TOKEN!");
        }
        ps_string *new = ps_string_append(token->value.s, lexer->current_token.value.s);
        if (new == NULL)
            return ps_lexer_return_error(lexer, PS_ERROR_STRING_TOO_LONG, "Too many characters in string value");
        token->value.s = new;
        *pos = new->len;
        return true;
    }
    if (token->type == PS_TOKEN_CHAR_VALUE)
    {
        if (lexer->current_token.type == PS_TOKEN_CHAR_VALUE)
        {
            ps_string *new = ps_string_concat_chars(token->value.c, lexer->current_token.value.c);
            *pos = 2;
            token->type = PS_TOKEN_STRING_VALUE;
            token->value.s = new;
            return true;
        }
        token->type = PS_TOKEN_STRING_VALUE;
        token->value.s = ps_string_create_char(token->value.c);
        if (token->value.s == NULL)
            return ps_lexer_return_error(lexer, ps_error_map_errno(), "CREATE_CHAR!");
        ps_string *new = ps_string_append(token->value.s, lexer->current_token.value.s);
        if (new == NULL)
            return ps_lexer_return_error(lexer, ps_error_map_errno(), "APPEND_STRING!");
        token->value.s = new;
        *pos += new->len;
        return true;
    }
    if (lexer->current_token.type == PS_TOKEN_CHAR_VALUE)
    {
        if (*pos >= (int)PS_STRING_MAX_LEN)
            return ps_lexer_return_error(lexer, PS_ERROR_STRING_TOO_LONG, "Too many characters in string value");
        ps_string *new = ps_string_append_char(token->value.s, lexer->current_token.value.c);
        if (new == NULL)
            return ps_lexer_return_error(lexer, ps_error_map_errno(), "APPEND_CHAR!");
        token->value.s = new;
        *pos += 1;
        return true;
    }
    ps_string *new = ps_string_append(token->value.s, lexer->current_token.value.s);
    if (new == NULL)
        return ps_lexer_return_error(lexer, ps_error_map_errno(), "APPEND_STRING!");
    token->value.s = new;
    *pos += new->len;
    return true;
}

static bool ps_lexer_append_escaped_value(ps_lexer *lexer, ps_token *token, int *pos, char current_char)
{
    if (current_char == '^' && !ps_lexer_read_char_value_caret(lexer))
        return false;
    if (current_char == '#' && !ps_lexer_read_char_value_hash(lexer))
        return false;
    if (token->type == PS_TOKEN_NONE)
    {
        token->type = PS_TOKEN_CHAR_VALUE;
        token->value = lexer->current_token.value;
        *pos = 1;
        return true;
    }
    if (token->type == PS_TOKEN_CHAR_VALUE)
    {
        token->type = PS_TOKEN_STRING_VALUE;
        ps_string *new = ps_string_concat_chars(token->value.c, lexer->current_token.value.c);
        if (new == NULL)
            return ps_lexer_return_error(lexer, ps_error_map_errno(), "CONCAT_CHARS!");
        token->value.s = new;
        *pos = 2;
        return true;
    }
    if (token->type == PS_TOKEN_STRING_VALUE)
    {
        if (*pos >= (int)PS_STRING_MAX_LEN)
            return ps_lexer_return_error(lexer, PS_ERROR_STRING_TOO_LONG, "Too many characters in string value");
        ps_string *new = ps_string_append_char(token->value.s, lexer->current_token.value.c);
        if (new == NULL)
            return ps_lexer_return_error(lexer, ps_error_map_errno(), "APPEND_CHAR!");
        token->value.s = new;
        return true;
    }
    return false;
}

static bool ps_lexer_read_char_or_string_component(ps_lexer *lexer, ps_token *token, int *pos, int current_char)
{
    if (current_char == '\'')
    {
        if (!ps_lexer_read_char_or_string_value_quote(lexer))
            return false;
        return ps_lexer_append_quoted_value(lexer, token, pos);
    }
    if (current_char == '^' || current_char == '#')
        return ps_lexer_append_escaped_value(lexer, token, pos, (char)current_char);
    return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "Unexpected character in char or string value");
}

bool ps_lexer_read_char_or_string_value(ps_lexer *lexer)
{
    ps_token token = {.type = PS_TOKEN_NONE, .value = {.identifier = {0}}};
    int pos = 0;
    bool success = false;
    int current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
        return ps_lexer_return_error(lexer, PS_ERROR_EOF, "Unexpected end of file");
    do
    {
        if (!ps_lexer_read_char_or_string_component(lexer, &token, &pos, current_char) ||
            !ps_lexer_skip_whitespace_and_comments(lexer))
            goto cleanup;
        current_char = ps_buffer_peek_char(lexer->buffer);
    } while (current_char != EOF && (current_char == '\'' || current_char == '^' || current_char == '#'));
    if (lexer->current_token.type == PS_TOKEN_STRING_VALUE && lexer->current_token.value.s != NULL)
        (void)ps_string_free(lexer->current_token.value.s);
    lexer->current_token.type = token.type;
    lexer->current_token.value = token.value;
    success = true;

cleanup:
    if (!success && token.type == PS_TOKEN_STRING_VALUE)
        (void)ps_string_free(token.value.s);
    return success;
}
