/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "ps_config.h"
#include "ps_error.h"
#include "ps_lexer_internal.h"
#include "ps_tools.h"

static bool ps_lexer_read_number_bin_oct_hex(ps_lexer *lexer, int base, const char *digits)
{
    ps_char buffer[PS_BITNESS + 1] = {0};
    int current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
        return ps_lexer_return_error(lexer, PS_ERROR_EOF, "Unexpected end of file");
    int pos = 0;
    char *end = NULL;
    if (strchr(digits, current_char) == NULL)
        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "Invalid digit at start of number");
    do
    {
        if (pos >= PS_BITNESS)
            return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Too many digits in number");
        buffer[pos++] = (char)current_char;
        GET_NEXT_CHAR(current_char)
    } while (strchr(digits, current_char) != NULL);
    buffer[pos] = '\0';
    errno = 0;
    unsigned long ul = strtoul((char *)buffer, &end, base);
    if (errno == ERANGE || end == (char *)buffer || ul > PS_UNSIGNED_MAX)
        return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Invalid unsigned integer value");
    lexer->current_token.type = PS_TOKEN_UNSIGNED_VALUE;
    lexer->current_token.value.u = (ps_unsigned)ul;
    return true;
}

#define APPEND_CHAR_DEC()                                                                                              \
    {                                                                                                                  \
        if (pos >= PS_BITNESS)                                                                                         \
            return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Too many digits in number");                       \
        buffer[pos++] = (ps_char)current_char;                                                                         \
        GET_NEXT_CHAR(current_char)                                                                                    \
    }

#define READ_DOT_DEC()                                                                                                 \
    {                                                                                                                  \
        if (is_real)                                                                                                   \
            return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,                                         \
                                         "Only one dot allowed in real constants");                                    \
        if (has_exponent)                                                                                              \
            return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "no dot in exponent allowed");          \
        if (ps_buffer_peek_next_char(lexer->buffer) == '.')                                                            \
            break;                                                                                                     \
        is_real = true;                                                                                                \
        APPEND_CHAR_DEC()                                                                                              \
        if (current_char == 'e' || current_char == 'E')                                                                \
            READ_EXP_DEC()                                                                                             \
    }

#define READ_EXP_DEC()                                                                                                 \
    {                                                                                                                  \
        if (has_exponent)                                                                                              \
            return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,                                         \
                                         "Only one exponent allowed in real constants");                               \
        is_real = true;                                                                                               \
        has_exponent = true;                                                                                           \
        APPEND_CHAR_DEC()                                                                                               \
        int c2 = ps_buffer_peek_char(lexer->buffer);                                                                   \
        if (c2 == EOF)                                                                                                  \
            return ps_lexer_return_error(lexer, PS_ERROR_EOF, "Unexpected end of file");                                \
        if (c2 != '+' && c2 != '-' && !isdigit(c2))                                                                    \
            return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,                                          \
                                         "Exponent must begin by +, - or a digit in Real constants");                   \
        if (current_char == '+' || current_char == '-')                                                                \
        {                                                                                                              \
            APPEND_CHAR_DEC()                                                                                           \
            c2 = ps_buffer_peek_char(lexer->buffer);                                                                    \
            if (c2 == '+' || c2 == '-')                                                                                \
                return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,                                      \
                                             "Only one exponent sign allowed in Real constants");                       \
            if (!isdigit(c2))                                                                                           \
                return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER,                                      \
                                             "Exponent sign must be followed by a digit in Real constants");            \
        }                                                                                                              \
    }

static bool ps_lexer_read_number_dec(ps_lexer *lexer)
{
    ps_char buffer[PS_BITNESS + 1] = {0};
    int pos = 0;
    bool is_real = false;
    bool has_exponent = false;
    char *end = NULL;
    int current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
        return ps_lexer_return_error(lexer, PS_ERROR_EOF, "Unexpected end of file");

    if (!isdigit(current_char) && current_char != '.')
        return ps_lexer_return_error(lexer, PS_ERROR_UNEXPECTED_CHARACTER, "Invalid digit at start of number");

    if (current_char == '.')
    {
        is_real = true;
        buffer[pos++] = '0';
        buffer[pos++] = '.';
        GET_NEXT_CHAR(current_char)
    }

    do
    {
        APPEND_CHAR_DEC()
        if (current_char == '.')
            READ_DOT_DEC()
        else if (current_char == 'e' || current_char == 'E')
            READ_EXP_DEC()
    } while (isdigit(current_char));
    buffer[pos] = '\0';
    if (is_real)
    {
        errno = 0;
        double d = strtod((char *)buffer, &end);
        if (errno == ERANGE || end == (char *)buffer || d > PS_REAL_MAX)
            return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Invalid real number value");
        lexer->current_token.type = PS_TOKEN_REAL_VALUE;
        lexer->current_token.value.r = (ps_real)d;
    }
    else
    {
        errno = 0;
        unsigned long u = strtoul((char *)buffer, &end, 10);
        if (errno == ERANGE || end == (char *)buffer || u > PS_UNSIGNED_MAX)
            return ps_lexer_return_error(lexer, PS_ERROR_OVERFLOW, "Invalid unsigned integer value");
        if (u <= PS_INTEGER_MAX)
        {
            lexer->current_token.type = PS_TOKEN_INTEGER_VALUE;
            lexer->current_token.value.i = (ps_integer)u;
        }
        else
        {
            lexer->current_token.type = PS_TOKEN_UNSIGNED_VALUE;
            lexer->current_token.value.u = (ps_unsigned)u;
        }
    }
    return true;
}

bool ps_lexer_read_number(ps_lexer *lexer)
{
    int current_char = ps_buffer_peek_char(lexer->buffer);
    if (current_char == EOF)
        return ps_lexer_return_error(lexer, PS_ERROR_EOF, "Unexpected end of file");
    if (current_char == '%')
    {
        GET_NEXT_CHAR(current_char)
        return ps_lexer_read_number_bin_oct_hex(lexer, 2, "01");
    }
    if (current_char == '&')
    {
        GET_NEXT_CHAR(current_char)
        return ps_lexer_read_number_bin_oct_hex(lexer, 8, "01234567");
    }
    if (current_char == '$')
    {
        GET_NEXT_CHAR(current_char)
        return ps_lexer_read_number_bin_oct_hex(lexer, 16, "01234567abcdefABCDEF");
    }
    return ps_lexer_read_number_dec(lexer);
}
