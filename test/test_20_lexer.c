/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_config.h"
#include "ps_error.h"
#include "ps_lexer.h"
#include "ps_memory.h"
#include "ps_readall.h"
#include "ps_token.h"
#include "ps_tools.h"

char *empty_source = "Program Empty;\n"
                     "{ Comment made with curly brackets }   \n"
                     "Begin\n"
                     "  (* Comment with parenthesis & stars *)\n"
                     "  { (* Comment within comment *) }\n"
                     "  (* { Comment within comment } *)\n"
                     "  // Comment with 2 slashes\n"
                     "End.\n"
                     "";
ps_token_type empty_expected[] = {
        // clang-format off
        PS_TOKEN_PROGRAM, PS_TOKEN_IDENTIFIER, PS_TOKEN_SEMI_COLON,
        PS_TOKEN_BEGIN,
        PS_TOKEN_END, PS_TOKEN_DOT
        // clang-format on
};

char *hello_source = "Program HelloWorld;\n"
                     "Const\n"
                     "  K1 = 1234;\n"
                     "  K2 = 'Select A or B.';\n"
                     "Begin\n"
                     "  { Comment1 (* Comment2 *) }\n"
                     "  WriteLn('Hello, World!');\n"
                     "  (* comment before\n"
                     "\n"
                     "     comment after *)\n"
                     "  { comment before\n"
                     "\n"
                     "    comment after }\n"
                     "  WriteLn(K1, K2);\n"
                     "End.\n"
                     "";
ps_token_type hello_expected[] = {
        // clang-format off
        PS_TOKEN_PROGRAM, PS_TOKEN_IDENTIFIER, PS_TOKEN_SEMI_COLON,
        PS_TOKEN_CONST,
        PS_TOKEN_IDENTIFIER, PS_TOKEN_EQ, PS_TOKEN_INTEGER_VALUE, PS_TOKEN_SEMI_COLON,
        PS_TOKEN_IDENTIFIER, PS_TOKEN_EQ, PS_TOKEN_STRING_VALUE, PS_TOKEN_SEMI_COLON,
        PS_TOKEN_BEGIN,
        PS_TOKEN_IDENTIFIER, PS_TOKEN_LEFT_PARENTHESIS, PS_TOKEN_STRING_VALUE, PS_TOKEN_RIGHT_PARENTHESIS,
        PS_TOKEN_SEMI_COLON,
        PS_TOKEN_IDENTIFIER, PS_TOKEN_LEFT_PARENTHESIS, PS_TOKEN_IDENTIFIER, PS_TOKEN_COMMA, PS_TOKEN_IDENTIFIER,
        PS_TOKEN_RIGHT_PARENTHESIS, PS_TOKEN_SEMI_COLON,
        PS_TOKEN_END, PS_TOKEN_DOT
        // clang-format on
};

char *quotes_source = "Program Quotes;\n"
                      "Const K = '''X''=''Y''';\n"
                      "Const K1 = 'X';\n"
                      "Const K2 = '''';\n"
                      "Begin\n"
                      "  Write(K1);\n"
                      "End.\n"
                      "";
ps_token_type quotes_expected[] = {
        // clang-format off
        PS_TOKEN_PROGRAM, PS_TOKEN_IDENTIFIER, PS_TOKEN_SEMI_COLON,
        PS_TOKEN_CONST, PS_TOKEN_IDENTIFIER, PS_TOKEN_EQ, PS_TOKEN_STRING_VALUE, PS_TOKEN_SEMI_COLON,
        PS_TOKEN_CONST, PS_TOKEN_IDENTIFIER, PS_TOKEN_EQ, PS_TOKEN_CHAR_VALUE, PS_TOKEN_SEMI_COLON,
        PS_TOKEN_CONST, PS_TOKEN_IDENTIFIER, PS_TOKEN_EQ, PS_TOKEN_CHAR_VALUE, PS_TOKEN_SEMI_COLON,
        PS_TOKEN_BEGIN,
        PS_TOKEN_IDENTIFIER, PS_TOKEN_LEFT_PARENTHESIS, PS_TOKEN_IDENTIFIER, PS_TOKEN_RIGHT_PARENTHESIS,
        PS_TOKEN_SEMI_COLON,
        PS_TOKEN_END, PS_TOKEN_DOT
        // clang-format on
};

char *range_source = "1..10";
ps_token_type range_expected[] = {PS_TOKEN_INTEGER_VALUE, PS_TOKEN_RANGE, PS_TOKEN_INTEGER_VALUE};

char *real_exponent_source = "1.e2";
ps_token_type real_exponent_expected[] = {PS_TOKEN_REAL_VALUE};

bool test_lexer(char *name, char *source, const ps_token_type *expected, int count)
{
    int index;
    bool failed = false;

    printf("TEST LEXER: INIT %s\n", name);
    ps_lexer *lexer = ps_lexer_alloc();
    if (lexer == NULL)
    {
        printf("TEST LEXER: ERROR allocating lexer\n");
        return false;
    }
    printf("TEST LEXER: LOAD %s\n", name);
    if (!ps_buffer_load_string(lexer->buffer, source, strlen(source)))
    {
        printf("TEST LEXER: ERROR %d %s / %d %s\n", lexer->error, ps_error_get_message(lexer->error),
               lexer->buffer->error, ps_error_get_message(lexer->buffer->error));
        ps_lexer_free(lexer);
        return false;
    }
    ps_buffer_dump(stdout, lexer->buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST LEXER: LOOP ON %s\n", name);
    index = 0;
    failed = false;
    ps_buffer_read_next_char(lexer->buffer);
    if (lexer->error != PS_ERROR_NONE || lexer->buffer->error != PS_ERROR_NONE)
    {
        printf("TEST LEXER: ERROR %d %s / %d %s\n", lexer->error, ps_error_get_message(lexer->error),
               lexer->buffer->error, ps_error_get_message(lexer->buffer->error));
        return false;
    }
    do
    {
        if (!ps_lexer_read_token(lexer))
        {
            printf("TEST LEXER: %02d/%02d ERROR %d %s / %d %s\n", index + 1, count, lexer->error,
                   ps_error_get_message(lexer->error), lexer->buffer->error,
                   ps_error_get_message(lexer->buffer->error));
            failed = true;
        }
        else if (lexer->current_token.type != expected[index])
        {
            printf("TEST LEXER: %02d/%02d ERROR EXPECTED TOKEN %4d, GOT %4d ", index + 1, count, expected[index],
                   lexer->current_token.type);
            ps_token_debug(stdout, "", &lexer->current_token);
            failed = true;
        }
        else
        {
            printf("TEST LEXER: %02d/%02d OK    EXPECTED TOKEN %4d, GOT %4d ", index + 1, count, expected[index],
                   lexer->current_token.type);
            ps_token_debug(stdout, "", &lexer->current_token);
        }
        // printf("TEST LEXER: %02d/%02d END\n", index + 1, count);
        if (!failed)
            index += 1;
    } while (index < count && !failed);
    ps_lexer_free(lexer);
    return !failed;
}

static bool test_empty_string_plus_char(void)
{
    char source[] = "'POISON', ''#65";
    ps_lexer *lexer = ps_lexer_alloc();
    if (lexer == NULL)
    {
        printf("TEST LEXER: ERROR allocating lexer for empty string plus char\n");
        return false;
    }

    bool ok = true;
    ok = ok && !ps_buffer_load_string(lexer->buffer, source, sizeof(source) - 1);
    ok = ok && !ps_buffer_read_next_char(lexer->buffer);
    ok = ok && !ps_lexer_read_token(lexer);
    ok = ok && lexer->current_token.type != PS_TOKEN_STRING_VALUE;
    ok = ok && !ps_lexer_read_token(lexer);
    ok = ok && lexer->current_token.type != PS_TOKEN_COMMA;
    ok = ok && !ps_lexer_read_token(lexer);
    ok = ok && lexer->current_token.type != PS_TOKEN_STRING_VALUE;
    ok = ok && (lexer->current_token.value.s[0] != 'A');
    ok = ok && (lexer->current_token.value.s[1] != '\0');
    if (!ok)
        printf("TEST LEXER: ERROR empty string plus char should produce terminated string \"A\"\n");

    ps_lexer_free(lexer);
    return ok;
}

int main(void)
{
    printf("TEST LEXER: BEGIN\n");

    printf("================================================================================\n");
    if (!test_lexer("EMPTY", empty_source, empty_expected, sizeof(empty_expected) / sizeof(ps_token_type)))
        goto failure;

    printf("================================================================================\n");
    if (!test_lexer("HELLO", hello_source, hello_expected, sizeof(hello_expected) / sizeof(ps_token_type)))
        goto failure;

    printf("================================================================================\n");
    if (!test_lexer("QUOTES", quotes_source, quotes_expected, sizeof(quotes_expected) / sizeof(ps_token_type)))
        goto failure;

    printf("================================================================================\n");
    if (!test_lexer("RANGE", range_source, range_expected, sizeof(range_expected) / sizeof(ps_token_type)))
        goto failure;

    printf("================================================================================\n");
    if (!test_lexer("REAL_EXPONENT", real_exponent_source, real_exponent_expected,
                    sizeof(real_exponent_expected) / sizeof(ps_token_type)))
        goto failure;

    printf("================================================================================\n");
    if (!test_empty_string_plus_char())
        goto failure;

    printf("================================================================================\n");
    printf("TEST LEXER: END\n");
    return EXIT_SUCCESS;

failure:
    printf("TEST LEXER: FAILED!\n");
    return EXIT_FAILURE;
}
