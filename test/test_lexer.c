/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>
#include <sys/resource.h>

#include "../include/ps_config.h"
#include "../include/ps_error.h"
#include "../include/ps_readall.h"
#include "../include/ps_token.h"
#include "../include/ps_lexer.h"

#include "../src/ps_error.c"
#include "../src/ps_readall.c"
#include "../src/ps_buffer.c"
#include "../src/ps_token.c"
#include "../src/ps_lexer.c"

ps_lexer _lexer;
ps_lexer *lexer = &_lexer;

char *minimal_source =
    "Program Minimal;\n"
    "{ Comment made with curly brackets }   \n"
    "Begin\n"
    "  (* Comment with parenthesis & stars *)\n"
    "  { (* Comment within comment *) }\n"
    "  (* { Comment within comment } *)\n"
    "End.\n"
    "";

ps_token_type minimal_expected[] = {
    // PROGRAM MINIMAL;
    TOKEN_PROGRAM, TOKEN_IDENTIFIER, TOKEN_SEMI_COLON,
    // BEGIN
    TOKEN_BEGIN,
    // END.
    TOKEN_END, TOKEN_DOT
    //
};

char *hello_source =
    "Program HelloWorld;\n"
    "Const\n"
    "  K1 = 1234;\n"
    "  K2 = 'Select ''A'' or ''B''.';\n"
    "  { K3 = '''Doesn''t work yet!'''; }\n"
    "Begin\n"
    "  { Comment1 (* Comment2 *) }\n"
    "  WriteLn('Hello, World!');\n"
    "  WriteLn(K1, K2);\n"
    "End.\n"
    "";

ps_token_type hello_expected[] = {
    // PROGRAM HELLOWORLD;
    TOKEN_PROGRAM, TOKEN_IDENTIFIER, TOKEN_SEMI_COLON,
    // CONST
    TOKEN_CONST,
    // K1 = 1234;
    TOKEN_IDENTIFIER, TOKEN_EQUAL, TOKEN_CARDINAL_VALUE, TOKEN_SEMI_COLON,
    // K2 = 'Choose ''A'' or ''B''.';\n"
    TOKEN_IDENTIFIER, TOKEN_EQUAL, TOKEN_STRING_VALUE, TOKEN_SEMI_COLON,
    // BEGIN
    TOKEN_BEGIN,
    // WRITELN('Hello, World!');
    TOKEN_IDENTIFIER, TOKEN_LEFT_PARENTHESIS, TOKEN_STRING_VALUE, TOKEN_RIGHT_PARENTHESIS, TOKEN_SEMI_COLON,
    // WRITELN(K1, K2);
    TOKEN_IDENTIFIER, TOKEN_LEFT_PARENTHESIS, TOKEN_IDENTIFIER, TOKEN_COMMA, TOKEN_IDENTIFIER, TOKEN_RIGHT_PARENTHESIS, TOKEN_SEMI_COLON,
    // END.
    TOKEN_END, TOKEN_DOT
    //
};

char *quotes_source =
    "Program Quotes;\n"
    "Const K = '''X''=''Y'' ';\n"
    "Begin\n"
    "End.\n"
    "";

ps_token_type quotes_expected[] = {
    // PROGRAM QUOTE;
    TOKEN_PROGRAM, TOKEN_IDENTIFIER, TOKEN_SEMI_COLON,
    // CONST K = '''X1''=''Y1'' ';
    TOKEN_CONST, TOKEN_IDENTIFIER, TOKEN_EQUAL, TOKEN_STRING_VALUE, TOKEN_SEMI_COLON,
    // BEGIN
    TOKEN_BEGIN,
    // END.
    TOKEN_END, TOKEN_DOT
    //
};

void test(char *name, char *source, ps_token_type *expected, int count)
{
    int index;

    printf("TEST LEXER: INIT %s\n", name);
    ps_lexer_init(lexer);
    lexer->buffer.debug = 0;
    ps_buffer_set_text(&lexer->buffer, source, strlen(source));
    ps_buffer_dump(&lexer->buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST LEXER: LOOP ON %s\n", name);
    index = 0;
    ps_buffer_read_next_char(&lexer->buffer);
    if (lexer->error != PS_LEXER_ERROR_NONE || lexer->buffer.error != PS_BUFFER_ERROR_NONE)
    {
        printf("TEST LEXER: ERROR %d %s / %d %s\n",
               lexer->error,
               ps_error_get_message(lexer->error),
               lexer->buffer.error,
               ps_error_get_message(lexer->buffer.error));
        return;
    }
    do
    {
        // printf("TEST LEXER: %02d/%02d BEGIN\n", index + 1, count);
        if (!ps_lexer_read_next_token(lexer))
        {
            printf("TEST LEXER: %02d/%02d ERROR %d %s / %d %s\n",
                   index + 1, count,
                   lexer->error,
                   ps_error_get_message(lexer->error),
                   lexer->buffer.error,
                   ps_error_get_message(lexer->buffer.error));
            break;
        }
        else if (lexer->current_token.type != expected[index])
        {
            printf("TEST LEXER: %02d/%02d ERROR EXPECTED TOKEN %4d, GOT %4d ",
                   index + 1, count,
                   expected[index],
                   lexer->current_token.type);
            ps_token_dump(&lexer->current_token);
            break;
        }
        else
        {
            printf("TEST LEXER: %02d/%02d OK    EXPECTED TOKEN %4d, GOT %4d ",
                   index + 1, count,
                   expected[index],
                   lexer->current_token.type);
            ps_token_dump(&lexer->current_token);
        }
        // printf("TEST LEXER: %02d/%02d END\n", index + 1, count);
        index += 1;
    } while (index < count);
}

int main(void)
{
    struct rlimit rl = {256 * 1024 * 12, 256 * 1024 * 12};
    setrlimit(RLIMIT_AS, &rl);

    printf("TEST LEXER: BEGIN\n");
    printf("================================================================================\n");
    test("MINIMAL", minimal_source, minimal_expected, sizeof(minimal_expected) / sizeof(ps_token_type));
    // printf("================================================================================\n");
    // test("HELLO", hello_source, hello_expected, sizeof(hello_expected) / sizeof(ps_token_type_t));
    // printf("================================================================================\n");
    // test("QUOTES", quotes_source, quotes_expected, sizeof(quotes_expected) / sizeof(ps_token_type_t));
    printf("================================================================================\n");
    printf("TEST LEXER: END\n");
    return 0;
}
