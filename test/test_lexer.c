/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
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

char *empty_source =
    "Program Empty;\n"
    "{ Comment made with curly brackets }   \n"
    "Begin\n"
    "  (* Comment with parenthesis & stars *)\n"
    "  { (* Comment within comment *) }\n"
    "  (* { Comment within comment } *)\n"
    "  // Comment with 2 slashes\n"
    "End.\n"
    "";
ps_token_type empty_expected[] = {
    // PROGRAM EMPTY;
    PS_TOKEN_PROGRAM, PS_TOKEN_IDENTIFIER, PS_TOKEN_SEMI_COLON,
    // BEGIN
    PS_TOKEN_BEGIN,
    // END.
    PS_TOKEN_END, PS_TOKEN_DOT
    //
};

// char *hello_source =
//     "Program HelloWorld;\n"
//     "Const\n"
//     "  K1 = 1234;\n"
//     "  K2 = 'Select A or B.';\n"
//     "Begin\n"
//     "  { Comment1 (* Comment2 *) }\n"
//     "  WriteLn('Hello, World!');\n"
//     "  WriteLn(K1, K2);\n"
//     "End.\n"
//     "";
// ps_token_type hello_expected[] = {
//     // PROGRAM HELLOWORLD;
//     PS_TOKEN_PROGRAM, PS_TOKEN_IDENTIFIER, PS_TOKEN_SEMI_COLON,
//     // CONST
//     PS_TOKEN_CONST,
//     // K1 = 1234;
//     PS_TOKEN_IDENTIFIER, PS_TOKEN_EQUAL, PS_TOKEN_UNSIGNED_VALUE, PS_TOKEN_SEMI_COLON,
//     // K2 = 'Choose ''A'' or ''B''.';\n"
//     PS_TOKEN_IDENTIFIER, PS_TOKEN_EQUAL, PS_TOKEN_STRING_VALUE, PS_TOKEN_SEMI_COLON,
//     // BEGIN
//     PS_TOKEN_BEGIN,
//     // WRITELN('Hello, World!');
//     PS_TOKEN_IDENTIFIER, PS_TOKEN_LEFT_PARENTHESIS, PS_TOKEN_STRING_VALUE, PS_TOKEN_RIGHT_PARENTHESIS, PS_TOKEN_SEMI_COLON,
//     // WRITELN(K1, K2);
//     PS_TOKEN_IDENTIFIER, PS_TOKEN_LEFT_PARENTHESIS, PS_TOKEN_IDENTIFIER, PS_TOKEN_COMMA, PS_TOKEN_IDENTIFIER, PS_TOKEN_RIGHT_PARENTHESIS, PS_TOKEN_SEMI_COLON,
//     // END.
//     PS_TOKEN_END, PS_TOKEN_DOT
//     // EOF
// };

char *quotes_source =
    "Program Quotes;\n"
    // "Const K = '''X''=''Y''';\n"
    "Const K1 = 'X';\n"
    "Const K2 = '''';\n"
    "Begin\n"
    "  Write(K1);\n"
    "End.\n"
    "";
ps_token_type quotes_expected[] = {
    // PROGRAM QUOTES;
    PS_TOKEN_PROGRAM, PS_TOKEN_IDENTIFIER, PS_TOKEN_SEMI_COLON,
    // // CONST K = '''X''=''Y'' ';
    // PS_TOKEN_CONST, PS_TOKEN_IDENTIFIER, PS_TOKEN_EQUAL, PS_TOKEN_STRING_VALUE, PS_TOKEN_SEMI_COLON,
    // CONST K1 = 'K';
    PS_TOKEN_CONST, PS_TOKEN_IDENTIFIER, PS_TOKEN_EQUAL, PS_TOKEN_CHAR_VALUE, PS_TOKEN_SEMI_COLON,
    // CONST K2 = '''';
    PS_TOKEN_CONST, PS_TOKEN_IDENTIFIER, PS_TOKEN_EQUAL, PS_TOKEN_CHAR_VALUE, PS_TOKEN_SEMI_COLON,
    // BEGIN
    PS_TOKEN_BEGIN,
    // WRITE(K1);
    PS_TOKEN_IDENTIFIER, PS_TOKEN_LEFT_PARENTHESIS, PS_TOKEN_IDENTIFIER, PS_TOKEN_RIGHT_PARENTHESIS, PS_TOKEN_SEMI_COLON,
    // END.
    PS_TOKEN_END, PS_TOKEN_DOT
    // EOF
};

void test_lexer(char *name, char *source, ps_token_type *expected, int count)
{
    int index;

    printf("TEST LEXER: INIT %s\n", name);
    lexer = ps_lexer_alloc(NULL);
    lexer->buffer->debug = 0;
    ps_buffer_load_string(lexer->buffer, source, strlen(source));
    ps_buffer_dump(lexer->buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST LEXER: LOOP ON %s\n", name);
    index = 0;
    ps_buffer_read_next_char(lexer->buffer);
    if (lexer->error != PS_ERROR_NONE || lexer->buffer->error != PS_ERROR_NONE)
    {
        printf("TEST LEXER: ERROR %d %s / %d %s\n",
               lexer->error,
               ps_error_get_message(lexer->error),
               lexer->buffer->error,
               ps_error_get_message(lexer->buffer->error));
        return;
    }
    do
    {
        // printf("TEST LEXER: %02d/%02d BEGIN\n", index + 1, count);
        if (!ps_lexer_read_token(lexer))
        {
            printf("TEST LEXER: %02d/%02d ERROR %d %s / %d %s\n",
                   index + 1, count,
                   lexer->error,
                   ps_error_get_message(lexer->error),
                   lexer->buffer->error,
                   ps_error_get_message(lexer->buffer->error));
            break;
        }
        else if (lexer->current_token.type != expected[index])
        {
            printf("TEST LEXER: %02d/%02d ERROR EXPECTED TOKEN %4d, GOT %4d ",
                   index + 1, count,
                   expected[index],
                   lexer->current_token.type);
            ps_token_debug(stdout, "", &lexer->current_token);
            break;
        }
        else
        {
            printf("TEST LEXER: %02d/%02d OK    EXPECTED TOKEN %4d, GOT %4d ",
                   index + 1, count,
                   expected[index],
                   lexer->current_token.type);
            ps_token_debug(stdout, "", &lexer->current_token);
        }
        // printf("TEST LEXER: %02d/%02d END\n", index + 1, count);
        index += 1;
    } while (index < count);
}

int main(void)
{
    struct rlimit rl = {1024 * 1024 * 3, 1024 * 1024 * 3};
    setrlimit(RLIMIT_AS, &rl);

    printf("TEST LEXER: BEGIN\n");

    // printf("================================================================================\n");
    // test_lexer("EMPTY", empty_source, empty_expected, sizeof(empty_expected) / sizeof(ps_token_type));

    // printf("================================================================================\n");
    // test_lexer("HELLO", hello_source, hello_expected, sizeof(hello_expected) / sizeof(ps_token_type));

    printf("================================================================================\n");
    test_lexer("QUOTES", quotes_source, quotes_expected, sizeof(quotes_expected) / sizeof(ps_token_type));

    printf("================================================================================\n");
    printf("TEST LEXER: END\n");
    return 0;
}
