/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "../include/ps_error.h"
#include "../include/ps_readall.h"
#include "../include/ps_token.h"
#include "../include/ps_lexer.h"

#include "../src/ps_error.c"
#include "../src/ps_readall.c"
#include "../src/ps_buffer.c"
#include "../src/ps_token.c"
#include "../src/ps_lexer.c"

lexer_t _lexer;
lexer_t *lexer = &_lexer;

char *minimal =
    "Program Minimal;\n"
    "{ Comment made with curly brackets }   \n"
    "Begin\n"
    "  (* Comment with parenthesis & stars *)\n"
    "End.\n";

token_type_t token_types_minimal[] = {
    // PROGRAM MINIMAL;
    TOKEN_PROGRAM, TOKEN_IDENTIFIER, TOKEN_SEMI_COLON,
    // BEGIN
    TOKEN_BEGIN,
    // END.
    TOKEN_END, TOKEN_DOT};

// "  K = 'The Quick Brown Fox Jumps Over The Lazy Dog. 0123456789 Times!';\n"
char *hello =
    //  |         1         2         3         4         5         6         7         8|
    //  |12345678901234567890123456789012345678901234567890123456789012345678901234567890|
    "Program Hello;\n"
    "Const\n"
    "  K = 1234;\n"
    "Begin\n"
    // "  WriteLn('Hello, World!');\n"
    "  WriteLn(k);\n"
    "End.\n";

token_type_t expected_hello[] = {
    // PROGRAM HELLO;
    TOKEN_PROGRAM, TOKEN_IDENTIFIER, TOKEN_SEMI_COLON,
    // CONST
    TOKEN_CONST,
    // K = '...';
    TOKEN_IDENTIFIER, TOKEN_OP_EQ, TOKEN_INTEGER_VALUE, TOKEN_SEMI_COLON,
    // BEGIN
    TOKEN_BEGIN,
    // WRITELN('...');
    // TOKEN_IDENTIFIER, TOKEN_LEFT_PARENTHESIS, TOKEN_STRING_VALUE, TOKEN_RIGHT_PARENTHESIS,
    // WRITELN(K);
    TOKEN_IDENTIFIER, TOKEN_LEFT_PARENTHESIS, TOKEN_IDENTIFIER, TOKEN_RIGHT_PARENTHESIS,
    // END.
    TOKEN_END, TOKEN_DOT};

void test(char *name, char *source, token_type_t *expected, int count)
{
    int index;

    printf("TEST LEXER: INIT %s\n", name);
    ps_lexer_init(lexer);
    ps_buffer_set_text(&lexer->buffer, source, strlen(source));
    ps_buffer_dump(&lexer->buffer, 0, BUFFER_MAX_LINES - 1);

    printf("TEST LEXER: LOOP ON %s\n", name);
    index = 0;
    ps_lexer_read_next_char(lexer);
    if (lexer->error != LEXER_ERROR_NONE)
    {
        printf("TEST LEXER: BEGIN ERROR %d\n",
               lexer->error);
        return;
    }
    do
    {
        printf("TEST LEXER: %02d/%02d BEGIN\n", index+1, count);
        if (!ps_lexer_read_next_token(lexer))
        {
            printf("TEST LEXER: %02d/%02d ERROR %d\n",
                   index+1, count,
                   lexer->error);
            break;
        }
        else if (lexer->current_token.type != expected[index])
        {
            printf("TEST LEXER: %02d/%02d ERROR EXPECTED TOKEN %4d, GOT %4d, %s\n",
                   index+1, count,
                   expected[index],
                   lexer->current_token.type,
                   lexer->current_token.type == TOKEN_IDENTIFIER ? lexer->current_token.value.identifier : lexer->current_token.value.i);
            break;
        }
        else
        {
            printf("TEST LEXER: %02d/%02d OK EXPECTED TOKEN %4d, GOT %4d, %s\n",
                   index+1, count,
                   expected[index],
                   lexer->current_token.type,
                   lexer->current_token.value.identifier);
        }
        printf("TEST LEXER: %02d/%02d END\n", index+1, count);
        index += 1;
    } while (index < count);
}

int main(void)
{
    printf("TEST LEXER: BEGIN\n");

    test("MINIMAL", minimal, token_types_minimal, sizeof(token_types_minimal) / sizeof(token_type_t));
    // test("HELLO", hello, expected_hello, sizeof(expected_hello) / sizeof(token_type_t));

    printf("TEST LEXER: END\n");
    return 0;
}
