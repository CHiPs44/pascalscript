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
    "PROGRAM MINIMAL;\n"
    // "{ Comment made with curly brackets }\n"
    "BEGIN\n"
    "END.\n";

char *hello =
    //  |         1         2         3         4         5         6         7         8|
    //  |12345678901234567890123456789012345678901234567890123456789012345678901234567890|
    "Program Hello;\n"
    "Const\n"
    "  K = 'The Quick Brown Fox Jumps Over The Lazy Dog. 0123456789 Times!';\n"
    "Begin\n"
    "  WriteLn('Hello, World!');\n"
    "  WriteLn('K=', k);\n"
    "End.\n";

int main(void)
{
    token_type_t token_types_minimal[] = {TOKEN_PROGRAM, TOKEN_SEMI_COLON, TOKEN_BEGIN, TOKEN_END, TOKEN_DOT};
    int index;
    error_t error;

    printf("TEST LEXER: BEGIN\n");

    printf("TEST LEXER: INIT\n");
    lexer_init(lexer);
    buffer_set_text(&lexer->buffer, minimal, strlen(minimal));
    buffer_dump(&lexer->buffer, 0, BUFFER_MAX_LINES - 1);

    printf("TEST LEXER: LOOP ON MINIMAL\n");
    index = 0;
    do
    {
        error = lexer_read_token(lexer);
        if (error != LEXER_ERROR_NONE)
        {
            printf("TEST LEXER: ERROR %d\n", error);
            break;
        }
        else if (lexer->current_token.type != token_types_minimal[index])
        {
            printf("TEST LEXER: ERROR EXPECTED TOKEN %d, GOT %d\n", token_types_minimal[index], lexer->current_token.type);
            break;
        }
        index += 1;
    } while (index < sizeof(token_types_minimal) / sizeof(token_type_t));

    printf("TEST LEXER: END\n");
    return 0;
}
