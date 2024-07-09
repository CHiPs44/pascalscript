/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "../include/ps_error.h"
#include "../include/ps_readall.h"
#include "../include/ps_buffer.h"

#include "../src/ps_error.c"
#include "../src/ps_readall.c"
#include "../src/ps_buffer.c"

ps_buffer _buffer;
ps_buffer *buffer = &_buffer;

char *minimal_source =
    "PROGRAM MINIMAL;\n"
    "\t(* This program does nothing. *)\n"
    "BEGIN\n"
    "\t{ NOP! }\n"
    "END.\n";

char *hello_utf8 =
    //  |         1         2         3         4         5         6         7         8|
    //  |12345678901234567890123456789012345678901234567890123456789012345678901234567890|
    "Program Hello;\n"
    "Const\n"
    "\tK = 'Pépé le putois a 1\u00a0234,56€ en espèces sonnantes et trébuchantes.';\n"
    "Begin\n"
    "\tWriteLn('Hello, World!');\n"
    "\tWriteLn('k=', k);\n"
    "End."; //\n";

int main(void)
{
    printf("TEST BUFFER: BEGIN\n");

    printf("TEST BUFFER: INIT\n");
    ps_buffer_init(buffer);
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: SET TEXT EMPTY\n");
    ps_buffer_set_text(buffer, "", 0);
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: SET TEXT MINIMAL\n");
    ps_buffer_set_text(buffer, minimal_source, strlen(minimal_source));
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(buffer, 0, PS_BUFFER_MAX_LINES - 1);
    buffer->debug = 2;
    int count = 0;
    while (ps_buffer_read_next_char(buffer))
    {
        count += 1;
    }
    printf(" => count=%d, strlen=%d\n", count, strlen(minimal_source));
    buffer->debug = false;

    printf("TEST BUFFER: SET TEXT HELLO\n");
    ps_buffer_set_text(buffer, hello_utf8, strlen(hello_utf8));
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: LOAD FILE\n");
    ps_buffer_init(buffer);
    ps_buffer_load_file(buffer, "../examples/00-hello.pas");
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: END\n");
    return 0;
}
