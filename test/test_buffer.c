/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>
#include <sys/resource.h>

#include "../include/ps_error.h"
#include "../include/ps_readall.h"
#include "../include/ps_buffer.h"

#include "../src/ps_error.c"
#include "../src/ps_readall.c"
#include "../src/ps_buffer.c"

ps_buffer *buffer;

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
    "\tWriteLn('K=', K);\n"
    "End."; //\n";

int main(void)
{
    struct rlimit rl = {256 * 1024 * 12, 256 * 1024 * 12};
    setrlimit(RLIMIT_AS, &rl);

    printf("TEST BUFFER: BEGIN\n");

    printf("TEST BUFFER: INIT\n");
    buffer = ps_buffer_init();
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: SET TEXT EMPTY\n");
    ps_buffer_load_string(buffer, "", 0);
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: SET TEXT MINIMAL\n");
    ps_buffer_load_string(buffer, minimal_source, strlen(minimal_source));
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(buffer, 0, PS_BUFFER_MAX_LINES - 1);
    buffer->debug = 0;
    int count = 0;
    while (ps_buffer_read_next_char(buffer))
    {
        count += 1;
    }
    printf(" => count=%d, strlen=%d\n", count, strlen(minimal_source));
    buffer->debug = 0;

    printf("TEST BUFFER: SET TEXT HELLO\n");
    ps_buffer_load_string(buffer, hello_utf8, strlen(hello_utf8));
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: LOAD FILE\n");
    ps_buffer_init(buffer);
    ps_buffer_load_file(buffer, "../examples/00-minimal.pas");
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: END\n");
    ps_buffer_done(buffer);
    return 0;
}
