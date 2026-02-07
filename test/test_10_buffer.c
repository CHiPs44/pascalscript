/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>
#include <sys/resource.h>

#include "../include/ps_buffer.h"
#include "../include/ps_error.h"
#include "../include/ps_memory.h"
#include "../include/ps_readall.h"

#include "../src/ps_buffer.c"
#include "../src/ps_error.c"
#include "../src/ps_memory.c"
#include "../src/ps_readall.c"

ps_buffer *buffer;

char *minimal_source = "PROGRAM MINIMAL;\n"
                       "    (* This program does nothing. *)\n"
                       "BEGIN\n"
                       "    { NOP! }\n"
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
    struct rlimit rl = {1024 * 1024 * 8, 1024 * 1024 * 8};
    setrlimit(RLIMIT_AS, &rl);

    printf("TEST BUFFER: BEGIN\n");

    printf("TEST BUFFER: INIT\n");
    buffer = ps_buffer_alloc();
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: SET TEXT EMPTY\n");
    ps_buffer_load_string(buffer, "", 0);
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: SET TEXT MINIMAL\n");
    ps_buffer_load_string(buffer, minimal_source, strlen(minimal_source));
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);
    int count = 0;
    while (ps_buffer_read_next_char(buffer))
    {
        count += 1;
    }
    printf(" => count=%d, length=%d\n", count, strlen(minimal_source));

    printf("TEST BUFFER: SET TEXT HELLO\n");
    ps_buffer_load_string(buffer, hello_utf8, strlen(hello_utf8));
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: LOAD FILE\n");
    ps_buffer_alloc(buffer);
    ps_buffer_load_file(buffer, "../examples/00-minimal.pas");
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: END\n");
    ps_buffer_free(buffer);
    return 0;
}
