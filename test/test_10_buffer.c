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
    ps_buffer *buffer = ps_buffer_alloc();
    if (buffer == NULL)
    {
        printf(" => ERROR: could not allocate buffer!\n");
        goto failure;
    }

    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);
    printf("\n");

    printf("TEST BUFFER: SET TEXT EMPTY\n");
    if (!ps_buffer_load_string(buffer, "", 0))
    {
        printf(" => error=%d\n", buffer->error);
        goto failure;
    }
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);
    printf("\n");

    printf("TEST BUFFER: SET TEXT MINIMAL\n");
    int minimal_length = strlen(minimal_source);
    if (!ps_buffer_load_string(buffer, minimal_source, minimal_length))
    {
        printf(" => error=%d\n", buffer->error);
        goto failure;
    }
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);
    int count = 0;
    while (ps_buffer_read_next_char(buffer))
    {
        count += 1;
    }
    printf("TEST BUFFER: count=%d, length=%d\n", count, minimal_length);
    if (count != minimal_length)
    {
        printf(" => ERROR: count does not match length!\n");
        goto failure;
    }

    printf("TEST BUFFER: SET TEXT HELLO\n");
    if (!ps_buffer_load_string(buffer, hello_utf8, strlen(hello_utf8)))
    {
        printf(" => error=%d\n", buffer->error);
        goto failure;
    }
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);
    printf("\n");

    printf("TEST BUFFER: LOAD FILE\n");
    ps_buffer_reset(buffer);
    static char *filename = "./test/test_10_buffer.pas";
    if (!ps_buffer_load_file(buffer, filename))
    {
        printf("%s => error=%d, file_errno=%d\n", filename, buffer->error, buffer->file_errno);
        goto failure;
    }
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);
    printf("\n");

    printf("TEST BUFFER: END\n");
    ps_buffer_free(buffer);
    return EXIT_SUCCESS;

failure:
    printf("TEST BUFFER: FAILURE\n");
    if (buffer != NULL)
        buffer = ps_buffer_free(buffer);
    return EXIT_FAILURE;
}
