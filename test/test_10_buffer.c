/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "ps_buffer.h"
#include "ps_error.h"
#include "ps_memory.h"
#include "ps_readall.h"
#include "ps_tools.h"
#include <stdlib.h>

char minimal_source[] = "PROGRAM MINIMAL;\n"
                        "    (* This program does nothing. *)\n"
                        "BEGIN\n"
                        "    { NOP! }\n"
                        "END.\n";

char *hello_utf8 = "Program Hello;\n"
                   "Const\n"
                   "\tK = 'Pépé le putois a 1\u00a0234,56€ en espèces sonnantes et trébuchantes.';\n"
                   "Begin\n"
                   "\tWriteLn('Hello, World!');\n"
                   "\tWriteLn('K=', K);\n"
                   "End.\n";

int main(void)
{
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
    size_t minimal_length = sizeof(minimal_source) - 1;
    printf("TEST BUFFER: minimal_length=%zu\n", minimal_length);
    if (!ps_buffer_load_string(buffer, minimal_source, minimal_length))
    {
        printf(" => error=%d\n", buffer->error);
        goto failure;
    }
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);
    size_t count = 0;
    while (ps_buffer_read_next_char(buffer))
    {
        count += 1;
    }
    printf("TEST BUFFER: count=%zu, length=%zu\n", count, minimal_length);
    if (count != minimal_length)
    {
        printf(" => ERROR: count does not match length!\n");
        goto failure;
    }

    printf("TEST BUFFER: SET TEXT HELLO\n");
    if (!ps_buffer_load_string(buffer, hello_utf8, sizeof(hello_utf8) - 1))
    {
        printf(" => error=%d\n", buffer->error);
        goto failure;
    }
    printf("TEST BUFFER: DUMP\n");
    ps_buffer_dump(stdout, buffer, 0, PS_BUFFER_MAX_LINES - 1);
    printf("\n");

    printf("TEST BUFFER: LOAD FILE\n");
    ps_buffer_reset(buffer);
    char *filename = "/tmp/test_10_buffer.pas"; // NOSONAR
    FILE *f = fopen(filename, "w");
    fprintf(f, "Program Test10Buffer;\n"
               "Begin\n"
               " { 1234567890123456789012345678901234567890123456789012345678901234567890 }\n"
               "End.\n");
    fclose(f);
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
    if (buffer != NULL)
        ps_buffer_free(buffer);
    printf("TEST BUFFER: FAILED!\n");
    return EXIT_FAILURE;
}
