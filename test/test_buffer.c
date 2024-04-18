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

buffer_t _buffer;
buffer_t *buffer = &_buffer;

char *empty = "";

char *minimal =
    "PROGRAM MINIMAL;\n"
    "BEGIN\n"
    "END.\n";

int main(void)
{
    printf("TEST BUFFER: BEGIN\n");

    printf("TEST BUFFER: INIT\n");
    buffer_init(buffer);
    printf("TEST BUFFER: LIST\n");
    buffer_list_text(buffer, 0, BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: SET TEXT EMPTY\n");
    buffer_set_text(buffer, empty, strlen(empty));
    printf("TEST BUFFER: LIST\n");
    buffer_list_text(buffer, 0, BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: SET TEXT MINIMAL\n");
    buffer_set_text(buffer, minimal, strlen(minimal));
    printf("TEST BUFFER: LIST\n");
    buffer_list_text(buffer, 0, BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: LOAD FILE\n");
    buffer_init(buffer);
    buffer_load_file(buffer, "../examples/00-hello.pas");
    printf("TEST BUFFER: LIST\n");
    buffer_list_text(buffer, 0, BUFFER_MAX_LINES - 1);

    printf("TEST BUFFER: END\n");
    return 0;
}
