/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>
#include <sys/resource.h>

#include "../include/ps_value.h"

#include "../src/ps_string.c"
#include "../src/ps_value.c"

int main(void)
{
    struct rlimit rl = {256 * 1024 * 12, 256 * 1024 * 12};
    setrlimit(RLIMIT_AS, &rl);

    ps_value *value;

    printf("TEST VALUE: BEGIN\n");
    value = calloc(1, sizeof(ps_value));
    ps_value_debug(value, "N=");

    printf("TEST VALUE: INTEGER\n");
    ps_value_set_integer(value, -1234567890);
    ps_value_debug(value, "I=");

    printf("TEST VALUE: UNSIGNED\n");
    ps_value_set_unsigned(value, 0xDEADBEEF);
    ps_value_debug(value, "U=");

    printf("TEST VALUE: BOOLEAN\n");
    ps_value_set_boolean(value, true);
    ps_value_debug(value, "B=");
    ps_value_set_boolean(value, false);
    ps_value_debug(value, "B=");

    printf("TEST VALUE: CHAR\n");
    ps_value_set_char(value, '@');
    ps_value_debug(value, "C=");

    printf("TEST VALUE: STRING\n");
    value->data.s = NULL;
    ps_value_set_string(value, "This is a test.", 50);
    ps_value_debug(value, "S=");
    ps_value_set_string(value, "The Quick Brown Fox Jumps Over The Lazy Dog.", 0);
    ps_value_debug(value, "S=");
    ps_value_set_string(value, "01234567890123456789012345678901234567890123456789", 0);
    ps_value_debug(value, "S=");

    printf("TEST VALUE: REAL\n");
    ps_value_set_real(value, 3.141592653589793115997963468544185161590576171875);
    //          displayed as 3.14159274101257324219 (it's a float)
    ps_value_debug(value, "R=");
    ps_value_set_real(value, FLT_MIN);
    ps_value_debug(value, "R=");
    ps_value_set_real(value, -FLT_MAX);
    ps_value_debug(value, "R=");

    printf("TEST VALUE: POINTER\n");
    ps_value_set_pointer(value, (void *)0x55AA55AA);
    ps_value_debug(value, "P=");

    free(value);

    printf("TEST VALUE: END\n");
    return 0;
}
