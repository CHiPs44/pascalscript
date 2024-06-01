/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>

#include "../include/ps_value.h"

#include "../src/ps_value.c"
int main(void)
{
    ps_value_t value;

    printf("TEST VALUE: BEGIN\n");
    value.type = PS_TYPE_NONE;
    value.size = 0;
    value.data.i = 0;
    value_dump(&value);

    printf("TEST VALUE: INTEGER\n");
    ps_value_integer(&value, -1234567890);
    value_dump(&value);

    printf("TEST VALUE: UNSIGNED\n");
    ps_value_unsigned(&value, 0xDEADBEEF);
    value_dump(&value);

    printf("TEST VALUE: BOOLEAN\n");
    ps_value_boolean(&value, true);
    value_dump(&value);
    ps_value_boolean(&value, false);
    value_dump(&value);

    printf("TEST VALUE: CHAR\n");
    ps_value_char(&value, 'A');
    value_dump(&value);

    printf("TEST VALUE: STRING\n");
    ps_value_string(&value, "This is a test.");
    value_dump(&value);

    printf("TEST VALUE: REAL\n");
    ps_value_real(&value, 3.141592653589793);
    value_dump(&value);

    printf("TEST VALUE: POINTER\n");
    ps_value_pointer(&value, (void *)&value);
    value_dump(&value);

    printf("TEST VALUE: END\n");
    return 0;
}
