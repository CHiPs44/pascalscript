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
    ps_value value;

    printf("TEST VALUE: BEGIN\n");
    value.type = PS_TYPE_NONE;
    value.size = 0;
    value.data.i = 0;
    ps_value_debug(&value);

    printf("TEST VALUE: INTEGER\n");
    ps_value_set_integer(&value, -1234567890);
    ps_value_debug(&value);

    return 0;

    printf("TEST VALUE: UNSIGNED\n");
    ps_value_set_unsigned(&value, 0xDEADBEEF);
    ps_value_debug(&value);

    printf("TEST VALUE: BOOLEAN\n");
    ps_value_set_boolean(&value, true);
    ps_value_debug(&value);
    ps_value_set_boolean(&value, false);
    ps_value_debug(&value);

    printf("TEST VALUE: CHAR\n");
    ps_value_set_char(&value, 'A');
    ps_value_debug(&value);

    printf("TEST VALUE: STRING\n");
    ps_value_set_string(&value, "This is a test.", 50);
    ps_value_debug(&value);

    printf("TEST VALUE: REAL\n");
    ps_value_set_real(&value, 3.141592653589793);
    ps_value_debug(&value);

    printf("TEST VALUE: POINTER\n");
    ps_value_set_pointer(&value, (void *)&value);
    ps_value_debug(&value);

    printf("TEST VALUE: END\n");
    return 0;
}
