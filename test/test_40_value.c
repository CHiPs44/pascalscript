/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <string.h>
#include <sys/resource.h>

#include "../include/ps_config.h"
#include "../include/ps_environment.h"
#include "../include/ps_memory.h"
#include "../include/ps_system.h"
#include "../include/ps_value.h"

#include "../src/ps_memory.c"
#include "../src/ps_string.c"
#include "../src/ps_symbol.c"
#include "../src/ps_symbol_table.c"
#include "../src/ps_environment.c"
#include "../src/ps_system.c"
#include "../src/ps_type_definition.c"
#include "../src/ps_value.c"
#include "../src/ps_value_type.c"

int main(void)
{
    struct rlimit rl = {1024 * 1024 * 8, 1024 * 1024 * 8};
    setrlimit(RLIMIT_AS, &rl);

    ps_identifier system = "SYSTEM";
    ps_environment *environment = ps_environment_alloc(NULL, &system, PS_SYSTEM_SYMBOL_TABLE_SIZE);
    ps_system_init(environment);

    printf("TEST VALUE: BEGIN\n");
    ps_value *dummy = ps_value_alloc(ps_system_integer.value->type, (ps_value_data){.i = 0});
    ps_value_debug(stdout, "DUMMY=", dummy);
    dummy = ps_value_free(dummy);

    printf("TEST VALUE: INTEGER\n");
    ps_value *integer_value = ps_value_set_integer(NULL, -1234567890);
    ps_value_debug(stdout, "I=", integer_value);
    integer_value = ps_value_free(integer_value);

    printf("TEST VALUE: UNSIGNED\n");
    ps_value *unsigned_value = ps_value_set_unsigned(NULL, 0xDEADBEEF);
    ps_value_debug(stdout, "U=", unsigned_value);
    unsigned_value = ps_value_free(unsigned_value);

    printf("TEST VALUE: BOOLEAN\n");
    ps_value *boolean_true = ps_value_set_boolean(NULL, true);
    ps_value_debug(stdout, "B=", boolean_true);
    boolean_true = ps_value_free(boolean_true);
    ps_value *boolean_false = ps_value_set_boolean(NULL, false);
    ps_value_debug(stdout, "B=", boolean_false);
    boolean_false = ps_value_free(boolean_false);

    printf("TEST VALUE: CHAR\n");
    ps_value *char_value = ps_value_set_char(NULL, '@');
    ps_value_debug(stdout, "C=", char_value);
    char_value = ps_value_free(char_value);

    printf("TEST VALUE: REAL\n");
    ps_value *real_value = ps_value_set_real(NULL, 3.141592653589793115997963468544185161590576171875);
    ps_value_debug(stdout, "R=", real_value);
    real_value = ps_value_free(real_value);

    printf("TEST VALUE: END\n");

    environment = ps_environment_free(environment);
    return 0;
}
