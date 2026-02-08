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

#include "../src/ps_environment.c"
#include "../src/ps_memory.c"
#include "../src/ps_string.c"
#include "../src/ps_symbol.c"
#include "../src/ps_symbol_table.c"
#include "../src/ps_system.c"
#include "../src/ps_type_definition.c"
#include "../src/ps_value.c"
#include "../src/ps_value_type.c"

ps_symbol_table table;
ps_symbol constant1 = {.name = "CONSTANT1", .kind = PS_SYMBOL_KIND_CONSTANT, .allocated = false, .system = false};
ps_symbol variable2 = {.name = "VARIABLE2", .kind = PS_SYMBOL_KIND_VARIABLE};
ps_symbol auto_var3 = {.name = "AUTO_VAR3", .kind = PS_SYMBOL_KIND_AUTO};
ps_symbol constant4 = {.name = "CONSTANT4", .kind = PS_SYMBOL_KIND_CONSTANT};

int main(void)
{
    struct rlimit rl = {1024 * 1024 * 8, 1024 * 1024 * 8};
    setrlimit(RLIMIT_AS, &rl);

    ps_identifier system = "SYSTEM";
    ps_environment *environment = ps_environment_alloc(NULL, &system, PS_SYSTEM_SYMBOL_TABLE_SIZE);
    ps_system_init(environment);

    ps_value value1 = {.type = &ps_system_integer.value->type, .allocated = false, .data.i = 0x55aa55aa55aa};
    constant1.value = &value1;
    ps_value value2 = {.type = &ps_system_unsigned.value->type, .allocated = false, .data.u = 0x55aa55aa55aa};
    variable2.value = &value2;
    ps_value value3 = {.type = &ps_system_integer.value->type, .allocated = false, .data.i = 0x55aa55aa55aa};
    auto_var3.value = &value3;
    ps_value value4 = {.type = &ps_system_integer.value->type, .allocated = false, .data.i = 0x55aa55aa55aa};
    constant4.value = &value4;

    int result;
    ps_symbol *symbol;

    ps_value_set_integer(&constant1.value, 1234567890);
    ps_value_set_unsigned(&variable2.value, 0xDEADBEEF);
    ps_value_set_integer(&auto_var3.value, 0x12345678);
    ps_value_set_integer(&constant4.value, 0x87654321);

    printf("TEST SYMBOL TABLE: BEGIN\n");
    ps_symbol_table_alloc(&table);
    printf("TEST SYMBOL TABLE: INIT OK\n");
    // Add constant1 & variable2 => 0 & 1
    result = ps_symbol_table_add(&table, &constant1);
    printf("TEST SYMBOL TABLE: ADD CONSTANT1 %s %d\n", result == 0 ? "OK" : "KO", result);
    result = ps_symbol_table_add(&table, &variable2);
    printf("TEST SYMBOL TABLE: ADD VARIABLE2 %s %d\n", result == 1 ? "OK" : "KO", result);
    ps_symbol_table_dump(stdout, "Test", &table);
    printf("TEST SYMBOL TABLE: DUMP OK\n");
    // Re-add constant1 => EXISTS
    symbol = ps_symbol_table_add(&table, &constant1);
    printf("TEST SYMBOL TABLE: RE-ADD CONSTANT1 %s %d\n", symbol == NULL ? "OK" : "KO", result);
    // auto_var3 shoulfd fit => 2
    result = ps_symbol_table_add(&table, &auto_var3);
    printf("TEST SYMBOL TABLE: ADD AUTO_VAR3 %s %d\n", result == 2 ? "OK" : "KO", result);
    // constant4 should not fit => FULL
    result = ps_symbol_table_add(&table, &constant4);
    printf("TEST SYMBOL TABLE: ADD CONSTANT4 %s %d\n", result == PS_SYMBOL_TABLE_ERROR_FULL ? "OK" : "KO", result);
    // This is the end
    ps_symbol_table_dump(stdout, "Test", &table);
    printf("TEST SYMBOL TABLE: DUMP OK\n");
    printf("TEST SYMBOL TABLE: END\n");

    environment = ps_environment_free(environment);

    return 0;
}
