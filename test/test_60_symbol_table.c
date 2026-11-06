/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_ast.h"
#include "ps_frame_stack.h"
#include "ps_system.h"
#include "ps_value.h"

ps_symbol constant1 = {.name = "CONSTANT1", .kind = PS_SYMBOL_KIND_CONSTANT, .allocated = false, .system = false};
ps_symbol variable2 = {.name = "VARIABLE2", .kind = PS_SYMBOL_KIND_VARIABLE, .allocated = false, .system = false};
ps_symbol auto_var3 = {.name = "AUTO_VAR3", .kind = PS_SYMBOL_KIND_AUTO, .allocated = false, .system = false};
ps_symbol constant4 = {.name = "CONSTANT4", .kind = PS_SYMBOL_KIND_CONSTANT, .allocated = false, .system = false};

int main(void)
{
    printf("TEST SYMBOL TABLE: BEGIN\n");
    ps_ast_block *system = ps_system_alloc();

    ps_value value1 = {.type = ps_system_integer.value->type, .allocated = false, .data.i = 0x55aa55aa};
    constant1.value = &value1;
    ps_value value2 = {.type = ps_system_unsigned.value->type, .allocated = false, .data.u = 0x55aa55aa};
    variable2.value = &value2;
    ps_value value3 = {.type = ps_system_integer.value->type, .allocated = false, .data.i = 0x55aa55aa};
    auto_var3.value = &value3;
    ps_value value4 = {.type = ps_system_integer.value->type, .allocated = false, .data.i = 0x55aa55aa};
    constant4.value = &value4;

    ps_symbol_table *table = system->symbols;
    int result;
    ps_error error = PS_ERROR_NONE;

    ps_value_set_integer(constant1.value, 1234567890);
    ps_value_set_unsigned(variable2.value, 0xDEADBEEF);
    ps_value_set_integer(auto_var3.value, 0x12345678);
    ps_value_set_integer(constant4.value, 0x87654321);

    printf("TEST SYMBOL TABLE: BEGIN\n");
    printf("TEST SYMBOL TABLE: INIT OK\n");
    // Add constant1 & variable2 => 0 & 1
    result = ps_symbol_table_add(table, &constant1);
    printf("TEST SYMBOL TABLE: ADD CONSTANT1 %s %d\n", result == 0 ? "OK" : "FAILED!", result);
    result = ps_symbol_table_add(table, &variable2);
    printf("TEST SYMBOL TABLE: ADD VARIABLE2 %s %d\n", result == 1 ? "OK" : "FAILED!", result);
    ps_symbol_table_dump(stdout, "Test", table);
    printf("TEST SYMBOL TABLE: DUMP OK\n");
    // Re-add constant1 => EXISTS
    error = ps_symbol_table_add(table, &constant1);
    printf("TEST SYMBOL TABLE: RE-ADD CONSTANT1 %s %d\n", error == PS_ERROR_SYMBOL_EXISTS ? "OK" : "FAILED!", error);
    // auto_var3 shoulfd fit => 2
    error = ps_symbol_table_add(table, &auto_var3);
    printf("TEST SYMBOL TABLE: ADD AUTO_VAR3 %s %d\n", error == PS_ERROR_NONE ? "OK" : "FAILED!", error);
    // constant4 should not fit => FULL
    error = ps_symbol_table_add(table, &constant4);
    printf("TEST SYMBOL TABLE: ADD CONSTANT4 %s %d\n", error == PS_ERROR_SYMBOL_TABLE_FULL ? "OK" : "FAILED!", error);
    // This is the end
    ps_symbol_table_dump(stdout, "Test", table);
    printf("TEST SYMBOL TABLE: DUMP OK\n");
    printf("TEST SYMBOL TABLE: END\n");

    ps_system_free(system);

    printf("TEST SYMBOL TABLE: END\n");
    return EXIT_SUCCESS;
}
