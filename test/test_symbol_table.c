/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>

// #define SYMBOL_TABLE_SIZE 3
#include "../include/ps_symbol.h"
#include "../include/ps_symbol_table.h"

symbol_table_t table;
symbol_t constant1 = {.name = "CONSTANT1", .kind = KIND_CONSTANT, .type = TYPE_INTEGER, .size = sizeof(int), .value.i = 0x0000DEAD};
symbol_t variable2 = {.name = "VARIABLE2", .kind = KIND_VARIABLE, .type = TYPE_INTEGER, .size = sizeof(int), .value.i = 0x0000BEEF};
symbol_t auto_var3 = {.name = "AUTO_VAR3", .kind = KIND_AUTO, .type = TYPE_INTEGER, .size = sizeof(int), .value.i = 0x12345678};
symbol_t constant4 = {.name = "CONSTANT4", .kind = KIND_CONSTANT, .type = TYPE_INTEGER, .size = sizeof(int), .value.i = 0x87654321};

// Poor man's Makefile ;-)
#include "../src/ps_symbol.c"
#include "../src/ps_symbol_table.c"

int main(void)
{
    int result;

    printf("TEST SYMBOL TABLE: BEGIN\n");
    symbol_table_init(&table);
    printf("TEST SYMBOL TABLE: INIT OK\n");
    // Add constant1 & variable2 => 0 & 1
    result = symbol_table_add(&table, &constant1);
    printf("TEST SYMBOL TABLE: ADD CONSTANT1 %s %d\n", result == 0 ? "OK" : "KO", result);
    result = symbol_table_add(&table, &variable2);
    printf("TEST SYMBOL TABLE: ADD VARIABLE2 %s %d\n", result == 1 ? "OK" : "KO", result);
    symbol_table_dump(&table, "Test");
    printf("TEST SYMBOL TABLE: DUMP OK\n");
    // Re-add constant1 => EXISTS
    result = symbol_table_add(&table, &constant1);
    printf("TEST SYMBOL TABLE: RE-ADD CONSTANT1 %s %d\n", result == SYMBOL_TABLE_ERROR_EXISTS ? "OK" : "KO", result);
    // auto_var3 shoulfd fit => 2
    result = symbol_table_add(&table, &auto_var3);
    printf("TEST SYMBOL TABLE: ADD AUTO_VAR3 %s %d\n", result == 2 ? "OK" : "KO", result);
    // constant4 should not fit => FULL
    result = symbol_table_add(&table, &constant4);
    printf("TEST SYMBOL TABLE: ADD CONSTANT4 %s %d\n", result == SYMBOL_TABLE_ERROR_FULL ? "OK" : "KO", result);
    // This is the end
    symbol_table_dump(&table, "Test");
    printf("TEST SYMBOL TABLE: DUMP OK\n");
    printf("TEST SYMBOL TABLE: END\n");
    return 0;
}
