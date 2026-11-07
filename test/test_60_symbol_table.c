/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "ps_symbol_table.h"

int main(void)
{
    printf("TEST SYMBOL TABLE: BEGIN\n");
    ps_symbol_table *table = ps_symbol_table_alloc(1, 4);
    if (table == NULL)
    {
        printf("TEST SYMBOL TABLE: ERROR allocating table\n");
        return EXIT_FAILURE;
    }

    ps_symbol constant1 = {.name = "CONSTANT1", .kind = PS_SYMBOL_KIND_CONSTANT};
    ps_symbol variable2 = {.name = "VARIABLE2", .kind = PS_SYMBOL_KIND_VARIABLE};
    ps_symbol auto_var3 = {.name = "AUTO_VAR3", .kind = PS_SYMBOL_KIND_AUTO};
    ps_symbol constant4 = {.name = "CONSTANT4", .kind = PS_SYMBOL_KIND_CONSTANT};

    bool ok = true;
    ps_error error = ps_symbol_table_add(table, &constant1);
    ok = error == PS_ERROR_NONE;
    printf("TEST SYMBOL TABLE: ADD CONSTANT1 %s %d\n", ok ? "OK" : "FAILED!", error);

    error = ps_symbol_table_add(table, &variable2);
    bool variable_added = error == PS_ERROR_NONE;
    ok = ok && variable_added;
    printf("TEST SYMBOL TABLE: ADD VARIABLE2 %s %d\n", variable_added ? "OK" : "FAILED!", error);

    error = ps_symbol_table_add(table, &constant4);
    bool constant_added = error == PS_ERROR_NONE;
    ok = ok && constant_added;
    printf("TEST SYMBOL TABLE: ADD CONSTANT4 %s %d\n", constant_added ? "OK" : "FAILED!", error);

    error = ps_symbol_table_add(table, &auto_var3);
    bool auto_rejected = error == PS_ERROR_SYMBOL_TABLE_INVALID;
    ok = ok && auto_rejected;
    printf("TEST SYMBOL TABLE: REJECT AUTO_VAR3 %s %d\n", auto_rejected ? "OK" : "FAILED!", error);

    bool symbols_found = ps_symbol_table_find(table, constant1.name) == &constant1 &&
                         ps_symbol_table_find(table, variable2.name) == &variable2 &&
                         ps_symbol_table_find(table, constant4.name) == &constant4 &&
                         ps_symbol_table_find(table, auto_var3.name) == NULL && table->used_buckets == 1 &&
                         table->buckets[0]->used == 3 && table->buckets[0]->size == 4;
    ok = ok && symbols_found;
    printf("TEST SYMBOL TABLE: FIND AND GROW %s\n", symbols_found ? "OK" : "FAILED!");

    ps_symbol_table_free(table);
    printf("TEST SYMBOL TABLE: %s\n", ok ? "END" : "FAILED!");
    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
