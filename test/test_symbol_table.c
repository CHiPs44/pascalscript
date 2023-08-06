#include <stdio.h>

#include "../src/symbol.h"
#define SYMBOL_TABLE_SIZE 3
#include "../src/symbol_table.h"

symbol_table_t table;
symbol_t constant1 = {"CONSTANT1", KIND_CONSTANT, TYPE_INTEGER, sizeof(int), 0x0000DEAD};
symbol_t variable2 = {"VARIABLE2", KIND_VARIABLE, TYPE_INTEGER, sizeof(int), 0x0000BEEF};
symbol_t auto_var3 = {"AUTO_VAR3", KIND_AUTO    , TYPE_INTEGER, sizeof(int), 0x12345678};
symbol_t constant4 = {"CONSTANT4", KIND_CONSTANT, TYPE_INTEGER, sizeof(int), 0x12345678};

// Poor man's Makefile ;-)
#include "../src/symbol_table.c"
#include "../src/symbol.c"

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
