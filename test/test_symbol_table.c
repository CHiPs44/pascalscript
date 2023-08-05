#include <stdio.h>

#define SYMBOL_TABLE_SIZE 3
#include "../src/symbol_table.h"
#include "../src/symbol_table.c"

symbol_table_t table;
symbol_t constant1 = {"CONSTANT1", KIND_CONSTANT, TYPE_INTEGER};
symbol_t variable2 = {"VARIABLE2", KIND_VARIABLE, TYPE_INTEGER};
symbol_t constant3 = {"CONSTANT3", KIND_CONSTANT, TYPE_INTEGER};
symbol_t constant4 = {"CONSTANT4", KIND_CONSTANT, TYPE_INTEGER};

int main(void)
{
    int result;
    constant1.value.i = 0x0000DEAD;
    variable2.value.i = 0x0000BEEF;
    constant3.value.i = 0x12345678;
    constant4.value.i = 0x12345678;
    printf("TEST SYMBOL TABLE: BEGIN\n");
    symbol_table_init(&table);
    printf("TEST SYMBOL TABLE: INIT OK\n");
    result = symbol_table_add(&table, &constant1);
    printf("TEST SYMBOL TABLE: ADD CONSTANT1 %s %d\n", result == 0 ? "OK" : "KO", result);
    result = symbol_table_add(&table, &variable2);
    printf("TEST SYMBOL TABLE: ADD VARIABLE2 %s %d\n", result == 1 ? "OK" : "KO", result);
    symbol_table_dump(&table, "Test");
    printf("TEST SYMBOL TABLE: DUMP OK\n");
    result = symbol_table_add(&table, &constant1);
    printf("TEST SYMBOL TABLE: ADD CONSTANT1 #2 %s %d\n", result == SYMBOL_TABLE_ERROR_EXISTS ? "OK" : "KO", result);
    result = symbol_table_add(&table, &constant3);
    printf("TEST SYMBOL TABLE: ADD CONSTANT3 %s %d\n", result == SYMBOL_TABLE_ERROR_FULL ? "KO" : "OK", result);
    result = symbol_table_add(&table, &constant4);
    printf("TEST SYMBOL TABLE: ADD CONSTANT4 %s %d\n", result == SYMBOL_TABLE_ERROR_FULL ? "OK" : "KO", result);
    symbol_table_dump(&table, "Test");
    printf("TEST SYMBOL TABLE: DUMP OK\n");
    printf("TEST SYMBOL TABLE: END\n");
    return 0;
}
