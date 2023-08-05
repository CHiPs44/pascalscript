#include <stdio.h>

#define SYMBOL_TABLE_SIZE 3
#include "../src/symbol_table.h"
#include "../src/symbol_table.c"

symbol_table_t table;
symbol_t constant1 = {"CONSTANT1", KIND_CONSTANT, TYPE_INTEGER}; //, {i = 0x0000DEAD}};
symbol_t variable2 = {"VARIABLE2", KIND_VARIABLE, TYPE_INTEGER}; //, {i = 0x0000BEEF}};
symbol_t constant3 = {"CONSTANT3", KIND_CONSTANT, TYPE_INTEGER}; //, {i = 0x12345678}};

int main(void)
{
    int result;
    constant1.value.i = 0x0000DEAD;
    variable2.value.i = 0x0000BEEF;
    printf("TEST SYMBOL TABLE: BEGIN\n");
    symbol_table_init(&table);
    printf("TEST SYMBOL TABLE: INIT OK\n");
    result = symbol_table_add(&table, &constant1);
    printf("TEST SYMBOL TABLE: ADD CONSTANT1 %s\n", result == 0 ? "OK" : "KO");
    result = symbol_table_add(&table, &variable2);
    printf("TEST SYMBOL TABLE: ADD VARIABLE2 %s\n", result == 1 ? "OK" : "KO");
    symbol_table_dump(&table, "Test");
    printf("TEST SYMBOL TABLE: DUMP OK\n");
    result = symbol_table_add(&table, &constant1);
    printf("TEST SYMBOL TABLE: ADD CONSTANT1 #2 %s %d\n", result == SYMBOL_TABLE_ERROR_EXISTS ? "OK" : "KO", result);
    result = symbol_table_add(&table, &constant3);
    printf("TEST SYMBOL TABLE: ADD CONSTANT3 %s %d\n", result == SYMBOL_TABLE_ERROR_FULL ? "OK" : "KO", result);
    symbol_table_dump(&table, "Test");
    printf("TEST SYMBOL TABLE: DUMP OK\n");
    printf("TEST SYMBOL TABLE: END\n");
    return 0;
}