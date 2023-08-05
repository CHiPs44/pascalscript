#include <stdio.h>

#define SYMBOL_TABLE_SIZE 3
#include "../src/global_table.h"
#include "../src/global_table.c"
#include "../src/symbol_table.c"

int main(void)
{
    symbol_t *result;
    // constant1.value.i = 0x0000DEAD;
    // variable2.value.i = 0x0000BEEF;
    // constant3.value.i = 0x12345678;
    // constant4.value.i = 0x12345678;
    printf("TEST GLOBAL TABLE: BEGIN\n");
    global_table_init();
    printf("TEST GLOBAL TABLE: INIT OK\n");
    symbol_table_dump(&global_table, "Global");
    printf("TEST GLOBAL TABLE: DUMP OK\n");
    result = global_table_get("MAXINT");
    printf("TEST GLOBAL TABLE: GET MAXINT %s %p\n", result != NULL ? "OK" : "KO", result);
    result = global_table_get("FOOBARBAZ");
    printf("TEST GLOBAL TABLE: GET FOOBARBAZ %s %p\n", result == NULL ? "OK" : "KO", result);
    printf("TEST GLOBAL TABLE: END\n");
    return 0;
}
