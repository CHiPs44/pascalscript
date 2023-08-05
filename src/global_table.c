#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "symbol_table.h"

symbol_table_t global_table;

symbol_t default_globals[] = {
    { "_PS_VERSION_", KIND_CONSTANT, TYPE_INTEGER, sizeof(int), 0x00000001L       }, // 0.0.0.1
    { "_PS_DATE_"   , KIND_CONSTANT, TYPE_INTEGER, sizeof(int), 0x20230804L       }, // 4-aug-23
    { "MAXINT"      , KIND_CONSTANT, TYPE_INTEGER, sizeof(int),  2147483647L      },
    // { "PI", KIND_CONSTANT, TYPE_REAL, sizeof(double), 3.141592653589793 },
};

/**
 * @brief Initialize global symbols table
 * 
 */
void global_table_init()
{
    // global_table.count = 0;
    symbol_table_init(&global_table);
    for (int i = 0; i < sizeof(default_globals) / sizeof(default_globals[0]); i += 1)
    {
        global_table_add(&default_globals[i]);
    }
}

int global_table_get(char *name)
{
    return symbol_table_get(&global_table, name);
}

int global_table_add(symbol_t *symbol)
{
    return symbol_table_add(&global_table, symbol);
}

/* EOF */
