#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "symbol_table.h"

/**
 * @brief Normalize symbol (=> UPPERCASE) in place (no string copy)
 *
 * @param name
 */
void symbol_normalize_name(char *name)
{
    while (*name)
    {
        /* a-z => A-Z */
        if (*name >= 'a' && *name <= 'z')
        {
            *name -= ('a' - 'A');
        }
        name++;
    }
}

void symbol_table_init(symbol_table_t *table)
{
    table->count = 0;
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++)
    {
        table->symbols[i] = NULL;
    }
}

void symbol_table_dump(symbol_table_t *table, char *title)
{
    symbol_t *s;
    fprintf(stderr, "*** Symbol table %s (%d) ***\n", title, table->count);
    fprintf(stderr, "#   name                            kind type value        value\n");
    fprintf(stderr, "--- ------------------------------- ---- ---- ------------ --------\n");
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++)
    {
        if (table->symbols[i] != NULL)
        {
            s = table->symbols[i];
            fprintf(stderr, "%03d %-*s %4d %4d %12d %08x\n", 
                i, MAX_SYMBOL_NAME, s->name, s->kind, s->type, s->value.i, s->value.i
            );
        }
    }
}

/**
 * @brief Search symbol
 *
 * @param name normalized
 * @return int index of symbol or -1 if not found
 */
int symbol_table_search(symbol_table_t *table, char *name)
{
    int index = 0;
    // symbol_normalize_name(name);
    while (index < table->count)
    {
        if (strcmp(name, table->symbols[index]->name) == 0)
        {
            return index;
        }
        index += 1;
    }
    return -1;
}

/**
 * @brief Get symbol
 *
 * @param name normalized
 * @return symbol or NULL if not found
 */
symbol_t *symbol_table_get(symbol_table_t *table, char *name)
{
    symbol_t *symbol = NULL;
    int index = symbol_table_search(table, name);
    if (index >= 0)
    {
        symbol = table->symbols[index];
    }
    return symbol;
}

/**
 * @brief Add symbol, returning an error
 *        if table is full
 *        or symbol already exists
 *
 * @param table
 * @param symbol
 * @return int index of added symbol (>=0) or error (<0)
 */
int symbol_table_add(symbol_table_t *table, symbol_t *symbol)
{
    if (table->count >= SYMBOL_TABLE_SIZE)
    {
        return SYMBOL_TABLE_ERROR_FULL;
    }
    int index = symbol_table_search(table, symbol->name);
    if (index >= 0)
    {
        return SYMBOL_TABLE_ERROR_EXISTS;
    }
    index = table->count;
    table->symbols[index] = symbol;
    table->count += 1;
    return index;
}

/* EOF */
