#ifndef _SYMBOL_TABLE_H
#define _SYMBOL_TABLE_H

#include "symbol.h"

#ifndef SYMBOL_TABLE_SIZE
#define SYMBOL_TABLE_SIZE (256)
#endif

#define SYMBOL_TABLE_ERROR_FULL (-1)
#define SYMBOL_TABLE_ERROR_EXISTS (-2)

typedef struct _symbol_table_t
{
    int count;
    symbol_t symbols[SYMBOL_TABLE_SIZE];
} symbol_table_t;

extern void      symbol_table_init(symbol_table_t *table);
extern int       symbol_table_find(symbol_table_t *table, char *name);
extern symbol_t *symbol_table_get (symbol_table_t *table, char *name);
extern int       symbol_table_add (symbol_table_t *table, symbol_t *symbol);
extern int       symbol_table_del (symbol_table_t *table, char *name);
extern int       symbol_table_free(symbol_table_t *table, char *name);
extern int       symbol_table_gc  (symbol_table_t *table);

#endif
