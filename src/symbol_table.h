#ifndef SYMBOL_TABLE_h
#define SYMBOL_TABLE_h

#include <stdlib.h>

#ifndef MAX_SYMBOL_NAME
#define MAX_SYMBOL_NAME 31
#endif
#ifndef SYMBOL_TABLE_SIZE
#define SYMBOL_TABLE_SIZE 256
#endif
#define SYMBOL_TABLE_ERROR_FULL -1
#define SYMBOL_TABLE_ERROR_EXISTS -2

typedef enum _kind_t
{
    KIND_UNKNOWN = 0,
    KIND_CONSTANT,
    KIND_VARIABLE,
    // KIND_TYPE
    // KIND_PROCEDURE,
    // KIND_FUNCTION,
} kind_t;

typedef enum _type_t
{
    // for constants and variables
    TYPE_UNDEFINED = 0,
    TYPE_INTEGER,
    // TYPE_UNSIGNED_INTEGER,
    // TYPE_CHAR,
    // TYPE_STRING,
    // TYPE_REAL,
} type_t;

typedef union _value_t
{
    int i;
    // uint u;
    // char c;
    // char *s;
    // double r;
} value_t;

typedef struct _symbol_t
{
    char name[MAX_SYMBOL_NAME + 1];
    kind_t kind;
    type_t type;
    size_t size;
    value_t value;
} symbol_t;

typedef struct _symbol_table_t
{
    int count;
    symbol_t *symbols[SYMBOL_TABLE_SIZE];
} symbol_table_t;

extern void      symbol_normalize_name(char *name);
extern void      symbol_table_init(symbol_table_t *table);
extern int       symbol_table_search(symbol_table_t *table, char *name);
extern symbol_t *symbol_table_get(symbol_table_t *table, char *name);
extern int       symbol_table_add(symbol_table_t *table, symbol_t *symbol);

#endif

