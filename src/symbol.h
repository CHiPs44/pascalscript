#ifndef _SYMBOL_H
#define _SYMBOL_H

#include <stdlib.h>

#ifndef MAX_SYMBOL_NAME
#define MAX_SYMBOL_NAME 31
#endif

typedef enum _kind_t
{
    KIND_UNKNOWN = 0,
    KIND_AUTO,
    KIND_FREE,
    KIND_CONSTANT,
    KIND_VARIABLE,
    // KIND_PROCEDURE,
    // KIND_FUNCTION,
    // KIND_TYPE,
} kind_t;

typedef enum _type_t
{
    // for constants and variables
    TYPE_NONE = 0,
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

extern void symbol_normalize_name(char *name);

#endif

