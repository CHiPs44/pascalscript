#include <stdio.h>
#include <string.h>
#include "symbol.h"

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

void symbol_dump(symbol_t *symbol)
{
    char *kind;
    char *type;
    char value[32];
    switch (symbol->kind)
    {
    case KIND_UNKNOWN:
        kind = "UNKNOWN ";
        break;
    case KIND_AUTO:
        kind = "AUTO    ";
        break;
    case KIND_FREE:
        kind = "FREE    ";
        break;
    case KIND_CONSTANT:
        kind = "CONSTANT";
        break;
    case KIND_VARIABLE:
        kind = "VARIABLE";
        break;
    default:
        kind = "????????";
        break;
    }
    switch (symbol->kind)
    {
    case TYPE_NONE:
        type = "NONE    ";
        snprintf(value, 31, "?");
        break;
    case TYPE_INTEGER:
        type = "INTEGER ";
        snprintf(value, 31, "%d / %08x", symbol->value.i);
        break;
    default:
        type = "????????";
        snprintf(value, 31, "?");
        break;
    }
    fprintf(stderr,
            "SYMBOL: name=%s, type=%d, kind=%s, size=%d, value=%s\n",
            symbol->name, kind, symbol->type, symbol->size, value);
}

/* EOF */
