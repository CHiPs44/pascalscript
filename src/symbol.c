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

/* EOF */
