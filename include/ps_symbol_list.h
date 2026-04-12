/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SYMBOL_LIST_H
#define _PS_SYMBOL_LIST_H

#ifdef __cplusplus
extern "C"
{
#endif

typedef struct s_ps_symbol_list
{
    ps_unsigned size;
    ps_unsigned more;
    ps_unsigned used;
    ps_symbol **values;
} ps_symbol_list;

ps_symbol_list *ps_symbol_list_alloc(int size, int more);
void ps_symbol_list_free(ps_symbol_list *list, bool free_symbols);
bool ps_symbol_list_grow(ps_symbol_list *list);
bool ps_symbol_list_find(const ps_symbol_list *list, const char *name);
ps_symbol *ps_symbol_list_add(ps_symbol_list *list, ps_symbol *type_symbol, const char *name);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYMBOL_LIST_H */
