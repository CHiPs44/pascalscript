/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_STRING_H
#define _PS_STRING_H

#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    ps_string *ps_string_new(ps_string_len max);
    void ps_string_free(ps_string *str);
    bool ps_string_set(ps_string *ptr, ps_char *zstr);
    // bool ps_string_copy(ps_string *ptr, ps_char *zstr);
    ps_string *ps_string_concat(ps_string *a, ps_string *b);
    ps_string *ps_string_get_slice(ps_string *a, ps_string_len from, ps_string_len to);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STRING_H */
