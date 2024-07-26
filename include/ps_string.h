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

    ps_string *ps_string_new(ps_string_len max, ps_char *str);
    void ps_string_free(ps_string *str);

#ifdef __cplusplus
}
#endif

#endif /* _PS_STRING_H */
