/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_VALUE_H
#define _PS_VALUE_H

#include <stdlib.h>
#include <stdbool.h>

#include "ps_type_definition.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // clang-format off
    /** @brief Union value*/
    typedef union u_ps_value_data
    {
        //             Model/bytes 16  32  64 bits
        //                         --- --- ---
        ps_integer          i; //  2   4   8
        ps_unsigned         u; //  2   4   8
        ps_real             r; //  2?  4   8
        ps_boolean          b; //  1?  1?  1?
        ps_char             c; //  1   1   1
        ps_unsigned         e; //  2   4   8
        ps_pointer          p; //  2   4   8
        ps_string          *s; //  2   4   8
        // ps_type_definition *t; //  2   4   8
    } ps_value_data;
    // clang-format on

    /* clang-format off */
    typedef struct s_ps_value
    {
        ps_type_definition *type;
        ps_value_data data;
    } ps_value;
    /* clang-format on */

    // clang-format off
    ps_value *ps_value_set_integer (ps_value *value, ps_integer  i);
    ps_value *ps_value_set_unsigned(ps_value *value, ps_unsigned u);
    ps_value *ps_value_set_real    (ps_value *value, ps_real     r);
    ps_value *ps_value_set_boolean (ps_value *value, ps_boolean  b);
    ps_value *ps_value_set_char    (ps_value *value, ps_char     c);
    ps_value *ps_value_set_enum    (ps_value *value, ps_unsigned e, ps_type_definition *type_def);
    ps_value *ps_value_set_subrange(ps_value *value, ps_integer  i, ps_type_definition *type_def);
    ps_value *ps_value_set_string  (ps_value *value, char *s, ps_string_len max);
    ps_value *ps_value_set_pointer (ps_value *value, ps_pointer  p);
    // TODO?
    ps_integer  ps_value_get_integer (ps_value *value);
    ps_unsigned ps_value_get_unsigned(ps_value *value);
    ps_boolean  ps_value_get_boolean (ps_value *value);
    ps_char     ps_value_get_char    (ps_value *value);
    ps_string  *ps_value_get_string  (ps_value *value);
    ps_real     ps_value_get_real    (ps_value *value);
    ps_pointer  ps_value_get_pointer (ps_value *value);
    // clang-format on

    char *ps_value_get_type_name(ps_value_type type);
    char *ps_value_get_value(ps_value *value);
    void ps_value_debug(ps_value *value, char *message);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_H */
