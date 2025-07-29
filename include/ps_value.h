/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_VALUE_H
#define _PS_VALUE_H

#include <stdbool.h>
#include <stdlib.h>

#include "ps_type_definition.h"
#include "ps_value_data.h"
#include "ps_value_types.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // // Forward references
    // typedef struct s_ps_type_definition ps_type_definition;
    // typedef struct s_ps_parameters ps_parameters;

    /** @brief Value: type + data */
    typedef struct s_ps_value
    {
        ps_type_definition *type;
        ps_value_data data;
    } __attribute__((__packed__)) ps_value;

#define PS_VALUE_DATA_SIZE sizeof(ps_value_data)
#define PS_VALUE_SIZE sizeof(ps_value)

    ps_value *ps_value_alloc(ps_type_definition *type, ps_value_data data);
    ps_value *ps_value_free(ps_value *value);

    // bool ps_value_is_scalar(ps_value *value);
    // bool ps_value_is_numeric(ps_value *value);
    ps_value *ps_value_set_integer(ps_value *value, ps_integer i);
    ps_value *ps_value_set_unsigned(ps_value *value, ps_unsigned u);
    ps_value *ps_value_set_real(ps_value *value, ps_real r);
    ps_value *ps_value_set_boolean(ps_value *value, ps_boolean b);
    ps_value *ps_value_set_char(ps_value *value, ps_char c);
    /*
    ps_value  *ps_value_set_subrange(ps_value *value, ps_subrange g, ps_type_definition *type_def);
    ps_value  *ps_value_set_enum    (ps_value *value, ps_enum     e, ps_type_definition *type_def);
    ps_value  *ps_value_set_string  (ps_value *value, ps_string  *s);
    ps_value  *ps_value_set_pointer (ps_value *value, ps_pointer  p, ps_type_definition *type_def);
    */

    char *ps_value_get_display_string(ps_value *value);

    char *ps_value_get_debug_value(ps_value *value);
    void ps_value_debug(FILE *output, char *message, ps_value *value);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_H */
