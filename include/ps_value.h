/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
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

    // Forward reference to parameters
    typedef struct s_ps_parameters ps_parameters;

    /** @brief Union value */
    typedef union u_ps_value_data
    {
        // clang-format off
        //             Model/bytes  16  32  64
        //                         --- --- --- ---------------------------
        ps_integer          i; //  2   4   8   "i" is for "_i_nteger"
        ps_unsigned         u; //  2   4   8   "u" is for "_u_nsigned"
        ps_real             r; //  2?  4   8   "r" is for "_r_eal"
        ps_boolean          b; //  1?  1?  1?  "b" is for "_b_oolean"
        ps_char             c; //  1   1   1   "c" is for "_c_har"
        /*
        ps_subrange         g; //  2   4   8   "g" is for "subran_g_e"
        ps_enum             e; //  2   4   8   "e" is for "_e_num"
        ps_pointer          p; //  2   4   8   "p" is for "_p_ointer"
        ps_string          *s; //  2   4   8   "s" is for "_s_tring"
        */
        ps_type_definition *t; //  2   4   8   "t" is for "_t_ype"
        ps_signature       *z; //  2   4   8   "z" is for "_z_ignature" ;-)
        void               *v; //  2   4   8   "v" is for "_v_oid"
        // clang-format on
    } __attribute__((__packed__)) ps_value_data;

    /** @brief Value */
    typedef struct s_ps_value
    {
        ps_type_definition *type;
        ps_value_data data;
    } __attribute__((__packed__)) ps_value;

#define PS_VALUE_DATA_SIZE sizeof(ps_value_data)
#define PS_VALUE_SIZE sizeof(ps_value)

    ps_value *ps_value_init(ps_type_definition *type, ps_value_data data);

    // clang-format off
    bool       ps_value_is_ordinal   (ps_value *value);
    bool       ps_value_is_number   (ps_value *value);
    ps_value  *ps_value_set_integer (ps_value *value, ps_integer  i);
    ps_value  *ps_value_set_unsigned(ps_value *value, ps_unsigned u);
    ps_value  *ps_value_set_real    (ps_value *value, ps_real     r);
    ps_value  *ps_value_set_boolean (ps_value *value, ps_boolean  b);
    ps_value  *ps_value_set_char    (ps_value *value, ps_char     c);
    // ps_value  *ps_value_set_subrange(ps_value *value, ps_subrange g, ps_type_definition *type_def);
    // ps_value  *ps_value_set_enum    (ps_value *value, ps_enum     e, ps_type_definition *type_def);
    // ps_value  *ps_value_set_string  (ps_value *value, ps_string  *s);
    // ps_value  *ps_value_set_pointer (ps_value *value, ps_pointer  p, ps_type_definition *type_def);
    // ps_string *ps_value_new_string  (char *s, ps_string_len max, ps_string_len len);
    // clang-format on

    char *ps_value_get_display_value(ps_value *value);

    char *ps_value_get_type_name(ps_value_type type);
    char *ps_value_get_type_definition_name(ps_type_definition *type_def);
    char *ps_value_get_debug_value(ps_value *value);
    void ps_value_debug(FILE *output, char *message, ps_value *value);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_H */
