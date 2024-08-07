/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_TYPE_DEFINITION
#define _PS_TYPE_DEFINITION

#include <stdlib.h>
#include <stdbool.h>

#include "ps_config.h"
#include "ps_types.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Base types */
    typedef enum e_ps_value_type
    {
        /* simple types => direct value */
        PS_TYPE_NONE = 0,
        PS_TYPE_INTEGER,
        PS_TYPE_UNSIGNED,
        PS_TYPE_REAL,
        PS_TYPE_BOOLEAN,
        PS_TYPE_CHAR,
        /* user defineable types */
        PS_TYPE_DEFINITION,
        PS_TYPE_ENUM,     // *FUTURE*
        PS_TYPE_SUBRANGE, // *FUTURE*
        PS_TYPE_SET,      // *FUTURE*
        PS_TYPE_POINTER,  // *FUTURE*
        /* reference types (pointer to value(s)) */
        PS_TYPE_STRING, // *IN PROGRESS*
        PS_TYPE_ARRAY,  // *FUTURE*
        PS_TYPE_RECORD, // *FUTURE*
        PS_TYPE_FILE,   // *FUTURE*
    } __attribute__((__packed__)) ps_value_type;

    /** @brief *FUTURE* => stored in unsigned value (first=0, second=1, ...) */
    typedef struct s_ps_type_definition_enum
    {
        ps_unsigned count;
        ps_identifier *values;
    } ps_type_definition_enum;

    /** @brief *FUTURE* => stored in integer value (-10..15), reference to type needed, needed to implement arrays */
    typedef struct s_ps_type_definition_subrange
    {
        ps_integer min;
        ps_integer max;
    } ps_type_definition_subrange;

    /** @brief *FUTURE* => stored in unsigned value as a bit field */
    typedef struct s_ps_type_definition_set
    {
        ps_unsigned count;
        ps_identifier *values;
    } ps_type_definition_set;

    /** @brief *IN PROGRESS* => ... */
    typedef struct s_ps_type_definition_string
    {
        ps_string_len max_length;
    } ps_type_definition_string;

    /** @brief  */
    typedef struct
    {
        ps_value_type base;
        union
        {
            ps_type_definition_enum def_enum;
            ps_type_definition_subrange def_subrange;
            ps_type_definition_set def_set;
            ps_type_definition_string def_string;
            // ps_type_definition_pointer def_pointer;
            // ps_type_definition_array def_array;
        } def;
    } ps_type_definition;

#ifdef __cplusplus
}
#endif

#endif /* _PS_TYPE_DEFINITION */
