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
#include "ps_symbol.h"
#include "ps_system_types.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Base types */
    typedef enum e_ps_value_type
    {
        /* Simple types => direct value */
        PS_TYPE_NONE = 0,
        PS_TYPE_INTEGER,
        PS_TYPE_UNSIGNED,
        PS_TYPE_REAL,
        PS_TYPE_BOOLEAN,
        PS_TYPE_CHAR,
        /* User defineable types */
        PS_TYPE_DEFINITION,
        PS_TYPE_ENUM,
        PS_TYPE_SUBRANGE,
        PS_TYPE_SET,
        PS_TYPE_POINTER,
        /* Reference types (pointer to value(s)) */
        PS_TYPE_STRING, // *IN PROGRESS*
        PS_TYPE_ARRAY,  // *FUTURE*
        PS_TYPE_RECORD, // *FUTURE*
        PS_TYPE_FILE,   // *FUTURE*
        PS_TYPE_OBJECT, // *FUTURE*
        PS_TYPE_MAX = UINT16_MAX
    } __attribute__((__packed__)) ps_value_type;

    typedef struct s_ps_type_name
    {
        bool is_base_type;
        ps_value_type type;
        char *name;
    } ps_type_name;

    /** @brief Enums are stored in unsigned value (first=0, second=1, ...) */
    typedef struct s_ps_type_definition_enum
    {
        ps_unsigned count;
        ps_identifier *values;
    } ps_type_definition_enum;

    /** @brief Subranges limits stored in simple values (-10..15 or 'A'..'Z'), needed to implement arrays */
    typedef struct s_ps_type_definition_subrange
    {
        ps_unsigned count;
        ps_value min;
        ps_value max;
    } ps_type_definition_subrange;

    /** @brief Sets are stored in unsigned value as a bit field */
    typedef struct s_ps_type_definition_set
    {
        ps_unsigned count; // max: UINT8_MAX, UINT16_MAX, UINT32_MAX, UINT64_MAX
        ps_identifier *values;
    } ps_type_definition_set;

    /** @brief stored in a symbol */
    typedef struct s_ps_type_definition_pointer
    {
        ps_symbol *type_def;
    } ps_type_definition_pointer;

    /** @brief stored in a symbol */
    typedef struct s_ps_type_definition_pointer
    {
        ps_symbol *type_def;
    } ps_type_definition_pointer;

    /** @brief *IN PROGRESS* => maximum length only, nothing more */
    typedef struct s_ps_type_definition_string
    {
        ps_string_len max;
    } ps_type_definition_string;

    /** @brief Type definition: base + parameters if needed */
    typedef struct s_ps_type_definition
    {
        ps_value_type base;
        union
        {
            ps_type_definition_enum def_enum;
            ps_type_definition_subrange def_subrange;
            ps_type_definition_set def_set;
            ps_type_definition_pointer def_pointer;
            ps_type_definition_string def_string;
            // ps_type_definition_array def_array;
            // ps_type_definition_record def_record;
            // ps_type_definition_file def_file;
        } def;
    } ps_type_definition;

    extern ps_type_definition ps_type_def_integer;
    extern ps_type_definition ps_type_def_unsigned;
    extern ps_type_definition ps_type_def_real;
    extern ps_type_definition ps_type_def_boolean;
    extern ps_type_definition ps_type_def_char;

#ifdef __cplusplus
}
#endif

#endif /* _PS_TYPE_DEFINITION */
