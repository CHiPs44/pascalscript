/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_TYPE_DEFINITION
#define _PS_TYPE_DEFINITION

#include <stdbool.h>
#include <stdlib.h>

#include "ps_config.h"
#include "ps_symbol.h"
#include "ps_system_types.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // Forward reference
    typedef struct s_ps_value ps_value;

    /** @brief Base types /!\ DO NOT CHANGE ORDER, AS RANGES ARE USED IN CODE! /!\ */
    typedef enum e_ps_value_type
    {
        PS_TYPE_NONE = 0,   // No type yet or unknown
        PS_TYPE_DEFINITION, // Type definition
        /* Simple types => direct value */
        PS_TYPE_REAL, // float
        /* Simple types with scalar values (with ord/pred/succ) */
        PS_TYPE_INTEGER,  // signed integer
        PS_TYPE_UNSIGNED, // unsigned integer
        /* Simple types that are scalar but not numbers (without Abs for example) */
        PS_TYPE_BOOLEAN, // boolean
        PS_TYPE_CHAR,    // char
        /* User defineable types with scalar values (with Ord/Pred/Succ) */
        PS_TYPE_SUBRANGE, // *FUTURE*
        PS_TYPE_ENUM,     // *FUTURE*
        /* User defineable types without scalar values */
        PS_TYPE_SET,     // *FUTURE*
        PS_TYPE_POINTER, // *FUTURE*
        /* Reference types (pointer to value(s)) */
        PS_TYPE_STRING, // string, Pascal style
        PS_TYPE_ARRAY,  // *FUTURE*
        PS_TYPE_RECORD, // *FUTURE*
        PS_TYPE_FILE,   // *FUTURE*
        PS_TYPE_OBJECT, // *FUTURE*
    } __attribute__((__packed__)) ps_value_type;

    /** @brief Subranges, stored as integer values, needed to implement arrays */
    typedef struct s_ps_type_definition_subrange
    {
        ps_unsigned count;
        ps_integer min;
        ps_integer max;
    } __attribute__((__packed__)) ps_type_definition_subrange;

    /** @brief Enums are stored as unsigned values (first=0, second=1, ...) */
    typedef struct s_ps_type_definition_enum
    {
        ps_unsigned count;
        ps_symbol *values;
    } __attribute__((__packed__)) ps_type_definition_enum;

    // /** @brief Sets are stored in unsigned value as a bit field, each value of referenced enum is corresponding to
    // 2^ord(enum_value) */ typedef struct s_ps_type_definition_set
    // {
    //     ps_unsigned count; // max: UINT8_MAX, UINT16_MAX, UINT32_MAX, UINT64_MAX
    //     ps_symbol *symbol_enum;
    // } __attribute__((__packed__)) ps_type_definition_set;

    // /** @brief Pointer type is stored in a symbol */
    // typedef struct s_ps_type_definition_pointer
    // {
    //     ps_symbol *type_def;
    // } __attribute__((__packed__)) ps_type_definition_pointer;

    // /** @brief Type definition type stored in a symbol */
    // typedef struct s_ps_type_definition_type_def
    // {
    //     ps_symbol *type_def;
    // } __attribute__((__packed__)) s_ps_type_definition_type_def;

    // /** @brief *IN PROGRESS* => maximum length only, nothing more */
    // typedef struct s_ps_type_definition_string
    // {
    //     ps_string_len max;
    // } __attribute__((__packed__)) ps_type_definition_string;

    /** @brief Type definition: type + base + parameters if needed (simple types have type == base) */
    typedef struct s_ps_type_definition
    {
        ps_value_type type; /** @brief visible value type */
        ps_value_type base; /** @brief same as type for system types like integer or char,
                                       values for sub-type for subranges and enums */
        union {
            ps_type_definition_enum def_enum;
            ps_type_definition_subrange def_subrange;
            // ps_type_definition_set def_set;
            // ps_type_definition_pointer def_pointer;
            // ps_type_definition_string def_string;
            // ps_type_definition_array def_array;
            // ps_type_definition_record def_record;
            // ps_type_definition_file def_file;
        } def;
    } __attribute__((__packed__)) ps_type_definition;

#define PS_TYPE_DEFINITION_SIZE sizeof(ps_type_definition)

#ifdef __cplusplus
}
#endif

#endif /* _PS_TYPE_DEFINITION */
