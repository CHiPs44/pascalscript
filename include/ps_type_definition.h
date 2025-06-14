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

    /** @brief Base types */
    typedef enum e_ps_value_type
    {
        PS_TYPE_NONE,       // No type yet or unknown
        PS_TYPE_REAL,       // real /float
        PS_TYPE_INTEGER,    // signed integer
        PS_TYPE_UNSIGNED,   // unsigned integer
        PS_TYPE_BOOLEAN,    // boolean
        PS_TYPE_CHAR,       // char
        PS_TYPE_STRING,     // string, Pascal style
        PS_TYPE_DEFINITION, // Type definition
        PS_TYPE_SUBRANGE,   // *FUTURE*
        PS_TYPE_ENUM,       // *FUTURE*
        PS_TYPE_SET,        // *FUTURE*
        PS_TYPE_POINTER,    // *FUTURE*
        PS_TYPE_ARRAY,      // *FUTURE*
        PS_TYPE_RECORD,     // *FUTURE*
        PS_TYPE_FILE,       // *FUTURE*
        PS_TYPE_OBJECT,     // *FUTURE*
    } __attribute__((__packed__)) ps_value_type;

    typedef struct s_ps_value_type_flags
    {
        bool is_base : 1;      /** @brief Base type (integer, real, boolean, char, string) */
        bool is_numeric : 1;   /** @brief Numeric value (integer, unsigned, real, subrange) */
        bool is_ordinal : 1;   /** @brief Ordinal value (integer, unsigned, char, subrange, enum) */
        bool is_scalar : 1;    /** @brief Scalar value (integer, unsigned, char, subrange, enum) */
        bool is_signed : 1;    /** @brief Signed value (integer, real, subrange) */
        bool is_reference : 1; /** @brief Reference value (string, pointer, array, record, file, object) */
    } /*__attribute__((__packed__))*/ ps_value_type_flags;

    extern ps_value_type_flags ps_value_type_flags_all[];

    /** @brief Subranges, stored as scalar values, needed to implement arrays */
    typedef struct s_ps_type_definition_subrange
    {
        ps_value_type base; /** @brief Base type of subrange: PS_TYPE_INTEGER, PS_TYPE_UNSIGNED, PS_TYPE_CHAR */
        ps_value min;
        ps_value max;
    } __attribute__((__packed__)) ps_type_definition_subrange;

    /** @brief Enums are stored as unsigned values (first=0, second=1, ...) */
    /** @example Months: (January, February, March, April, ..., December) */
    typedef struct s_ps_type_definition_enum
    {
        ps_unsigned count;
        /** @brief Array of symbols, each symbol is a value of the enum */
        ps_symbol *values;
    } __attribute__((__packed__)) ps_type_definition_enum;

    /** @brief Sets are stored in unsigned value as a bit field, each value of referenced enum is corresponding to
     * 2^ord(enum_value) */
    typedef struct s_ps_type_definition_set
    {
        ps_unsigned count; // max: UINT8_MAX, UINT16_MAX, UINT32_MAX, UINT64_MAX
        ps_symbol *symbol_enum;
    } __attribute__((__packed__)) ps_type_definition_set;

    /** @brief Pointer type is stored in a symbol */
    typedef struct s_ps_type_definition_pointer
    {
        ps_symbol *type_def;
    } __attribute__((__packed__)) ps_type_definition_pointer;

    /** @brief Type definition type stored in a symbol */
    typedef struct s_ps_type_definition_type_def
    {
        ps_symbol *type_def;
    } __attribute__((__packed__)) s_ps_type_definition_type_def;

    /** @brief *IN PROGRESS* => maximum length only, nothing more */
    typedef struct s_ps_type_definition_string
    {
        ps_string_len max;
    } __attribute__((__packed__)) ps_type_definition_string;

    /** @brief *FUTURE* => index goes from range->value->g.min to range->value->g.max */
    typedef struct s_ps_type_definition_array
    {
        ps_symbol *range;    /** @brief index range as subrange */
        ps_integer count;    /** @brief number of elements (max - min) + 1 */
        ps_symbol *type_def; /** @brief type of elements */
    } __attribute__((__packed__)) ps_type_definition_array;

    /** @brief Type definition: type + base + parameters if needed (simple types have type == base) */
    typedef struct s_ps_type_definition
    {
        ps_value_type type; /** @brief visible value type */
        ps_value_type base; /** @brief same as type for system types like integer or char,
                                       values for sub-type for subranges and enums, ... */
        union {
            ps_type_definition_enum def_enum;
            ps_type_definition_subrange def_subrange;
            ps_type_definition_set def_set;
            ps_type_definition_pointer def_pointer;
            ps_type_definition_string def_string;
            ps_type_definition_array def_array;
            // ps_type_definition_record def_record;
            // ps_type_definition_file def_file;
        } def;
    } __attribute__((__packed__)) ps_type_definition;

#define PS_TYPE_DEFINITION_SIZE sizeof(ps_type_definition)

#ifdef __cplusplus
}
#endif

#endif /* _PS_TYPE_DEFINITION */
