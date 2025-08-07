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
#include "ps_value.h"
#include "ps_value_data.h"
#include "ps_value_types.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // Forward reference
    typedef struct s_ps_value ps_value;

    // /** @brief Executable: function or procedure */
    // typedef struct s_ps_type_definition_executable
    // {
    //     ps_symbol *symbol;               /** @brief Symbol of the executable */
    //     ps_parameters *params;           /** @brief Parameters of the executable */
    //     ps_type_definition *return_type; /** @brief Return type of the executable, NULL for procedures */
    //     uint16_t line;                   /** @brief Line number in the source code */
    //     uint16_t column;                  /** @brief Column number in the source code */
    // } __attribute__((__packed__)) ps_type_definition_executable;

    // typedef struct s_ps_type_definition_subrange
    // {
    //     ps_value_type def; /** @brief type of subrange: integer, unsigned, char, enum */
    //     ps_value_data min; /** @brief Minimum value of the subrange */
    //     ps_value_data max; /** @brief Maximum value of the subrange */
    // } __attribute__((__packed__)) ps_type_definition_subrange;

    // /** @brief Enumerations are stored as unsigned values (first=0, second=1, ...) */
    // /** @example Months: (January, February, March, April, ..., December) */
    // typedef struct s_ps_type_definition_enum
    // {
    //     ps_unsigned count;
    //     /** @brief Array of symbols for each item in the enumeration */
    //     ps_symbol *values;
    // } __attribute__((__packed__)) ps_type_definition_enum;

    // /** @brief Sets are stored in unsigned value as a bit field, each value of referenced enum is corresponding to
    //  * 2^ord(enum_value) */
    // typedef struct s_ps_type_definition_set
    // {
    //     ps_unsigned count; // max: UINT8_MAX, UINT16_MAX, UINT32_MAX, UINT64_MAX
    //     ps_symbol *symbol_enum;
    // } __attribute__((__packed__)) ps_type_definition_set;

    // /** @brief Pointer type is stored in a symbol */
    // typedef struct s_ps_type_definition_pointer
    // {
    //     ps_symbol *type_def;
    // } __attribute__((__packed__)) ps_type_definition_pointer;

    /** @brief Type definition type stored in a symbol */
    typedef struct s_ps_type_definition_type_def
    {
        ps_symbol *type_def;
    } __attribute__((__packed__)) ps_type_definition_type_def;

    /** @brief *IN PROGRESS* => maximum length only, nothing more */
    typedef struct s_ps_type_definition_string
    {
        ps_string_len max;
    } __attribute__((__packed__)) ps_type_definition_string;

    // /** @brief *FUTURE* => index goes from range->value->g.min to range->value->g.max */
    // typedef struct s_ps_type_definition_array
    // {
    //     ps_symbol *range;    /** @brief index range as subrange */
    //     ps_unsigned count;   /** @brief number of elements (max - min) + 1 */
    //     ps_symbol *type_def; /** @brief type of elements, may be another array definition */
    // } __attribute__((__packed__)) ps_type_definition_array;

    /** @brief Type definition: type + base + parameters if needed (simple types have type == base) */
    typedef struct s_ps_type_definition
    {
        ps_value_type type; /** @brief visible value type */
        ps_value_type base; /** @brief same as type for internal types like integer or char,
                                       values for sub-type for subranges and enums, ... */
        union {
            // ps_type_definition_enum def_enum;
            // ps_type_definition_subrange def_subrange;
            // ps_type_definition_set def_set;
            // ps_type_definition_pointer def_pointer;
            ps_type_definition_string def_string;
            // ps_type_definition_array def_array;
            // ps_type_definition_record def_record;
            // ps_type_definition_file def_file;
        } def;
    } __attribute__((__packed__)) ps_type_definition;

#define PS_TYPE_DEFINITION_SIZE sizeof(ps_type_definition)

    char *ps_type_definition_get_name(ps_type_definition *type_def);

    void ps_type_definition_debug(FILE *output, char *message, ps_type_definition *type_def);

#ifdef __cplusplus
}
#endif

#endif /* _PS_TYPE_DEFINITION */
