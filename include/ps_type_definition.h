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
#include "ps_subrange.h"
#include "ps_symbol.h"
#include "ps_system_types.h"
#include "ps_value.h"
#include "ps_value_data.h"
#include "ps_value_type.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // Forward reference
    typedef struct s_ps_value ps_value;

    /** @brief Enumerations are stored as unsigned integers (first=0, second=1, ...) */
    /** @example Months: (January, February, March, April, ..., December) with Ord(January)=0, ..., Ord(December)=11 */
    typedef struct s_ps_type_definition_enum
    {
        ps_unsigned count;  /** @brief Number of items in the enumeration */
        ps_symbol **values; /** @brief TODO use a VLA - Array of symbols for each item in the enumeration */
    } __attribute__((__packed__)) ps_type_definition_enum;

    typedef struct s_ps_type_definition_subrange
    {
        union {
            ps_type_definition_subrange_char c;
            ps_type_definition_subrange_integer i;
            ps_type_definition_subrange_unsigned u;
            ps_type_definition_subrange_enum e;
        };
    } __attribute__((__packed__)) ps_type_definition_subrange;

#define PS_TYPE_DEFINITION_SUBRANGE_SIZE sizeof(ps_type_definition_subrange)

    /** @brief Sets are stored in 32 bytes as a 256 bits field,
     *         each value of referenced enumeration or Char is corresponding
     *         to 2^Ord(enum_value or char_code)
     * @details For example, for a set of (One, Two, Three):
     *  - empty set is 0
     *  - set with One and Three is <253 zeroes>101 (5 in decimal)
     *  - set with Two only is <253 zeroes>010 (2 in decimal)
     *  - full set is <253 zeroes>111 (7 in decimal)
     *  NB: Set Of Char is a special case, as Char is not an enumeration, but the principle is the same
     */
    typedef struct s_ps_type_definition_set
    {
        ps_symbol *symbol_enum; /** @brief Symbol of the enumeration defining the set values */
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
    } __attribute__((__packed__)) ps_type_definition_type_def;

    /** @brief *IN PROGRESS* => maximum length only, nothing more */
    typedef struct s_ps_type_definition_string
    {
        ps_string_len max;
    } __attribute__((__packed__)) ps_type_definition_string;

    /** @brief Array type definition: type + dimensions + subranges */
    typedef struct s_ps_type_definition_array
    {
        ps_symbol *item_type;   /** @brief type of elements, may be another array definition */
        uint8_t dimensions;     /** @brief 1 for vectors, more than 1 for arrays of arrays   */
        ps_symbol *subranges[]; /** @brief index range as subrange                           */
    } __attribute__((__packed__)) ps_type_definition_array;

    /** @brief *FUTURE* */
    typedef struct s_ps_type_definition_record_field
    {
        ps_identifier name;  /** @brief field name */
        ps_symbol *type_def; /** @brief field type definition */
    } __attribute__((__packed__)) ps_type_definition_record_field;

    /** @brief *FUTURE* */
    typedef struct s_ps_type_definition_record
    {
        uint8_t fields_count;
        ps_type_definition_record_field *fields;
    } __attribute__((__packed__)) ps_type_definition_record;

    /** @brief *FUTURE* => file type definition */
    typedef struct s_ps_type_definition_file
    {
        ps_symbol *type_def; /** @brief type definition of the file content */
    } __attribute__((__packed__)) ps_type_definition_file;

    /** @brief Type definition: type + base + parameters if needed (simple types have type == base) */
    typedef struct s_ps_type_definition
    {
        ps_value_type type; /** @brief visible value type */
        ps_value_type base; /** @brief same as type for internal types like integer or char, values for sub-type for
                               subranges and enums, ... */
        union {
            // clang-format off
            ps_type_definition_enum     e; /** @brief _e_numeration      */
            ps_type_definition_subrange g; /** @brief subran_g_e         */
            ps_type_definition_string   s; /** @brief _s_tring           */
            ps_type_definition_array    a; /** @brief _a_rray            */
            ps_type_definition_set      t; /** @brief se_t_     *FUTURE* */
            ps_type_definition_pointer  p; /** @brief _p_ointer *FUTURE* */
            ps_type_definition_record   r; /** @brief _r_ecord  *FUTURE* */
            ps_type_definition_file     f; /** @brief _f_ile    *FUTURE* */
            // clang-format on
        } def; /** @brief type definition */
    } __attribute__((__packed__)) ps_type_definition;

#define PS_TYPE_DEFINITION_SIZE sizeof(ps_type_definition)

    char *ps_type_definition_get_name(const ps_type_definition *type_def);

    void ps_type_definition_debug(FILE *output, char *message, const ps_type_definition *type_def);

    ps_type_definition *ps_type_definition_alloc(ps_value_type type, ps_value_type base);
    ps_type_definition *ps_type_definition_free(ps_type_definition *type_def);
    ps_type_definition *ps_type_definition_create_string(ps_string_len max);
    ps_type_definition *ps_type_definition_create_array(ps_symbol *dimension);
    bool ps_type_definition_is_array(const ps_type_definition *type_def);

#ifdef __cplusplus
}
#endif

#endif /* _PS_TYPE_DEFINITION */
