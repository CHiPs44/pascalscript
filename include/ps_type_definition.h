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
#include "ps_value.h"
#include "ps_value_data.h"
#include "ps_value_types.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // Forward reference
    typedef struct s_ps_value ps_value;

    typedef struct s_ps_type_definition_subrange_char
    {
        ps_char min;
        ps_char max;
    } __attribute__((__packed__)) ps_type_definition_subrange_char;

    typedef struct s_ps_type_definition_subrange_integer
    {
        ps_integer min;
        ps_integer max;
    } __attribute__((__packed__)) ps_type_definition_subrange_integer;

    typedef struct s_ps_type_definition_subrange_unsigned
    {
        ps_unsigned min;
        ps_unsigned max;
    } __attribute__((__packed__)) ps_type_definition_subrange_unsigned;

    typedef uint8_t ps_enum_value;
    typedef struct s_ps_type_definition_subrange_enum
    {
        ps_symbol *symbol_enum; /** @brief Symbol of the enumeration defining the subrange values */
        ps_enum_value min;      /** @brief min value in the enumeration for the subrange */
        ps_enum_value max;      /** @brief max value in the enumeration for the subrange */
    } __attribute__((__packed__)) ps_type_definition_subrange_enum;

    typedef struct s_ps_type_definition_subrange
    {
        union
        {
            ps_type_definition_subrange_char c;
            ps_type_definition_subrange_integer i;
            ps_type_definition_subrange_unsigned u;
            ps_type_definition_subrange_enum e;
        };
    } __attribute__((__packed__)) ps_type_definition_subrange;

    /** @brief Enumerations are stored as unsigned bytes (first=0, second=1, ...) */
    /** @example Months: (January, February, March, April, ..., December) */
    typedef struct s_ps_type_definition_enum
    {
        /** @brief Array of symbols for each item in the enumeration */
        ps_symbol **values;
        /** @brief Number of items in the enumeration */
        uint8_t count;
    } __attribute__((__packed__)) ps_type_definition_enum;

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

    /** @brief *FUTURE* => index goes from range->value->g.min to range->value->g.max */
    typedef struct s_ps_type_definition_array
    {
        ps_symbol *range;    /** @brief index range as subrange */
        ps_unsigned count;   /** @brief number of elements (max - min) + 1 */
        ps_symbol *type_def; /** @brief type of elements, may be another array definition */
    } __attribute__((__packed__)) ps_type_definition_array;

    typedef struct s_ps_type_definition_record_field
    {
        ps_symbol *name;                                /** @brief field name */
        ps_symbol *type_def;                            /** @brief field type definition */
        struct s_ps_type_definition_record_field *next; /** @brief next field in the record, NULL if last */
    } __attribute__((__packed__)) ps_type_definition_record_field;

    /** @brief *FUTURE* => linked list of fields */
    typedef struct s_ps_type_definition_record
    {
        ps_type_definition_record_field *fields; /** @brief linked list of fields */
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
        ps_value_type base; /** @brief same as type for internal types like integer or char,
                                       values for sub-type for subranges and enums, ... */
        union
        {
            ps_type_definition_enum e;
            ps_type_definition_subrange g;
            ps_type_definition_set t;
            ps_type_definition_pointer p;
            ps_type_definition_string s;
            ps_type_definition_array a;
            ps_type_definition_record r;
            ps_type_definition_file f;
        } def;
    } /*__attribute__((__packed__))*/ ps_type_definition;

#define PS_TYPE_DEFINITION_SIZE sizeof(ps_type_definition)

    char *ps_type_definition_get_name(const ps_type_definition *type_def);

    void ps_type_definition_debug(FILE *output, char *message, ps_type_definition *type_def);

    // clang-format off
    ps_type_definition *ps_type_definition_create                  (ps_value_type type, ps_value_type base);
    ps_type_definition *ps_type_definition_free                    (ps_type_definition *type_def);
    ps_type_definition *ps_type_definition_create_string           (ps_string_len max);
    ps_type_definition *ps_type_definition_create_enum             (uint8_t count, ps_symbol **values);
    ps_type_definition *ps_type_definition_create_subrange_char    (ps_char     min, ps_char     max);
    ps_type_definition *ps_type_definition_create_subrange_integer (ps_integer  min, ps_integer  max);
    ps_type_definition *ps_type_definition_create_subrange_unsigned(ps_unsigned min, ps_unsigned max);
    ps_type_definition *ps_type_definition_create_subrange_enum    (ps_symbol *symbol_enum, ps_enum_value min, ps_enum_value max);
    // clang-format on

#ifdef __cplusplus
}
#endif

#endif /* _PS_TYPE_DEFINITION */
