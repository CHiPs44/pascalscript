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
#include "ps_enum.h"
#include "ps_subrange.h"
#include "ps_system_types.h"
#include "ps_value_data.h"
#include "ps_value_type.h"

/* forward declarations to avoid cyclic includes */
typedef struct s_ps_symbol ps_symbol;

#ifdef __cplusplus
extern "C"
{
#endif

    // Forward reference
    typedef struct s_ps_value ps_value;

    /**
     * @brief Enumerations are stored as unsigned integers (first=0, second=1, ...)
     *
     *        Example: Months = (January, February, March, April, ..., December)
     *                 with Ord(January)=0, ..., Ord(December)=11
     */
    typedef struct s_ps_type_definition_enum
    {
        ps_unsigned count;  /** @brief Number of items in the enumeration */
        ps_symbol **values; /** @brief TODO use a VLA - Array of symbols for each item in the enumeration */
    } __attribute__((__packed__)) ps_type_definition_enum;

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

    typedef struct s_ps_type_definition_subrange_enum
    {
        ps_symbol *symbol_enum; /** @brief Symbol of the enumeration defining the subrange values */
        ps_unsigned min;        /** @brief Minimum value in the enumeration for the subrange      */
        ps_unsigned max;        /** @brief Maximum value in the enumeration for the subrange      */
    } __attribute__((__packed__)) ps_type_definition_subrange_enum;

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

    /** @brief Array type definition: item type + dimensions + subranges */
    typedef struct s_ps_type_definition_array
    {
        ps_symbol *item_type;  /** @brief type of elements, may be another array definition */
        int dimensions;        /** @brief 1 for vectors, more than 1 for arrays of arrays   */
        ps_symbol **subranges; /** @brief index range as subrange                           */
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
        uint8_t count;
        ps_type_definition_record_field **fields;
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

    /**
     * @brief Get the name of a type definition
     * @param type_def Pointer to the type definition
     * @return Name string of the type definition
     */
    char *ps_type_definition_get_name(const ps_type_definition *type_def);

    /**
     * @brief Debug print a type definition
     * @param output File stream for output
     * @param message Message prefix to print
     * @param type_def Pointer to the type definition to debug
     */
    void ps_type_definition_debug(FILE *output, char *message, const ps_type_definition *type_def);

    /**
     * @brief Allocate and initialize a new type definition
     * @param type Visible value type
     * @param base Base value type
     * @return Pointer to the newly allocated type definition
     */
    ps_type_definition *ps_type_definition_alloc(ps_value_type type, ps_value_type base);

    /**
     * @brief Free a type definition and its resources
     * @param type_def Pointer to the type definition to free
     * @return NULL pointer
     */
    ps_type_definition *ps_type_definition_free(ps_type_definition *type_def);

    /**
     * @brief Create a string type definition
     * @param max Maximum length of the string
     * @return Pointer to the newly created string type definition
     */
    ps_type_definition *ps_type_definition_create_string(ps_string_len max);

    /**
     * @brief Create an array type definition
     * @param item_type Symbol of the element type
     * @param dimensions Number of dimensions (1 for vector, >1 for multi-dimensional arrays)
     * @param subranges Array of subrange symbols for each dimension
     * @return Pointer to the newly created array type definition
     */
    ps_type_definition *ps_type_definition_create_array(ps_symbol *item_type, int dimensions, ps_symbol **subranges);

    /**
     * @brief Check if a type definition is an enumeration
     * @param type_def Pointer to the type definition
     * @return true if the type is an enumeration, false otherwise
     */
    bool ps_type_definition_is_enum(const ps_type_definition *type_def);

    /**
     * @brief Check if a type definition is a subrange
     * @param type_def Pointer to the type definition
     * @return true if the type is a subrange, false otherwise
     */
    bool ps_type_definition_is_subrange(const ps_type_definition *type_def);

    /**
     * @brief Check if a type definition is an array
     * @param type_def Pointer to the type definition
     * @return true if the type is an array, false otherwise
     */
    bool ps_type_definition_is_array(const ps_type_definition *type_def);

#ifdef __cplusplus
}
#endif

#endif /* _PS_TYPE_DEFINITION */
