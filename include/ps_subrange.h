/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SUBRANGE
#define _PS_SUBRANGE

#include "ps_config.h"
#include "ps_symbol.h"
#include "ps_type_definition.h"

#ifdef __cplusplus
extern "C"
{
#endif

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

    typedef ps_unsigned ps_enum_value;
    typedef struct s_ps_type_definition_subrange_enum
    {
        ps_symbol *symbol_enum; /** @brief Symbol of the enumeration defining the subrange values */
        ps_enum_value min;      /** @brief Minimum value in the enumeration for the subrange      */
        ps_enum_value max;      /** @brief Maximum value in the enumeration for the subrange      */
    } __attribute__((__packed__)) ps_type_definition_subrange_enum;

    /*
        This file is part of the PascalScript Pascal interpreter.
        SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
        SPDX-License-Identifier: LGPL-3.0-or-later
    */

    /** @brief Create a char based subrange type definition */
    ps_type_definition *ps_subrange_create_char(ps_char min, ps_char max);

    /** @brief Create an integer based subrange type definition */
    ps_type_definition *ps_subrange_create_integer(ps_integer min, ps_integer max);

    /** @brief Create an unsigned based subrange type definition */
    ps_type_definition *ps_subrange_create_unsigned(ps_unsigned min, ps_unsigned max);

    /** @brief Create an enum based subrange type definition */
    ps_type_definition *ps_subrange_create_enum(ps_symbol *symbol_enum, ps_enum_value min, ps_enum_value max);

    /**
     * @brief Get the number of elements in a subrange
     *
     *        Example: 'A'..'F' will return return Ord('F') - Ord('A') e.g. 6
     *
     * @returns PS_UNSIGNED_MAX if not a subrange
     */
    ps_unsigned ps_subrange_get_count(const ps_type_definition *type_def);

    /**
     * @brief Get the zero based offset of an index in a subrange
     *
     *        Example: 'C' in 'A'..'F' will return Ord('C') - Ord('A') e.g. 2
     *
     * @returns PS_UNSIGNED_MAX if not a subrange or index is out of range
     */
    ps_unsigned ps_subrange_get_offset(const ps_type_definition *type_def, const ps_value *index);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SUBRANGE */
