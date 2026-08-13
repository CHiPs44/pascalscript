/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SUBRANGE
#define _PS_SUBRANGE

#include "ps_config.h"
#include "ps_enum.h"
#include "ps_symbol.h"
#include "ps_type_definition.h"

#ifdef __cplusplus
extern "C"
{
#endif

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
