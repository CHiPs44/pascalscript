/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_ARRAY_H
#define _PS_ARRAY_H

#ifdef __cplusplus
extern "C"
{
#endif

#include "ps_value_data.h"

    typedef struct s_ps_array_data
    {
        ps_unsigned count;
        ps_value_data *values;
    } ps_array_data;

    /** @brief Allocate array and values */
    ps_array_data *ps_array_alloc(const ps_symbol *type_symbol);
    /** @brief Free array and values */
    ps_array_data *ps_array_free(ps_array_data *data);

    /** @brief Get array type definition */
    ps_type_definition *ps_array_get_type_def(const ps_symbol *array);
    /** @brief Get array subrange */
    ps_symbol *ps_array_get_subrange(const ps_symbol *array);
    /** @brief Get array item type */
    ps_symbol *ps_array_get_item_type(const ps_symbol *array);
    /** @brief value := array[index] (allocating value if NULL) */
    bool ps_array_get_value(const ps_symbol *array, const ps_value *index, ps_value *value);
    /** @brief array[index] := value */
    bool ps_array_set_value(ps_symbol *array, ps_value *index, ps_value_data data);

#ifdef __cplusplus
}
#endif

#endif /* _PS_ARRAY_H */
