/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_array.h"
#include "ps_memory.h"

ps_array_data *ps_array_alloc(ps_symbol *type_symbol)
{
    ps_type_definition *type_def = type_symbol->value->type->value->data.t;
    ps_array_data *array_data = ps_memory_malloc(PS_MEMORY_VALUE, sizeof(ps_array_data));
    if (array_data == NULL)
        return NULL;
    array_data->count = ps_type_definition_get_subrange_count(type_def);
    array_data->values = ps_memory_calloc(PS_MEMORY_VALUE, array_data->count, sizeof(ps_value_data));
    if (array_data->values == NULL)
    {
        ps_memory_free(PS_MEMORY_VALUE, array_data);
        return NULL;
    }
    return array_data;
}

ps_array_data *ps_array_free(ps_array_data *array_data)
{
    ps_memory_free(PS_MEMORY_VALUE, array_data->values);
    ps_memory_free(PS_MEMORY_VALUE, array_data);
    return NULL;
}

ps_symbol *ps_array_get_subrange(ps_symbol *array)
{
    return array->value->type->value->data.t->def.a.subrange;
}

ps_symbol *ps_array_get_item_type(ps_symbol *array)
{
    return array->value->type->value->data.t->def.a.item_type;
}

bool ps_array_get_value(ps_symbol *array, ps_value *index, ps_value *value)
{
    ps_type_definition *type_def = array->value->type->value->data.t;
    // Get offset from index
    ps_unsigned offset = ps_type_definition_get_subrange_offset(type_def, index);
    if (offset >= array->value->data.a->count)
        return false;
    ps_value_data data = array->value->data.a->values[offset];
    if (value == NULL)
    {
        // Allocate value to "box" value data
        value = ps_value_alloc(ps_array_get_item_type(array), data);
        if (value == NULL)
            return false;
    }
    else
    {
        // Store value data to already "box" value
        value->data = data;
    }
    return true;
}

bool ps_array_set_value(ps_symbol *array, ps_value *index, ps_value_data data)
{
    ps_type_definition *type_def = array->value->type->value->data.t;
    // Get offset from index
    ps_unsigned offset = ps_type_definition_get_subrange_offset(type_def, index);
    if (offset >= array->value->data.a->count)
        return false;
    array->value->data.a->values[offset] = data;
    return true;
}
