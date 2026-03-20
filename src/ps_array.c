/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_array.h"
#include "ps_memory.h"

ps_array_data *ps_array_alloc(const ps_symbol *array)
{
    ps_symbol_debug(stderr, "ps_array_alloc, symbol: ", array);
    const ps_type_definition *type_def = array->value->data.t; //->value->data.t;
    ps_type_definition_debug(stderr, "ps_array_alloc, type_def: ", type_def);
    ps_array_data *array_data = ps_memory_malloc(PS_MEMORY_VALUE, sizeof(ps_array_data));
    if (array_data == NULL)
        return NULL;
    array_data->count = ps_type_definition_get_subrange_count(type_def->def.a.subrange->value->data.t);
    fprintf(stderr, "ps_array_alloc: %u x %zu = %zu\n", array_data->count, sizeof(ps_value_data),
            array_data->count * sizeof(ps_value_data));
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

ps_type_definition *ps_array_get_type_def(const ps_symbol *array)
{
    if (array == NULL || array->value == NULL || array->value->type == NULL || array->value->type->value == NULL)
        return NULL;
    return array->value->type->value->data.t;
}

ps_symbol *ps_array_get_subrange(const ps_symbol *array)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array);
    if (type_def == NULL)
        return NULL;
    return type_def->def.a.subrange;
}

ps_symbol *ps_array_get_item_type(const ps_symbol *array)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array);
    if (type_def == NULL)
        return NULL;
    return type_def->def.a.item_type;
}

ps_error ps_array_get_value(const ps_symbol *array, const ps_value *index, ps_value *value, bool range_check)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array); // WIP: "array->value->type->value->data.t"
    ps_unsigned offset = ps_type_definition_get_subrange_offset(type_def->def.a.subrange->value->data.t, index);
    if (offset >= array->value->data.a->count)
        return PS_ERROR_OUT_OF_RANGE;
    ps_value array_value = {
        .allocated = false, .type = ps_array_get_item_type(array), .data = array->value->data.a->values[offset]};
    return ps_value_copy(&array_value, value, range_check);
}

ps_error ps_array_set_value(ps_symbol *array, const ps_value *index, const ps_value *value, bool range_check)
{
    if (array == NULL || array->value == NULL || array->value->data.a->values == NULL)
        return PS_ERROR_INVALID_PARAMETERS;
    const ps_type_definition *type_def = ps_array_get_type_def(array);
    // Get offset from index
    ps_unsigned offset = ps_type_definition_get_subrange_offset(type_def, index);
    if (offset >= array->value->data.a->count)
        return PS_ERROR_INVALID_SUBRANGE;
    ps_value array_value = {.allocated = false, .type = ps_array_get_item_type(array), .data.v = NULL};
    ps_error error = ps_value_copy(value, &array_value, range_check);
    if (error != PS_ERROR_NONE)
        return error;
    array->value->data.a->values[offset] = value->data;
    return PS_ERROR_NONE;
}
