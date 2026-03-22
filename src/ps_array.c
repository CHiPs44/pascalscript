/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_array.h"
#include "ps_memory.h"

bool ps_array_debug = true;

ps_array_data *ps_array_alloc(const ps_symbol *array_type)
{
    if (ps_array_debug)
        ps_symbol_debug(stderr, "ps_array_alloc, array_type: ", array_type);
    const ps_type_definition *type_def = array_type->value->data.t;
    if (ps_array_debug)
        ps_type_definition_debug(stderr, "ps_array_alloc, type_def: ", type_def);
    ps_array_data *array_data = ps_memory_malloc(PS_MEMORY_VALUE, sizeof(ps_array_data));
    if (array_data == NULL)
        return NULL;
    array_data->count = ps_type_definition_get_subrange_count(type_def->def.a.subrange->value->data.t);
    if (ps_array_debug)
        fprintf(stderr, " DEBUG\tps_array_alloc, size: %u * %zu = %zu\n", array_data->count, sizeof(ps_value_data),
                array_data->count * sizeof(ps_value_data));
    if (array_data->count == PS_UNSIGNED_MAX)
        return NULL;
    array_data->values = ps_memory_calloc(PS_MEMORY_VALUE, array_data->count, sizeof(ps_value_data));
    if (array_data->values == NULL)
    {
        ps_memory_free(PS_MEMORY_VALUE, array_data);
        return NULL;
    }
    return array_data;
}

// TODO do not forget to call this when necessary
ps_array_data *ps_array_free(ps_array_data *array_data)
{
    if (ps_array_debug)
        fprintf(stderr, "ps_array_free, array_data: %p\n", (void *)array_data);
    ps_memory_free(PS_MEMORY_VALUE, array_data->values);
    ps_memory_free(PS_MEMORY_VALUE, array_data);
    return NULL;
}

ps_type_definition *ps_array_get_type_def(const ps_symbol *array_type)
{
    if (ps_array_debug)
        ps_symbol_debug(stderr, "ps_array_get_type_def, array_type: ", array_type);
    if (array_type == NULL || array_type->value == NULL || array_type->value->type == NULL ||
        array_type->value->type->value == NULL)
        return NULL;
    ps_type_definition *type_def = array_type->value->data.t;
    if (!ps_type_definition_is_array(type_def))
        return NULL;
    return type_def;
}

ps_symbol *ps_array_get_subrange(const ps_symbol *array_type)
{
    if (ps_array_debug)
        ps_symbol_debug(stderr, "ps_array_get_subrange, array_type: ", array_type);
    // WORKS(?) => array->value->type->value->data.t->def.a.subrange
    ps_symbol_debug(stderr, "GET_SUBRANGE\tARRAY\t\t", array_type);
    ps_value_debug(stderr, "GET_SUBRANGE\tVALUE\t\t", array_type->value);
    ps_symbol_debug(stderr, "GET_SUBRANGE\tTYPE\t\t", array_type->value->type);
    ps_value_debug(stderr, "GET_SUBRANGE\tTYPE\t\t", array_type->value->type->value);
    const ps_type_definition *type_def = ps_array_get_type_def(array_type);
    ps_type_definition_debug(stderr, "GET_SUBRANGE\tTYPE_DEF\t", type_def);
    if (type_def == NULL)
        return NULL;
    ps_symbol *subrange = type_def->def.a.subrange;
    ps_symbol_debug(stderr, "GET_SUBRANGE\tSUBRANGE\t", subrange);
    return subrange;
}

ps_symbol *ps_array_get_item_type(const ps_symbol *array_type)
{
    if (ps_array_debug)
        ps_symbol_debug(stderr, "ps_array_get_item_type, array_type: ", array_type);
    const ps_type_definition *type_def = ps_array_get_type_def(array_type);
    if (type_def == NULL)
        return NULL;
    return type_def->def.a.item_type;
}

ps_error ps_array_get_value(const ps_symbol *array, const ps_value *index, ps_value *value, bool range_check)
{
    if (ps_array_debug)
    {
        ps_symbol_debug(stderr, "ps_array_get_value, array: ", array);
        ps_value_debug(stderr, "ps_array_get_value, index: ", index);
    }
    const ps_type_definition *type_def = ps_array_get_type_def(array); // WIP: "array->value->type->value->data.t"
    ps_unsigned offset = ps_type_definition_get_subrange_offset(type_def->def.a.subrange->value->data.t, index);
    if (offset >= array->value->data.a->count)
        return PS_ERROR_OUT_OF_RANGE;
    ps_value array_value = {
        .allocated = false, .type = ps_array_get_item_type(array), .data = array->value->data.a->values[offset]};
    return ps_value_copy(&array_value, value, range_check);
}

ps_error ps_array_set_value(ps_symbol *array_var, const ps_value *index, const ps_value *value, bool range_check)
{
    if (ps_array_debug)
    {
        ps_symbol_debug(stderr, "ps_array_set_value, array_var: ", array_var);
        ps_value_debug(stderr, "ps_array_set_value, index: ", index);
    }
    if (array_var == NULL || array_var->value == NULL || array_var->value->type == NULL ||
        array_var->value->data.a->values == NULL)
        return PS_ERROR_INVALID_PARAMETERS;
    const ps_symbol *subrange = ps_array_get_subrange(array_var->value->type);
    ps_symbol_debug(stderr, "SET_VALUE ", subrange);
    ps_type_definition_debug(stderr, "SET_VALUE ", subrange->value->data.t);
    // Get offset from index
    ps_unsigned offset = ps_type_definition_get_subrange_offset(subrange->value->data.t, index);
    if (offset >= array_var->value->data.a->count)
        return PS_ERROR_INVALID_SUBRANGE;
    ps_value array_value = {.allocated = false, .type = ps_array_get_item_type(array_var), .data.v = NULL};
    ps_error error = ps_value_copy(value, &array_value, range_check);
    if (error != PS_ERROR_NONE)
        return error;
    array_var->value->data.a->values[offset] = array_value.data;
    return PS_ERROR_NONE;
}
