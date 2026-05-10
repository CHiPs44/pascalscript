/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_array.h"
#include "ps_memory.h"
#include "ps_subrange.h"
#include "ps_symbol.h"
#include "ps_type_definition.h"
#include "ps_value.h"

bool ps_array_debug = false;

ps_array_data *ps_array_alloc_data(const ps_symbol *array_type)
{
    if (ps_array_debug)
        ps_symbol_debug(stderr, "ps_array_alloc_data, array_type: ", array_type);
    const ps_type_definition *type_def = array_type->value->data.t;
    if (ps_array_debug)
        ps_type_definition_debug(stderr, "ps_array_alloc_data, type_def: ", type_def);
    ps_array_data *array_data = ps_memory_malloc(PS_MEMORY_VALUE, sizeof(ps_array_data));
    if (array_data == NULL)
        return NULL;
    ps_unsigned count = ps_subrange_get_count(type_def->def.a.subrange->value->data.t);
    if (ps_array_debug)
        fprintf(stderr, " DEBUG\tps_array_alloc, size: %u * %zu = %zu\n", count, sizeof(ps_value_data),
                count * sizeof(ps_value_data));
    if (count == PS_UNSIGNED_MAX)
    {
        ps_memory_free(PS_MEMORY_VALUE, array_data);
        return NULL;
    }
    array_data->count = count;
    array_data->values = ps_memory_calloc(PS_MEMORY_VALUE, count, sizeof(ps_value_data));
    if (array_data->values == NULL)
    {
        ps_memory_free(PS_MEMORY_VALUE, array_data);
        return NULL;
    }
    return array_data;
}

ps_array_data *ps_array_free_data(ps_array_data *array_data)
{
    if (ps_array_debug)
        fprintf(stderr, "ps_array_free_data, array_data: %p\n", (void *)array_data);
    ps_memory_free(PS_MEMORY_VALUE, array_data->values);
    ps_memory_free(PS_MEMORY_VALUE, array_data);
    return NULL;
}

ps_type_definition *ps_array_get_type_def(const ps_symbol *var_or_type)
{
    if (ps_array_debug)
        ps_symbol_debug(stderr, "PS_ARRAY_GET_TYPE_DEF, => array    : ", var_or_type);
    if (var_or_type == NULL || var_or_type->value == NULL || var_or_type->value->type == NULL)
        return NULL;
    const ps_symbol *type = NULL;
    if (var_or_type->kind == PS_SYMBOL_KIND_TYPE_DEFINITION)
    {
        type = var_or_type;
        if (ps_array_debug)
            ps_symbol_debug(stderr, "PS_ARRAY_GET_TYPE_DEF, => type_def : ", type);
    }
    else if (var_or_type->kind == PS_SYMBOL_KIND_VARIABLE)
    {
        type = var_or_type->value->type;
        if (ps_array_debug)
            ps_symbol_debug(stderr, "PS_ARRAY_GET_TYPE_DEF, => variable : ", type);
    }
    else
    {
        if (ps_array_debug)
            ps_symbol_debug(stderr, "PS_ARRAY_GET_TYPE_DEF, => type/var!: ", var_or_type);
        return NULL;
    }
    ps_type_definition *type_def = type->value->data.t;
    if (!ps_type_definition_is_array(type_def))
        return NULL;
    if (ps_array_debug)
        ps_type_definition_debug(stderr, "PS_ARRAY_GET_TYPE_DEF, => type/var!: ", type_def);
    return type_def;
}

uint8_t ps_array_get_dimensions(const ps_symbol *array_type)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array_type);
    return type_def == NULL ? 0 : type_def->def.a.dimensions;
}

ps_symbol *ps_array_get_subrange(const ps_symbol *array_type)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array_type);
    return type_def == NULL ? NULL : type_def->def.a.subrange;
}

ps_symbol *ps_array_get_item_type(const ps_symbol *array_type)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array_type);
    if (type_def == NULL)
        return NULL;
    // Traverse through all nested array dimensions to find the actual element type
    ps_symbol *item_type = type_def->def.a.item_type;
    while (item_type != NULL)
    {
        const ps_type_definition *item_type_def = ps_array_get_type_def(item_type);
        if (item_type_def == NULL || !ps_type_definition_is_array(item_type_def))
            break; // reached the actual element type (not an array)
        item_type = item_type_def->def.a.item_type;
    }
    return item_type;
}

ps_error ps_array_get_value_offset(const ps_symbol *array_var, uint8_t dimensions, const ps_value **indexes,
                                   ps_unsigned *final_offset)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array_var);
    if (ps_array_debug)
    {
        ps_symbol_debug(stderr, "ps_array_get_value_offset, array: ", array_var);
        fprintf(stderr, "ps_array_get_value_offset, dimensions: %u\n", dimensions);
        for (uint8_t i = 0; i < dimensions; i += 1)
            ps_value_debug(stderr, "ps_array_get_value_offset, index[%u]: ", indexes[i]);
        ps_type_definition_debug(stderr, "ps_array_get_value_offset, type_def: ", type_def);
    }
    if (!ps_type_definition_is_array(type_def))
        return PS_ERROR_INVALID_PARAMETERS;
    if (type_def->def.a.dimensions < dimensions)
        return PS_ERROR_NOT_ENOUGH_DIMENSIONS;
    if (type_def->def.a.dimensions > dimensions)
        return PS_ERROR_TOO_MANY_DIMENSIONS;
    // Collect all subranges for each dimension
    ps_symbol *subranges[dimensions];
    ps_symbol *subrange = ps_array_get_subrange(array_var);
    for (uint8_t i = 0; i < dimensions; i += 1)
    {
        subranges[i] = subrange;
        if (i < dimensions - 1)
            subrange = ps_array_get_subrange(subrange);
    }
    // Calculate offset using row-major ordering (iterate backwards, right to left)
    *final_offset = 0;
    ps_unsigned stride = 1;
    for (int i = dimensions - 1; i >= 0; i -= 1)
    {
        // Copy given index to a local variable of the same type as subrange definition
        ps_value index = {.allocated = false, .type = subranges[i], .data.v = NULL};
        ps_error error = ps_value_copy(indexes[i], &index, true);
        if (error != PS_ERROR_NONE)
            return error;
        ps_unsigned index_offset = ps_subrange_get_offset(subranges[i]->value->data.t, &index);
        ps_unsigned subrange_count = ps_subrange_get_count(subranges[i]->value->data.t);
        if (index_offset >= subrange_count)
            return PS_ERROR_OUT_OF_RANGE;
        *final_offset += stride * index_offset;
        stride *= subrange_count;
    }
    if (*final_offset >= array_var->value->data.a->count)
        return PS_ERROR_OUT_OF_RANGE;
    if (ps_array_debug)
        fprintf(stderr, "ps_array_get_value_offset, final_offset: %u\n", *final_offset);
    return PS_ERROR_NONE;
}

ps_error ps_array_get_value(const ps_symbol *array_var, uint8_t dimensions, const ps_value **indexes, ps_value *value,
                            bool range_check)
{
    ps_unsigned offset = 0;
    ps_error error = ps_array_get_value_offset(array_var, dimensions, indexes, &offset);
    if (error != PS_ERROR_NONE)
        return error;
    ps_value array_value = {.allocated = false,
                            .type = ps_array_get_item_type(array_var),
                            .data = array_var->value->data.a->values[offset]};
    error = ps_value_copy(&array_value, value, range_check);
    if (ps_array_debug)
        ps_value_debug(stderr, "ps_array_get_value, array: ", &array_value);
    return error;
}

ps_error ps_array_set_value(ps_symbol *array_var, uint8_t dimensions, const ps_value **indexes, const ps_value *value,
                            bool range_check)
{
    ps_unsigned offset = 0;
    ps_error error = ps_array_get_value_offset(array_var, dimensions, indexes, &offset);
    if (error != PS_ERROR_NONE)
        return error;
    ps_value array_value = {.allocated = false, .type = ps_array_get_item_type(array_var), .data.v = NULL};
    error = ps_value_copy(value, &array_value, range_check);
    if (error != PS_ERROR_NONE)
        return error;
    array_var->value->data.a->values[offset] = array_value.data;
    return PS_ERROR_NONE;
}

void ps_array_debug_values(FILE *out, ps_symbol *array_var)
{
    if (out == NULL)
        out = stderr;
    fprintf(out, "========== ARRAY: %s ==========\n", array_var->name);
    const ps_type_definition *type_def = ps_array_get_type_def(array_var);
    ps_type_definition_debug(out, "TYPE_DEF ", type_def);
    const ps_symbol *subrange = ps_array_get_subrange(array_var);
    ps_symbol_debug(out, "SUBRANGE ", subrange);
    ps_symbol *item_type = ps_array_get_item_type(array_var);
    ps_value value = {.allocated = false, .type = item_type, .data.v = NULL};
    ps_unsigned count = array_var->value->data.a->count;
    fprintf(out, "count=%u\n", count);
    for (ps_unsigned i = 0; i < count; i += 1)
    {
        value.data = array_var->value->data.a->values[i];
        fprintf(out, " - %s[%u] = %s\n", array_var->name, i, ps_value_get_debug_string(&value));
    }
    fprintf(out, "\n");
}
