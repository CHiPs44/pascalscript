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

ps_array_data *ps_array_alloc_data(const ps_symbol *array)
{
    if (ps_array_debug)
        ps_symbol_debug(stderr, "ps_array_alloc_data, array_type: ", array);
    const ps_type_definition *type_def = ps_array_get_type_def(array);
    if (type_def == NULL || type_def->def.a.dimensions < 1)
        return NULL;
    ps_unsigned total = 1;
    ps_unsigned count;
    for (uint8_t i = 0; i < type_def->def.a.dimensions; i++)
    {
        count = ps_subrange_get_count(type_def->def.a.subranges[i]->value->data.t);
        if (count == PS_UNSIGNED_MAX)
            return NULL;
        total *= count;
    }
    if (ps_array_debug)
        fprintf(stderr, " DEBUG\tps_array_alloc_data, size: %u * %zu = %zu\n", total, sizeof(ps_value_data),
                total * sizeof(ps_value_data));
    ps_array_data *data = ps_memory_malloc(PS_MEMORY_VALUE, sizeof(ps_array_data) + total * sizeof(ps_value_data));
    if (data == NULL)
        return NULL;
    data->count = total;
    return data;
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
    ps_type_definition *type_def = ps_symbol_get_type_def(var_or_type);
    if (!ps_type_definition_is_array(type_def))
        return NULL;
    return type_def;
}

uint8_t ps_array_get_dimensions(const ps_symbol *array_type)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array_type);
    return type_def == NULL ? 0 : type_def->def.a.dimensions;
}

ps_symbol *ps_array_get_subrange(const ps_symbol *array_type, int dimension)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array_type);
    if (type_def == NULL || dimension >= type_def->def.a.dimensions)
        return NULL;
    return type_def->def.a.subranges[dimension];
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

ps_error ps_array_get_value_offset(const ps_symbol *array_var, int dimensions, const ps_value **indexes,
                                   ps_unsigned *final_offset)
{
    // Check if the array variable has the expected number of dimensions
    const ps_type_definition *type_def = ps_array_get_type_def(array_var);
    if (type_def == NULL)
        return PS_ERROR_INVALID_PARAMETERS;
    if (type_def->def.a.dimensions < dimensions)
        return PS_ERROR_NOT_ENOUGH_DIMENSIONS;
    if (type_def->def.a.dimensions > dimensions)
        return PS_ERROR_TOO_MANY_DIMENSIONS;

    // Calculate offset using row-major ordering (iterate backwards, right to left)
    *final_offset = 0;
    ps_unsigned stride = 1;
    for (int i = dimensions - 1; i >= 0; i -= 1)
    {
        ps_symbol *subrange = ps_array_get_subrange(array_var, i);
        if (subrange == NULL)
            return PS_ERROR_INVALID_PARAMETERS;
        ps_type_definition *subrange_def = ps_symbol_get_type_def(subrange);
        if (subrange_def == NULL || !ps_type_definition_is_subrange(subrange_def))
            return PS_ERROR_INVALID_PARAMETERS;
        // Copy given index to a local variable of the same type as subrange definition
        ps_value index = {.allocated = false, .type = subrange, .data = {0}};
        ps_error error = ps_value_copy(indexes[i], &index, true);
        if (error != PS_ERROR_NONE)
            return error;
        ps_unsigned index_offset = ps_subrange_get_offset(subrange_def, &index);
        ps_unsigned subrange_count = ps_subrange_get_count(subrange_def);
        if (index_offset >= subrange_count)
            return PS_ERROR_OUT_OF_RANGE;
        *final_offset += stride * index_offset;
        stride *= subrange_count;
    }

    // Check if the calculated offset is within the array bounds
    if (*final_offset >= array_var->value->data.a->count)
        return PS_ERROR_OUT_OF_RANGE;

    return PS_ERROR_NONE;
}

ps_error ps_array_get_value(const ps_symbol *array_var, int dimensions, const ps_value **indexes, ps_value *value,
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

ps_error ps_array_set_value(ps_symbol *array_var, int dimensions, const ps_value **indexes, const ps_value *value,
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

void ps_array_debug_type(FILE *output, ps_symbol *array_var)
{
    char buffer[32];

    if (output == NULL)
        output = stderr;

    const ps_type_definition *type_def = ps_array_get_type_def(array_var);
    if (type_def == NULL)
    {
        fprintf(output, "TYPE_DEF NULL\n");
        return;
    }
    int dimensions = ps_array_get_dimensions(array_var);
    ps_symbol *item_type = ps_array_get_item_type(array_var);

    fprintf(output, "========== ARRAY: %s ==========\n", array_var->name);
    ps_symbol_debug(output, "ITEM_TYPE ", item_type);
    ps_type_definition_debug(output, "TYPE_DEF ", type_def);

    for (int dimension = 0; dimension < dimensions; dimension += 1)
    {
        const ps_symbol *subrange = ps_array_get_subrange(array_var, dimension);
        snprintf(buffer, sizeof(buffer) - 1, "SUBRANGE %d/%d ", dimension, dimensions);
        ps_symbol_debug(output, buffer, subrange);
    }
}
