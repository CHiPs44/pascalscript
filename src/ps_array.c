/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_array.h"
#include "ps_memory.h"

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
    ps_unsigned count = ps_type_definition_get_subrange_count(type_def->def.a.subrange->value->data.t);
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

ps_type_definition *ps_array_get_type_def(const ps_symbol *symbol)
{
    if (ps_array_debug)
        ps_symbol_debug(stderr, "PS_ARRAY_GET_TYPE_DEF, => array    : ", symbol);
    if (symbol == NULL || symbol->value == NULL || symbol->value->type == NULL)
        return NULL;
    const ps_symbol *type_symbol = NULL;
    if (symbol->kind == PS_SYMBOL_KIND_TYPE_DEFINITION)
    {
        type_symbol = symbol;
        if (ps_array_debug)
            ps_symbol_debug(stderr, "PS_ARRAY_GET_TYPE_DEF, => type_def : ", type_symbol);
    }
    else if (symbol->kind == PS_SYMBOL_KIND_VARIABLE)
    {
        type_symbol = symbol->value->type;
        if (ps_array_debug)
            ps_symbol_debug(stderr, "PS_ARRAY_GET_TYPE_DEF, => variable : ", type_symbol);
    }
    else
    {
        if (ps_array_debug)
            ps_symbol_debug(stderr, "PS_ARRAY_GET_TYPE_DEF, => type/var!: ", symbol);
        return NULL;
    }
    ps_type_definition *type_def = type_symbol->value->data.t;
    if (!ps_type_definition_is_array(type_def))
        return NULL;
    if (ps_array_debug)
        ps_type_definition_debug(stderr, "PS_ARRAY_GET_TYPE_DEF, => type/var!: ", type_def);
    return type_def;
}

uint8_t *ps_array_get_dimensions(const ps_symbol *array_type)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array_type);
    return type_def == NULL ? 0 : type_def->def.a.dimensions;
}

ps_symbol *ps_array_get_subrange(const ps_symbol *array_type,uint8_t dimension)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array_type);
    return type_def == NULL ? NULL : type_def->def.a.subrange;
}

ps_symbol *ps_array_get_item_type(const ps_symbol *array_type)
{
    const ps_type_definition *type_def = ps_array_get_type_def(array_type);
    return type_def == NULL ? NULL : type_def->def.a.item_type;
}

ps_error ps_array_get_value(const ps_symbol *array_var, const ps_value *index, ps_value *value, bool range_check)
{
    if (ps_array_debug)
    {
        ps_symbol_debug(stderr, "ps_array_get_value, array: ", array_var);
        ps_value_debug(stderr, "ps_array_get_value, index: ", index);
    }
    const ps_type_definition *type_def = array_var->value->type->value->data.t;
    if (ps_array_debug)
        ps_type_definition_debug(stderr, "*** ps_array_get_value, type_def: ", type_def);
    ps_unsigned offset = ps_type_definition_get_subrange_offset(type_def->def.a.subrange->value->data.t, index);
    if (offset >= array_var->value->data.a->count)
        return PS_ERROR_OUT_OF_RANGE;
    ps_value array_value = {.allocated = false,
                            .type = ps_array_get_item_type(array_var),
                            .data = array_var->value->data.a->values[offset]};
    ps_error error = ps_value_copy(&array_value, value, range_check);
    if (ps_array_debug)
        ps_value_debug(stderr, "ps_array_get_value, array: ", &array_value);
    return error;
}

ps_error ps_array_set_value(ps_symbol *array_var, const ps_value **indicies, const ps_value *value, bool range_check)
{
    if (ps_array_debug)
    {
        ps_symbol_debug(stderr, "PS_ARRAY_SET_VALUE, array_var: ", array_var);
        ps_value_debug(stderr, "PS_ARRAY_SET_VALUE, index: ", index);
    }
    if (array_var == NULL || array_var->value == NULL || array_var->value->type == NULL ||
        array_var->value->data.a->values == NULL)
        return PS_ERROR_INVALID_PARAMETERS;
    const ps_symbol *subrange = ps_array_get_subrange(array_var->value->type);
    // Get offset from index
    ps_array_debug = true;
    ps_unsigned offset = ps_type_definition_get_subrange_offset(subrange->value->data.t, index);
    if (offset >= array_var->value->data.a->count)
        return PS_ERROR_INVALID_SUBRANGE;
    ps_array_debug = false;
    ps_value array_value = {.allocated = false, .type = ps_array_get_item_type(array_var), .data.v = NULL};
    ps_error error = ps_value_copy(value, &array_value, range_check);
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
