/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "ps_error.h"
#include "ps_value.h"
#include "ps_runtime.h"
#include "ps_version.h"

/** @brief Create new runtime */
ps_runtime *ps_runtime_init(ps_runtime *runtime)
{
    char buffer[32];
    ps_value *value;

    bool allocated = false;
    if (runtime == NULL)
    {
        runtime = calloc(1, sizeof(ps_runtime));
        if (runtime == NULL)
            return NULL;
        allocated = true;
    }
    /* Parser */
    runtime->parser = ps_parser_init(NULL);
    if (runtime->parser == NULL)
    {
        if (allocated)
            free(runtime);
        return NULL;
    }
    // /* Symbol table */
    // runtime->symbols = ps_symbol_table_init(NULL);
    // if (runtime->symbols == NULL)
    // {
    //     free(runtime->parser);
    //     if (allocated)
    //         free(runtime);
    //     return NULL;
    // }
    runtime->error = PS_RUNTIME_ERROR_NONE;

    // Version
    value = ps_value_set_unsigned(NULL, PS_VERSION_MAJOR);
    ps_runtime_add_system_constant(runtime, "PS_VERSION_MAJOR", value);
    value = ps_value_set_unsigned(NULL, PS_VERSION_MINOR);
    ps_runtime_add_system_constant(runtime, "PS_VERSION_MINOR", value);
    value = ps_value_set_unsigned(NULL, PS_VERSION_PATCH);
    ps_runtime_add_system_constant(runtime, "PS_VERSION_PATCH", value);
    value = ps_value_set_unsigned(NULL, PS_VERSION_INDEX);
    ps_runtime_add_system_constant(runtime, "PS_VERSION_INDEX", value);
    snprintf(buffer, sizeof(buffer) - 1, "%d.%d.%d.%d", PS_VERSION_MAJOR, PS_VERSION_MINOR, PS_VERSION_PATCH, PS_VERSION_INDEX);
    value = ps_value_set_string(NULL, buffer, strlen(buffer), strlen(buffer));
    ps_runtime_add_system_constant(runtime, "PS_VERSION", value);
    // Limits
    value = ps_value_set_integer(NULL, ps_integer_max);
    ps_runtime_add_system_constant(runtime, "MAXINT", value);
    value = ps_value_set_unsigned(NULL, ps_unsigned_max);
    ps_runtime_add_system_constant(runtime, "MAXUINT", value);
    // These are keywords for now (until enums are implemented)
    // value = ps_value_set_boolean(NULL, ps_false);
    // ps_runtime_add_system_constant(runtime, "FALSE", value);
    // value = ps_value_set_boolean(NULL, ps_true);
    // ps_runtime_add_system_constant(runtime, "TRUE", value);
    // Reals without PI is not conceivable
    value = ps_value_set_real(NULL, 3.141592653589793); // 115997963468544185161590576171875);
    ps_runtime_add_system_constant(runtime, "PI", value);
    return runtime;
}

ps_runtime *ps_runtime_done(ps_runtime *runtime)
{
    ps_parser_init
    free(runtime);
}

/** @brief Allocate new value */
ps_value *ps_runtime_alloc_value(ps_runtime *runtime)
{
    if (runtime->error != PS_RUNTIME_ERROR_NONE)
        return NULL;
    ps_value *value = (ps_value *)calloc(1, sizeof(ps_value));
    if (value == NULL)
    {
        runtime->error = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
        return NULL;
    }
    return value;
}

void ps_runtime_free_value(ps_runtime *runtime, ps_value *value)
{
    free(value);
}

ps_symbol *ps_runtime_auto_add_value(ps_runtime *runtime, ps_symbol_scope scope, ps_value value)
{
    ps_symbol symbol;
    strcpy(symbol.name, "");
    symbol.kind = PS_SYMBOL_KIND_AUTO;
    symbol.scope = scope;
    symbol.value.type = PS_TYPE_INTEGER;
    symbol.value.data.i = value.data.i;
    int index = ps_symbol_table_add(runtime->symbols, &symbol);
    return index >= 0 ? &runtime->symbols[index] : NULL;
}

/**
 * @brief Free auto variable after use
 *
 * @param VM
 * @param string Normalized name
 * @return index of symbol or -1 if not found
 */
int ps_runtime_auto_free(ps_runtime *runtime, char *name)
{
    return ps_symbol_table_free(runtime->symbols, name);
}

/**
 * @brief Garbage collector: release free symbols
 *
 * @param VM
 * @return Count of garbage collected symbols
 */
int ps_runtime_auto_gc(ps_runtime *runtime)
{
    int count = ps_symbol_table_gc(runtime->symbols);
    fprintf(stderr, "*** VM_AUTO_GC: %d symbol%s freed\n", count, count > 0 ? "s" : "");
    return count;
}

bool ps_runtime_load_source(ps_runtime *runtime, char *source, size_t length)
{
    bool ok = ps_buffer_load_text(runtime->parser->lexer[0]->buffer, source, length);
    return ok;
}

/******************************************************************************/
/* FUNCTIONS                                                                  */
/******************************************************************************/

/** @brief Get absolute value of integer / unsigned / real */
ps_value *ps_runtime_func_abs(ps_runtime *runtime, ps_value *value)
{
    if (!ps_value_is_number(value))
    {
        runtime->error = PS_RUNTIME_ERROR_EXPECTED_NUMBER;
        return NULL;
    }
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
    {
        runtime->error = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
        return NULL;
    }
    memcpy(result, value, sizeof(ps_value));
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        // abs(u) => u
        return result;
    case PS_TYPE_INTEGER:
        result->data.i = abs(value->data.i);
        return result;
    case PS_TYPE_REAL:
        result->data.r = fabs(value->data.r);
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_EXPECTED_NUMBER;
        return NULL;
    }
}

/** @brief true if integer/unsigned value is odd, false if even */
ps_value *ps_runtime_func_odd(ps_runtime *runtime, ps_value *value)
{
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
        return NULL;
    result->type = PS_TYPE_BOOLEAN;
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        result->data.b = (ps_boolean)(value->data.u & 1 == 1);
        return result;
    case PS_TYPE_INTEGER:
        result->data.b = (ps_boolean)(value->data.i & 1 == 1);
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_EXPECTED_INTEGER_OR_UNSIGNED;
        return NULL;
    }
}

/** @brief true if integer/unsigned value is even, false if odd */
ps_value *ps_runtime_func_even(ps_runtime *runtime, ps_value *value)
{
    ps_value *result = ps_runtime_func_odd(runtime, value);
    if (result == NULL)
        return NULL;
    result->data.b = !result->data.b;
    return result;
}

/** @brief Get ordinal value of boolean / char */
ps_value *ps_runtime_func_ord(ps_runtime *runtime, ps_value *value)
{
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
        return NULL;
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
    case PS_TYPE_INTEGER:
        // case PS_TYPE_ENUM:
        // case PS_TYPE_SUBRANGE:
        // just copy: ord(x) => x
        memcpy(result, value, sizeof(ps_value));
        return result;
    case PS_TYPE_BOOLEAN:
        // ord(true) => 1 / ord(false) => 0
        result->type = PS_TYPE_UNSIGNED;
        result->data.u = value->data.b ? 1u : 0u;
        return result;
    case PS_TYPE_CHAR:
        // ord('0') => 48 / ord('A') => 65 / ...
        result->type = PS_TYPE_UNSIGNED;
        result->data.u = (ps_unsigned)(value->data.c);
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return NULL;
    }
}

/** @brief Get char value of unsigned / integer or subrange value */
ps_value *ps_runtime_func_chr(ps_runtime *runtime, ps_value *value)
{
    if (!ps_value_is_scalar(value))
    {
        runtime->error = PS_RUNTIME_ERROR_EXPECTED_SCALAR;
        return NULL;
    }
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
        return NULL;
    result->type->base = PS_TYPE_CHAR;
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        // case PS_TYPE_ENUM:
        result->data.c = (ps_char)(value->data.u);

    case PS_TYPE_INTEGER:
        // case PS_TYPE_SUBRANGE:
        result->data.c = (ps_char)(value->data.i);
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return NULL;
    }
    return result;
}

/** @brief Get previous value (predecessor) of scalar value */
ps_value *ps_runtime_func_pred(ps_runtime *runtime, ps_value *value)
{
    if (value == NULL)
    {
        runtime->error = PS_RUNTIME_ERROR_EXPECTED_VALUE;
        return NULL;
    }
    if (!ps_value_is_scalar(value))
    {
        runtime->error = PS_RUNTIME_ERROR_TYPE_MISMATCH;
        return NULL;
    }
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
        return NULL;
    memcpy(result, value, sizeof(ps_value));
    switch (value->type->base)
    {
    case PS_TYPE_INTEGER:
        // succ(min) => error / succ(i) => i - 1
        if (runtime->range_check && value->data.i == ps_integer_min)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.i = value->data.i - 1;
        return result;
    case PS_TYPE_UNSIGNED:
        // pred(0) => error / pred(u) => u - 1
        if (runtime->range_check && value->data.u == 0)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.u = value->data.u - 1;
        return result;
    // case PS_TYPE_ENUM:
    //   TODO needs low()
    // case PS_TYPE_SUBRANGE:
    //   TODO needs low()
    case PS_TYPE_BOOLEAN:
        // succ(true) => false / succ(false) => error
        if (runtime->range_check && value->data.b == false)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.b = false;
        return result;
    case PS_TYPE_CHAR:
        // succ(NUL) => error / succ(c) => c - 1
        if (runtime->range_check && value->data.c == 0)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.c = value->data.c - 1;
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return NULL;
    }
}

/** @brief Get next value (successor) of ordinal value */
ps_value *ps_runtime_func_succ(ps_runtime *runtime, ps_value *value)
{
    ps_value *result = ps_runtime_alloc_value(runtime);
    if (result == NULL)
        return NULL;
    memcpy(result, value, sizeof(ps_value));
    switch (value->type->base)
    {
    case PS_TYPE_UNSIGNED:
        // succ(max) => error / succ(u) => u + 1
        if (runtime->range_check && value->data.u == ps_unsigned_max)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.u = value->data.u + 1;
        return result;
    // case PS_TYPE_ENUM:
    //   TODO needs high()
    case PS_TYPE_INTEGER:
        // succ(max) => error / succ(i) => i + 1
        if (runtime->range_check && value->data.u == ps_integer_max)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.i = value->data.i + 1;
        return result;
    // case PS_TYPE_SUBRANGE:
    //   TODO needs high()
    case PS_TYPE_BOOLEAN:
        // succ(true) => error / succ(false) => true
        if (runtime->range_check && value->data.b == true)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.b = true;
        return result;
    case PS_TYPE_CHAR:
        // succ(char_max) => error / succ(c) => c + 1
        if (runtime->range_check && value->data.c == ps_char_max)
        {
            free(result);
            runtime->error = PS_RUNTIME_ERROR_OUT_OF_RANGE;
            return NULL;
        }
        result->data.c = value->data.c + 1;
        return result;
    default:
        free(result);
        runtime->error = PS_RUNTIME_ERROR_UNEXPECTED_TYPE;
        return NULL;
    }
}
