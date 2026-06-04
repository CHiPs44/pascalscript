/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <string.h>

#include "ps_array.h"
#include "ps_error.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_memory.h"
#include "ps_procedures.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_value.h"

ps_interpreter *ps_interpreter_alloc(ps_symbol_table *system, ps_string_heap *string_heap, bool range_check,
                                     bool bool_eval, bool io_check)
{
    ps_interpreter *interpreter = ps_memory_malloc(PS_MEMORY_INTERPRETER, sizeof(ps_interpreter));
    if (interpreter == NULL)
        return NULL;
    interpreter->system = system;
    interpreter->string_heap = string_heap;
    interpreter->level = 0;
    interpreter->error = PS_ERROR_NONE;
    memset(interpreter->message, 0, sizeof(interpreter->message));
    interpreter->debug = DEBUG_NONE;
    interpreter->range_check = range_check;
    interpreter->bool_eval = bool_eval;
    interpreter->io_check = io_check;
    return interpreter;
}

ps_interpreter *ps_interpreter_free(ps_interpreter *interpreter)
{
    if (interpreter != NULL)
    {
        if (interpreter->string_heap != NULL)
            interpreter->string_heap = ps_string_heap_free(interpreter->string_heap);
        ps_memory_free(PS_MEMORY_INTERPRETER, interpreter);
    }
    return NULL;
}

bool ps_interpreter_return_false(ps_interpreter *interpreter, ps_error error)
{
    assert(NULL != interpreter);
    interpreter->error = error;
    return false;
}

void *ps_interpreter_return_null(ps_interpreter *interpreter, ps_error error)
{
    assert(NULL != interpreter);
    interpreter->error = error;
    return NULL;
}

bool ps_interpreter_set_message(ps_interpreter *interpreter, const char *format, ...) // NOSONAR
{
    assert(NULL != interpreter);
    assert(NULL != format);
    va_list args;
    va_start(args, format);
    vsnprintf(interpreter->message, sizeof(interpreter->message) - 1, format, args); // NOSONAR
    va_end(args);
    return true;
}

bool ps_interpreter_enter_environment(ps_interpreter *interpreter, const ps_identifier name, ps_symbol_table *symbols,
                                      size_t n_values, ps_value *values)
{
    (void)n_values;
    (void)values;
    assert(NULL != interpreter);
    interpreter->level += 1;
    return true;
}

bool ps_interpreter_exit_environment(ps_interpreter *interpreter)
{
    assert(NULL != interpreter);
    interpreter->level -= 1;
    return true;
}

ps_symbol *ps_interpreter_find_symbol(ps_interpreter *interpreter, ps_ast_block *block, const char *name, bool local)
{
    assert(interpreter != NULL);
    assert(name != NULL);
    ps_symbol *symbol = NULL;

    // No block => search into SYSTEM
    if (block == NULL)
    {
        symbol = ps_symbol_table_get(interpreter->system, name);
        if (interpreter->debug >= DEBUG_VERBOSE)
            fprintf(stderr, " DEBUG\tps_interpreter_find_symbol('%s', '%s', %s) => '%s'\n", "SYSTEM", name,
                    local ? "Local" : "Global", symbol == NULL ? "Not found" : symbol->name);
    }
    else
    {
        // Search in current block
        symbol = ps_symbol_table_get(block->symbols, name);
        if (!local && symbol == NULL)
            // Not found => search in parent
            return ps_interpreter_find_symbol(interpreter, block->parent, name, false);
        if (interpreter->debug >= DEBUG_VERBOSE)
            fprintf(stderr, " DEBUG\tps_interpreter_find_symbol('%s', '%s', %s) => '%s'\n", block->name, name,
                    local ? "Local" : "Global", symbol == NULL ? "Not found" : symbol->name);
    }

    return symbol;
}

bool ps_interpreter_add_symbol(ps_interpreter *interpreter, ps_symbol *symbol)
{
    assert(NULL != interpreter);
    assert(NULL != symbol);
    ps_environment *environment = ps_interpreter_get_environment(interpreter);
    if (environment == NULL)
        ps_interpreter_return_false(interpreter, PS_ERROR_ENVIRONMENT_NOT_FOUND);
    if (interpreter->debug >= DEBUG_TRACE)
        fprintf(stderr, "ADD %*s SYMBOL '%*s' TO ENVIRONMENT '%*s' with value %p: '%s'\n", -10,
                ps_symbol_get_kind_name(symbol->kind), -(int)PS_IDENTIFIER_LEN, symbol->name, -(int)PS_IDENTIFIER_LEN,
                environment->name, (void *)(symbol->value),
                symbol->value == NULL ? "NULL" : ps_value_get_debug_string(symbol->value));
    if (!ps_environment_add_symbol(environment, symbol))
        return ps_interpreter_return_false(interpreter, PS_ERROR_SYMBOL_NOT_ADDED);
    return true;
}

bool ps_interpreter_add_variable(ps_interpreter *interpreter, const ps_identifier identifier, ps_symbol *type_symbol)
{
    assert(NULL != interpreter);
    assert('\0' != identifier[0]);
    assert(NULL != type_symbol);
    assert(NULL != type_symbol->value);
    ps_value_data data = {0};
    if (ps_type_definition_is_array(type_symbol->value->data.t))
    {
        data.a = ps_array_alloc_data(type_symbol);
        if (data.a == NULL)
            return ps_interpreter_return_false(interpreter, PS_ERROR_OUT_OF_MEMORY);
    }
    ps_value *value = ps_value_alloc(type_symbol, data);
    ps_symbol *variable = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, identifier, value);
    if (variable == NULL)
    {
        if (ps_type_definition_is_array(type_symbol->value->data.t))
            ps_memory_free(PS_MEMORY_VALUE, data.a);
        return ps_interpreter_return_false(interpreter, PS_ERROR_OUT_OF_MEMORY);
    }
    return ps_interpreter_add_symbol(interpreter, variable);
}

bool ps_interpreter_copy_value(ps_interpreter *interpreter, const ps_value *from, ps_value *to) // NOSONAR
{
    assert(NULL != interpreter);
    assert(NULL != from);
    assert(NULL != to);
    if (interpreter->debug >= DEBUG_VERBOSE)
    {
        fprintf(stderr, ">");
        ps_value_debug(stderr, "FROM\t", from);
        fprintf(stderr, ">");
        ps_value_debug(stderr, "TO\t", to);
    }
    ps_error error = ps_value_copy(from, to, interpreter->range_check);
    if (error == PS_ERROR_NONE)
    {
        if (interpreter->debug >= DEBUG_VERBOSE)
        {
            fprintf(stderr, "*");
            ps_value_debug(stderr, "TO\t", to);
        }
        return true;
    }
    if (error != PS_ERROR_TYPE_MISMATCH)
        return ps_interpreter_return_false(interpreter, error);
    ps_interpreter_set_message(
        interpreter, "Cannot convert value from type '%s' (based on '%s') to type '%s' (based on '%s')",
        ps_value_type_get_name(from->type->value->data.t->type), ps_value_type_get_name(ps_value_get_base(from)),
        ps_value_type_get_name(ps_value_get_type(to)), ps_value_type_get_name(ps_value_get_base(to)));
    return ps_interpreter_return_false(interpreter, PS_ERROR_TYPE_MISMATCH);
}
