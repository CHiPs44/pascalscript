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

ps_interpreter *ps_interpreter_alloc(bool range_check, bool bool_eval, bool io_check)
{
    ps_interpreter *interpreter = ps_memory_malloc(PS_MEMORY_INTERPRETER, sizeof(ps_interpreter));
    if (interpreter == NULL)
        return NULL;
    // Set default state
    for (size_t i = 0; i < PS_INTERPRETER_ENVIRONMENTS; i++)
        interpreter->environments[i] = NULL;
    interpreter->level = PS_INTERPRETER_ENVIRONMENT_SYSTEM;
    interpreter->error = PS_ERROR_NONE;
    interpreter->message[0] = '\0';
    interpreter->debug = DEBUG_NONE;
    interpreter->range_check = range_check;
    interpreter->bool_eval = bool_eval;
    interpreter->io_check = io_check;
    // Allocate string heap
    interpreter->string_heap = ps_string_heap_alloc(PS_STRING_HEAP_SIZE, PS_STRING_HEAP_MORE);
    if (interpreter->string_heap == NULL)
        return ps_interpreter_free(interpreter);
    // Allocate system environment
    ps_identifier system = "SYSTEM";
    interpreter->environments[0] =
        ps_environment_alloc(NULL, system, PS_SYSTEM_SYMBOL_TABLE_SIZE, PS_SYSTEM_SYMBOL_TABLE_MORE);
    if (interpreter->environments[PS_INTERPRETER_ENVIRONMENT_SYSTEM] == NULL)
        return ps_interpreter_free(interpreter);
    // Initialize system environment
    if (!ps_system_init(interpreter->environments[PS_INTERPRETER_ENVIRONMENT_SYSTEM]))
        return ps_interpreter_free(interpreter);
    if (!ps_procedures_init(interpreter->environments[PS_INTERPRETER_ENVIRONMENT_SYSTEM]))
        return ps_interpreter_free(interpreter);
    if (!ps_functions_init(interpreter->environments[PS_INTERPRETER_ENVIRONMENT_SYSTEM]))
        return ps_interpreter_free(interpreter);
    return interpreter;
}

ps_interpreter *ps_interpreter_free(ps_interpreter *interpreter)
{
    if (interpreter != NULL)
    {
        if (interpreter->string_heap != NULL)
            interpreter->string_heap = ps_string_heap_free(interpreter->string_heap);
        ps_system_done(interpreter->environments[PS_INTERPRETER_ENVIRONMENT_SYSTEM]);
        for (size_t i = 0; i < PS_INTERPRETER_ENVIRONMENTS; i++)
        {
            if (interpreter->environments[i] != NULL)
                interpreter->environments[i] = ps_environment_free(interpreter->environments[i]);
        }
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

bool ps_interpreter_set_message(ps_interpreter *interpreter, char *format, ...) // NOSONAR
{
    assert(NULL != interpreter);
    assert(NULL != format);
    va_list args;
    va_start(args, format);
    vsnprintf(interpreter->message, sizeof(interpreter->message), format, args); // NOSONAR
    va_end(args);
    return true;
}

bool ps_interpreter_enter_environment(ps_interpreter *interpreter, ps_identifier name, ps_symbol_table *symbols,
                                      size_t n_values, ps_value *values)
{
    (void)n_values;
    (void)values;
    assert(NULL != interpreter);
    if (interpreter->level >= PS_INTERPRETER_ENVIRONMENTS - 1)
        return ps_interpreter_return_false(interpreter, PS_ERROR_ENVIRONMENT_OVERFLOW);
    ps_environment *parent = interpreter->environments[interpreter->level];
    ps_environment *environment = ps_environment_alloc(parent, name, symbols == NULL ? 0 : symbols->size, 0);
    if (environment == NULL)
        return ps_interpreter_return_false(interpreter, PS_ERROR_OUT_OF_MEMORY);
    // for (size_t i = 0; i < n_values; i++)
    // {
    //     ps_symbol *variable = symbols->symbols[i];
    //     if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
    //         continue;
    //     variable->value = &values[i];
    //     if (!ps_value_init(variable->value, variable->type->value->data.t))
    //     {
    //         return ps_interpreter_return_false(interpreter, PS_ERROR_OUT_OF_MEMORY);
    //     }
    // }
    interpreter->level += 1;
    interpreter->environments[interpreter->level] = environment;
    if (interpreter->debug >= DEBUG_VERBOSE)
    {
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
        fprintf(stderr, "=> ENTER ENVIRONMENT %d/%d '%s'\n", interpreter->level, PS_INTERPRETER_ENVIRONMENTS, name);
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
    }
    return true;
}

bool ps_interpreter_exit_environment(ps_interpreter *interpreter)
{
    assert(NULL != interpreter);
    ps_environment *environment = interpreter->environments[interpreter->level];
    if (environment == NULL)
        return ps_interpreter_return_false(interpreter, PS_ERROR_ENVIRONMENT_NOT_FOUND);
    if (interpreter->debug >= DEBUG_VERBOSE)
    {
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
        fprintf(stderr, "=> EXIT ENVIRONMENT %d/%d '%s'\n", interpreter->level, PS_INTERPRETER_ENVIRONMENTS,
                environment->name);
        ps_symbol_table_dump(NULL, "EXIT ENVIRONMENT", environment->symbols);
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
    }
    if (interpreter->level <= PS_INTERPRETER_ENVIRONMENT_SYSTEM)
        return ps_interpreter_return_false(interpreter, PS_ERROR_ENVIRONMENT_UNDERFLOW);
    interpreter->environments[interpreter->level] = ps_environment_free(environment);
    interpreter->level -= 1;
    return true;
}

ps_environment *ps_interpreter_get_environment(ps_interpreter *interpreter)
{
    if (interpreter->level <= PS_INTERPRETER_ENVIRONMENT_SYSTEM || interpreter->level >= PS_INTERPRETER_ENVIRONMENTS)
    {
        ps_interpreter_set_message(interpreter, "Invalid environment level %d", interpreter->level);
        return NULL;
    }
    return interpreter->environments[interpreter->level];
}

ps_symbol *ps_interpreter_find_symbol(ps_interpreter *interpreter, const char *name, bool local)
{
    ps_environment *environment = ps_interpreter_get_environment(interpreter);
    if (environment == NULL)
        ps_interpreter_return_null(interpreter, PS_ERROR_ENVIRONMENT_NOT_FOUND);
    ps_symbol *symbol = ps_environment_find_symbol(environment, name, local);
    if (interpreter->debug >= DEBUG_VERBOSE)
        fprintf(stderr, " DEBUG\tps_interpreter_find_symbol('%s', '%s', %s) => '%s'\n", environment->name, name,
                local ? "Local" : "Global", symbol == NULL ? "Not found" : symbol->name);
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

bool ps_interpreter_copy_value(ps_interpreter *interpreter, ps_value *from, ps_value *to) // NOSONAR
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
