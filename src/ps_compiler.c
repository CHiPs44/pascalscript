/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_compiler.h"
#include "ps_environment.h"
#include "ps_error.h"
#include "ps_functions.h"
#include "ps_memory.h"
#include "ps_parser.h"
#include "ps_procedures.h"
#include "ps_string_heap.h"
#include "ps_system.h"
#include "ps_value.h"

ps_compiler *ps_compiler_alloc(bool range_check, bool bool_eval, bool io_check)
{
    ps_compiler *compiler = ps_memory_malloc(PS_MEMORY_COMPILER, sizeof(ps_compiler));
    compiler->parser = NULL;
    compiler->string_heap = NULL;
    compiler->level = 0;
    compiler->error = PS_ERROR_NONE;
    compiler->debug = COMPILER_DEBUG_NONE;
    compiler->range_check = range_check;
    compiler->bool_eval = bool_eval;
    compiler->io_check = io_check;
    // Allocate string heap
    compiler->string_heap = ps_string_heap_alloc(PS_STRING_HEAP_SIZE, PS_STRING_HEAP_MORE);
    if (compiler->string_heap == NULL)
        return ps_compiler_free(compiler);
    // Allocate system environment
    ps_identifier system = "SYSTEM";
    compiler->environments[0] =
        ps_environment_alloc(NULL, system, PS_SYSTEM_SYMBOL_TABLE_SIZE, PS_SYSTEM_SYMBOL_TABLE_MORE);
    if (compiler->environments[PS_COMPILER_ENVIRONMENT_SYSTEM] == NULL)
        return ps_compiler_free(compiler);
    // Initialize system environment
    if (!ps_system_init(compiler->environments[PS_COMPILER_ENVIRONMENT_SYSTEM]))
        return ps_compiler_free(compiler);
    if (!ps_procedures_init(compiler->environments[PS_COMPILER_ENVIRONMENT_SYSTEM]))
        return ps_compiler_free(compiler);
    if (!ps_functions_init(compiler->environments[PS_COMPILER_ENVIRONMENT_SYSTEM]))
        return ps_compiler_free(compiler);
    return compiler;
}

ps_compiler *ps_compiler_free(ps_compiler *compiler)
{
    if (compiler != NULL)
    {
        if (compiler->string_heap != NULL)
            compiler->string_heap = ps_string_heap_free(compiler->string_heap);
        ps_system_done(compiler->environments[PS_COMPILER_ENVIRONMENT_SYSTEM]);
        for (size_t i = 0; i < PS_COMPILER_ENVIRONMENTS; i++)
        {
            if (compiler->environments[i] != NULL)
                compiler->environments[i] = ps_environment_free(compiler->environments[i]);
        }
        ps_memory_free(PS_MEMORY_COMPILER, compiler);
    }
    return NULL;
}

bool ps_compiler_return_false(ps_compiler *compiler, ps_error error)
{
    compiler->error = error;
    return false;
}

void *ps_compiler_return_null(ps_compiler *compiler, ps_error error)
{
    compiler->error = error;
    return NULL;
}

bool ps_compiler_set_message(ps_compiler *compiler, char *format, ...) // NOSONAR
{
    va_list args;
    va_start(args, format);
    vsnprintf(compiler->message, sizeof(compiler->message), format, args); // NOSONAR
    va_end(args);
    return true;
}

bool ps_compiler_enter_environment(ps_compiler *compiler, ps_identifier name)
{
    if (compiler->level >= PS_COMPILER_ENVIRONMENTS - 1)
        return ps_compiler_return_false(compiler, PS_ERROR_ENVIRONMENT_OVERFLOW);
    ps_environment *parent = compiler->environments[compiler->level];
    ps_environment *environment = ps_environment_alloc(parent, name, 0, 0);
    if (environment == NULL)
        return ps_compiler_return_false(compiler, PS_ERROR_OUT_OF_MEMORY);
    compiler->level += 1;
    compiler->environments[compiler->level] = environment;
    if (compiler->debug >= COMPILER_DEBUG_VERBOSE)
    {
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
        fprintf(stderr, "=> ENTER ENVIRONMENT %d/%d '%s'\n", compiler->level, PS_COMPILER_ENVIRONMENTS, name);
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
    }
    return true;
}

bool ps_compiler_exit_environment(ps_compiler *compiler)
{
    ps_environment *environment = compiler->environments[compiler->level];
    if (environment == NULL)
        return ps_compiler_return_false(compiler, PS_ERROR_ENVIRONMENT_NOT_FOUND);
    if (compiler->debug >= COMPILER_DEBUG_VERBOSE)
    {
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
        fprintf(stderr, "=> EXIT ENVIRONMENT %d/%d '%s'\n", compiler->level, PS_COMPILER_ENVIRONMENTS,
                environment->name);
        ps_symbol_table_dump(NULL, "EXIT ENVIRONMENT", environment->symbols);
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
    }
    if (compiler->level <= PS_COMPILER_ENVIRONMENT_SYSTEM)
        return ps_compiler_return_false(compiler, PS_ERROR_ENVIRONMENT_UNDERFLOW);
    compiler->environments[compiler->level] = ps_environment_free(environment);
    compiler->level -= 1;
    return true;
}

ps_environment *ps_compiler_get_environment(ps_compiler *compiler)
{
    if (compiler->level <= PS_COMPILER_ENVIRONMENT_SYSTEM || compiler->level >= PS_COMPILER_ENVIRONMENTS)
    {
        ps_compiler_set_message(compiler, "Invalid environment level %d", compiler->level);
        return NULL;
    }
    return compiler->environments[compiler->level];
}

ps_symbol *ps_compiler_find_symbol(ps_compiler *compiler, const char *name, bool local)
{
    ps_environment *environment = ps_compiler_get_environment(compiler);
    if (environment == NULL)
        ps_compiler_return_null(compiler, PS_ERROR_ENVIRONMENT_NOT_FOUND);
    ps_symbol *symbol = ps_environment_find_symbol(environment, name, local);
    if (compiler->debug >= COMPILER_DEBUG_VERBOSE)
        fprintf(stderr, " DEBUG\tps_compiler_find_symbol('%s', '%s', %s) => '%s'\n", environment->name, name,
                local ? "Local" : "Global", symbol == NULL ? "Not found" : symbol->name);
    return symbol;
}

bool ps_compiler_add_symbol(ps_compiler *compiler, ps_symbol *symbol)
{
    ps_environment *environment = ps_compiler_get_environment(compiler);
    if (environment == NULL)
        ps_compiler_return_false(compiler, PS_ERROR_ENVIRONMENT_NOT_FOUND);
    if (compiler->debug >= COMPILER_DEBUG_TRACE)
        fprintf(stderr, "ADD %*s SYMBOL '%*s' TO ENVIRONMENT '%*s' with value %p: '%s'\n", -10,
                ps_symbol_get_kind_name(symbol->kind), -(int)PS_IDENTIFIER_LEN, symbol->name, -(int)PS_IDENTIFIER_LEN,
                environment->name, (void *)(symbol->value),
                symbol->value == NULL ? "NULL" : ps_value_get_debug_string(symbol->value));
    if (!ps_environment_add_symbol(environment, symbol))
        return ps_compiler_return_false(compiler, PS_ERROR_SYMBOL_NOT_ADDED);
    return true;
}

bool ps_compiler_add_variable(ps_compiler *compiler, const ps_identifier identifier, ps_symbol *type_symbol)
{
    ps_value_data data = {0};
    if (ps_type_definition_is_array(type_symbol->value->data.t))
    {
        return ps_compiler_return_false(compiler, PS_ERROR_NOT_IMPLEMENTED);
        // data.a = ps_array_alloc_data(type_symbol);
        // if (data.a == NULL)
        //     return ps_compiler_return_false(compiler, PS_ERROR_OUT_OF_MEMORY);
    }
    ps_value *value = ps_value_alloc(type_symbol, data);
    ps_symbol *variable = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, identifier, value);
    if (variable == NULL)
    {
        if (ps_type_definition_is_array(type_symbol->value->data.t))
            ps_memory_free(PS_MEMORY_VALUE, data.a);
        return ps_compiler_return_false(compiler, PS_ERROR_OUT_OF_MEMORY);
    }
    return ps_compiler_add_symbol(compiler, variable);
}

bool ps_compiler_is_number(ps_compiler *compiler, ps_value *value)
{
    (void)compiler;
    (void)value;
    return false;
}

bool ps_compiler_is_ordinal(ps_compiler *compiler, ps_value *value)
{
    (void)compiler;
    (void)value;
    return false;
}

bool ps_compiler_copy_value(ps_compiler *compiler, ps_value *from, ps_value *to)
{
    if (compiler->debug >= COMPILER_DEBUG_VERBOSE)
    {
        fprintf(stderr, ">");
        ps_value_debug(stderr, "FROM\t", from);
        fprintf(stderr, ">");
        ps_value_debug(stderr, "TO\t", to);
    }
    ps_error error = ps_value_copy(from, to, compiler->range_check);
    if (error == PS_ERROR_NONE)
    {
        if (compiler->debug >= COMPILER_DEBUG_VERBOSE)
        {
            fprintf(stderr, "*");
            ps_value_debug(stderr, "TO\t", to);
        }
        return true;
    }
    if (error != PS_ERROR_TYPE_MISMATCH)
        return ps_compiler_return_false(compiler, error);
    ps_compiler_set_message(
        compiler, "Cannot convert value from type '%s' (based on '%s') to type '%s' (based on '%s')",
        ps_value_type_get_name(from->type->value->data.t->type), ps_value_type_get_name(ps_value_get_base(from)),
        ps_value_type_get_name(ps_value_get_type(to)), ps_value_type_get_name(ps_value_get_base(to)));
    return ps_compiler_return_false(compiler, PS_ERROR_TYPE_MISMATCH);
}

bool ps_compiler_load_string(ps_compiler *compiler, char *source, size_t length)
{
    (void)compiler;
    (void)source;
    (void)length;
    return false;
}

bool ps_compiler_load_file(ps_compiler *compiler, const char *filename)
{
    (void)compiler;
    (void)filename;
    return false;
}

bool ps_compiler_run(ps_compiler *compiler)
{
    (void)compiler;
    return false;
}
