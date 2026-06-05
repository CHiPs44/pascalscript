/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <string.h>

#include "ps_ast.h"
#include "ps_compiler.h"
#include "ps_error.h"
#include "ps_functions.h"
#include "ps_memory.h"
#include "ps_parser.h"
#include "ps_procedures.h"
#include "ps_string_heap.h"
#include "ps_system.h"
#include "ps_value.h"

ps_compiler *ps_compiler_alloc(ps_symbol_table *system, bool range_check, bool bool_eval, bool io_check)
{
    // Allocate compiler itself
    ps_compiler *compiler = ps_memory_malloc(PS_MEMORY_COMPILER, sizeof(ps_compiler));
    compiler->parser = NULL;
    compiler->error = PS_ERROR_NONE;
    compiler->debug = PS_DEBUG_FATAL;
    compiler->range_check = range_check;
    compiler->bool_eval = bool_eval;
    compiler->io_check = io_check;
    compiler->system = NULL;
    // Allocate string heap
    compiler->string_heap = ps_string_heap_alloc(PS_STRING_HEAP_SIZE, PS_STRING_HEAP_MORE);
    if (compiler->string_heap == NULL)
        return ps_compiler_free(compiler);
    // Allocate and initialize system environment
    compiler->system = ps_symbol_table_alloc(256, 0);
    if (compiler->system == NULL)
        return ps_compiler_free(compiler);
    compiler->system = system;
    return compiler;
}

ps_compiler *ps_compiler_free(ps_compiler *compiler)
{
    if (compiler != NULL)
    {
        if (compiler->string_heap != NULL)
            compiler->string_heap = ps_string_heap_free(compiler->string_heap);
        if (compiler->system != NULL)
        {
            ps_system_done(compiler->system);
            compiler->system = ps_symbol_table_free(compiler->system);
        }
        ps_memory_free(PS_MEMORY_COMPILER, compiler);
    }
    return NULL;
}

bool ps_compiler_return_false(ps_compiler *compiler, ps_error error)
{
    assert(compiler != NULL);
    compiler->error = error;
    return false;
}

void *ps_compiler_return_null(ps_compiler *compiler, ps_error error)
{
    assert(compiler != NULL);
    compiler->error = error;
    return NULL;
}

bool ps_compiler_set_message(ps_compiler *compiler, const char *format, ...) // NOSONAR
{
    assert(compiler != NULL);
    assert(format != NULL);
    va_list args;
    va_start(args, format);
    vsnprintf(compiler->message, sizeof(compiler->message) - 1, format, args); // NOSONAR
    va_end(args);
    return false;
}

bool ps_compiler_set_error_message(ps_compiler *compiler, ps_error error, const char *format, ...) // NOSONAR
{
    assert(compiler != NULL);
    assert(format != NULL);
    compiler->error = error;
    va_list args;
    va_start(args, format);
    vsnprintf(compiler->message, sizeof(compiler->message) - 1, format, args); // NOSONAR
    va_end(args);
    return false;
}

ps_symbol *ps_compiler_find_symbol(ps_compiler *compiler, ps_ast_block *block, const char *name, bool local)
{
    assert(compiler != NULL);
    assert(name != NULL);
    ps_symbol *symbol = NULL;

    // No block => search into SYSTEM
    if (block == NULL)
    {
        symbol = ps_symbol_table_get(compiler->system, name);
        if (compiler->debug >= PS_DEBUG_VERBOSE)
            fprintf(stderr, " DEBUG\tps_compiler_find_symbol('%s', '%s', %s) => '%s'\n", "SYSTEM", name,
                    local ? "Local" : "Global", symbol == NULL ? "Not found" : symbol->name);
    }
    else
    {
        // Search in current block
        symbol = ps_symbol_table_get(block->symbols, name);
        if (!local && symbol == NULL)
            // Not found => search in parent
            return ps_compiler_find_symbol(compiler, block->parent, name, false);
        if (compiler->debug >= PS_DEBUG_VERBOSE)
            fprintf(stderr, " DEBUG\tps_compiler_find_symbol('%s', '%s', %s) => '%s'\n", block->name, name,
                    local ? "Local" : "Global", symbol == NULL ? "Not found" : symbol->name);
    }

    return symbol;
}

bool ps_compiler_add_symbol(ps_compiler *compiler, ps_ast_block *block, ps_symbol *symbol)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(symbol != NULL);
    if (compiler->debug >= PS_DEBUG_TRACE)
        fprintf(stderr, "ADD %*s SYMBOL '%*s' TO BLOCK '%*s' with value %p: '%s'\n", -10,
                ps_symbol_get_kind_name(symbol->kind), -(int)PS_IDENTIFIER_LEN, symbol->name, -(int)PS_IDENTIFIER_LEN,
                block->name, (void *)(symbol->value),
                symbol->value == NULL ? "NULL" : ps_value_get_debug_string(symbol->value));
    ps_error error = ps_symbol_table_add(block->symbols, symbol);
    if (error != PS_ERROR_NONE)
        return ps_compiler_return_false(compiler, PS_ERROR_SYMBOL_NOT_ADDED);
    return true;
}

bool ps_compiler_add_variable(ps_compiler *compiler, ps_ast_block *block, const ps_identifier identifier,
                              ps_symbol *type_symbol)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(type_symbol != NULL);
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
    return ps_compiler_add_symbol(compiler, block, variable);
}

bool ps_compiler_is_number(ps_compiler *compiler, ps_value *value)
{
    assert(compiler != NULL);
    assert(value != NULL);
    (void)compiler;
    return ps_value_is_number(value);
}

bool ps_compiler_is_ordinal(ps_compiler *compiler, ps_value *value)
{
    assert(compiler != NULL);
    assert(value != NULL);
    (void)compiler;
    return ps_value_is_ordinal(value);
}

bool ps_compiler_copy_value(ps_compiler *compiler, ps_value *from, ps_value *to)
{
    if (compiler->debug >= PS_DEBUG_VERBOSE)
    {
        fprintf(stderr, ">");
        ps_value_debug(stderr, "FROM\t", from);
        fprintf(stderr, ">");
        ps_value_debug(stderr, "TO\t", to);
    }
    ps_error error = ps_value_copy(from, to, compiler->range_check);
    if (error == PS_ERROR_NONE)
    {
        if (compiler->debug >= PS_DEBUG_VERBOSE)
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
    compiler->error = PS_ERROR_NOT_IMPLEMENTED;
    return false;
}

bool ps_compiler_load_file(ps_compiler *compiler, const char *filename)
{
    (void)compiler;
    (void)filename;
    compiler->error = PS_ERROR_NOT_IMPLEMENTED;
    return false;
}

bool ps_compiler_run(ps_compiler *compiler)
{
    (void)compiler;
    compiler->error = PS_ERROR_NOT_IMPLEMENTED;
    return false;
}
