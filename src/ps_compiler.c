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
#include "ps_parse.h"
#include "ps_parse_declaration.h"
#include "ps_parser.h"
#include "ps_procedures.h"
#include "ps_string_heap.h"
#include "ps_system.h"
#include "ps_value.h"

ps_compiler *ps_compiler_alloc(ps_ast_block *system)
{
    // Allocate compiler itself
    ps_compiler *compiler = ps_memory_malloc(PS_MEMORY_COMPILER, sizeof(ps_compiler));

    // Initialize compiler
    compiler->error = PS_ERROR_NONE;
    compiler->debug = PS_DEBUG_FATAL;
    memset(compiler->message, 0, sizeof(compiler->message));
    compiler->parser = NULL;
    compiler->string_heap = NULL;
    compiler->system = system;

    // Allocate parser
    compiler->parser = ps_parser_alloc();
    if (compiler->parser == NULL)
        goto cleanup;

    // Allocate string heap
    compiler->string_heap = ps_string_heap_alloc(PS_STRING_HEAP_SIZE, PS_STRING_HEAP_MORE);
    if (compiler->string_heap == NULL)
        goto cleanup;

    return compiler;
cleanup:
    return ps_compiler_free(compiler);
}

ps_compiler *ps_compiler_free(ps_compiler *compiler)
{
    if (compiler != NULL)
    {
        if (compiler->parser != NULL)
            compiler->parser = ps_parser_free(compiler->parser);
        if (compiler->string_heap != NULL)
            compiler->string_heap = ps_string_heap_free(compiler->string_heap);
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
        symbol = ps_symbol_table_get(compiler->system->symbols, name);
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
        fprintf(stderr, "ADD %*s SYMBOL '%*s' TO BLOCK '%*s' with value '%s'\n", -10,
                ps_symbol_get_kind_name(symbol->kind), -(int)PS_IDENTIFIER_LEN, symbol->name, -(int)PS_IDENTIFIER_LEN,
                block->name, symbol->value == NULL ? "NULL" : ps_value_get_debug_string(symbol->value));
    ps_error error = ps_symbol_table_add(block->symbols, symbol);
    if (error != PS_ERROR_NONE)
        return ps_compiler_set_error_message(compiler, error, "Cannot add symbol '%s' to block '%s': %s", symbol->name,
                                             block->name, ps_error_get_message(error));
    return true;
}

bool ps_compiler_add_variable(ps_compiler *compiler, ps_ast_block *block, const ps_identifier identifier,
                              ps_symbol *type_symbol)
{
    assert(compiler != NULL);
    assert(block != NULL);
    assert(type_symbol != NULL);
    // Variable handle is its index in block variables
    ps_value_data data = {.h = (ps_handle)(block->n_vars++)};
    if (ps_type_definition_is_array(type_symbol->value->data.t))
    {
        return ps_compiler_set_error_message(compiler, PS_ERROR_NOT_IMPLEMENTED,
                                             "Array variables are not implemented yet");
    }
    ps_value *value = ps_value_alloc(type_symbol, data);
    ps_symbol *variable = ps_symbol_alloc(PS_SYMBOL_KIND_VARIABLE, identifier, value);
    if (variable == NULL)
    {
        // if (ps_type_definition_is_array(type_symbol->value->data.t))
        //     ps_memory_free(PS_MEMORY_VALUE, data.a);
        return ps_compiler_set_error_message(compiler, PS_ERROR_OUT_OF_MEMORY, "Cannot add variable %s", identifier);
    }
    return ps_compiler_add_symbol(compiler, block, variable);
}

bool ps_compiler_copy_value(ps_compiler *compiler, const ps_value *from, ps_value *to)
{
    if (compiler->debug >= PS_DEBUG_VERBOSE)
    {
        fprintf(stderr, ">");
        ps_value_debug(stderr, "FROM\t", from);
        fprintf(stderr, ">");
        ps_value_debug(stderr, "TO\t", to);
    }
    ps_error error = ps_value_copy(from, to, true);
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
    assert(NULL != source);
    compiler->error = PS_ERROR_NONE;
    ps_lexer *lexer = ps_parser_get_lexer(compiler->parser);
    return ps_buffer_load_string(lexer->buffer, source, length);
}

bool ps_compiler_load_file(ps_compiler *compiler, const char *filename)
{
    assert(NULL != compiler);
    compiler->error = PS_ERROR_NONE;
    ps_lexer *lexer = ps_parser_get_lexer(compiler->parser);
    return ps_buffer_load_file(lexer->buffer, filename);
}

bool ps_compiler_compile(ps_compiler *compiler, ps_ast_block **program)
{
    assert(compiler != NULL);
    ps_error error = PS_ERROR_NONE;
    if (compiler->parser == NULL)
        return ps_compiler_return_false(compiler, PS_ERROR_GENERIC);
    ps_lexer *lexer = ps_parser_get_lexer(compiler->parser);
    if (lexer == NULL)
        return ps_compiler_return_false(compiler, PS_ERROR_GENERIC);
    if (!ps_buffer_read_next_char(lexer->buffer))
    {
        error = lexer->error;
        if (error == PS_ERROR_NONE)
            error = PS_ERROR_GENERIC;
        return ps_compiler_return_false(compiler, error);
    }
    if (compiler->debug >= PS_DEBUG_INFO)
        fprintf(stderr, "*** Compilation of %s\n", lexer->buffer->from_file ? "file" : "string");
    if (!ps_lexer_read_token(lexer))
    {
        error = lexer->error;
        if (error == PS_ERROR_NONE)
            error = PS_ERROR_GENERIC;
        return ps_compiler_return_false(compiler, error);
    }
    ps_token_debug(stderr, "FIRST TOKEN\t", &lexer->current_token);
    *program = ps_ast_create_block(0, 0, NULL, PS_AST_PROGRAM, NULL);
    if (*program == NULL)
        return ps_compiler_return_false(compiler, PS_ERROR_OUT_OF_MEMORY);
    if (!ps_parse_program(compiler, *program))
    {
        error = compiler->parser->error;
        if (error == PS_ERROR_NONE)
            error = PS_ERROR_GENERIC;
        return ps_compiler_return_false(compiler, error);
    }
    return true;
}
