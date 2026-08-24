/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <errno.h>
#include <string.h>

#include "ps_array.h"
#include "ps_ast.h"
#include "ps_ast_debug.h"
#include "ps_ast_execute.h"
#include "ps_error.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_logger.h"
#include "ps_memory.h"
#include "ps_procedures.h"
#include "ps_stack.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_type_definition.h"
#include "ps_value.h"

ps_interpreter *ps_interpreter_alloc(ps_ast_block *system, ps_string_heap *string_heap, bool range_check,
                                     bool bool_eval, bool io_check)
{
    assert(NULL != system);
    assert(NULL != string_heap);
    ps_interpreter *interpreter = ps_memory_malloc(PS_MEMORY_INTERPRETER, sizeof(ps_interpreter));
    if (interpreter == NULL)
        return NULL;
    // Initialize interpreter
    interpreter->system = system;
    interpreter->string_heap = string_heap;
    interpreter->level = 0;
    interpreter->error = PS_ERROR_NONE;
    memset(interpreter->message, 0, sizeof(interpreter->message));
    interpreter->error_line = 0;
    interpreter->error_column = 0;
    // Allocate logger
    interpreter->logger = ps_logger_alloc(stderr, PS_DEBUG_FATAL);
    if (interpreter->logger == NULL)
        return ps_interpreter_free(interpreter);
    interpreter->range_check = range_check;
    interpreter->bool_eval = bool_eval;
    interpreter->io_check = io_check;
    // Allocate stack
    interpreter->stack = ps_stack_alloc(PS_INTERPRETER_STACK_SIZE);
    if (interpreter->stack == NULL)
        return ps_interpreter_free(interpreter);
    // Allocate system variables
    ps_frame *frame = ps_frame_alloc(system);
    if (frame == NULL)
    {
        return ps_interpreter_free(interpreter); // errno = ENONEM;
    }
    for (ps_handle i = 0; i < system->n_vars; i++)
    {
        frame->data[i].v = 0;
    }
    // Push system frame on stack
    if (NULL == ps_stack_push(interpreter->stack, frame))
    {
        ps_frame_free(frame);
        return ps_interpreter_free(interpreter);
    }
    return interpreter;
}

ps_interpreter *ps_interpreter_free(ps_interpreter *interpreter)
{
    if (interpreter != NULL)
    {
        if (interpreter->logger != NULL)
            interpreter->logger = ps_logger_free(interpreter->logger);
        if (interpreter->stack != NULL)
            interpreter->stack = ps_stack_free(interpreter->stack);
        ps_memory_free(PS_MEMORY_INTERPRETER, interpreter);
    }
    return NULL;
}

void ps_interpreter_log(ps_interpreter *interpreter, ps_debug_level debug_level, const char *format, ...) // NOSONAR
{
    if (interpreter->logger->debug_level >= debug_level)
    {
        static char message[256];
        va_list args;
        va_start(args, format);
        vsnprintf(message, sizeof(message), format, args); // NOSONAR
        va_end(args);
        ps_log(interpreter->logger, debug_level, message);
    }
}

bool ps_interpreter_return_false(ps_interpreter *interpreter, ps_error error)
{
    assert(NULL != interpreter);
    interpreter->error = error;
    return false;
}

bool ps_interpreter_set_error_message(ps_interpreter *interpreter, ps_error error, const char *format, ...) // NOSONAR
{
    assert(NULL != interpreter);
    assert(NULL != format);
    interpreter->error = error;
    va_list args;
    va_start(args, format);
    vsnprintf(interpreter->message, sizeof(interpreter->message) - 1, format, args); // NOSONAR
    va_end(args);
    return false;
}

bool ps_interpreter_set_message(ps_interpreter *interpreter, const char *format, ...) // NOSONAR
{
    assert(NULL != interpreter);
    assert(NULL != format);
    va_list args;
    va_start(args, format);
    vsnprintf(interpreter->message, sizeof(interpreter->message) - 1, format, args); // NOSONAR
    va_end(args);
    return false;
}

bool ps_interpreter_allocate_variables(ps_interpreter *interpreter, ps_ast_block *block, ps_frame *frame)
{
    assert(NULL != interpreter);
    assert(NULL != block);
    assert(NULL != frame);

    // size_t count = block->n_vars;
    // if (block->signature != NULL)
    // {
    //     count += block->signature->parameter_count;
    //     if (block->signature->result_type != NULL)
    //         count += 1;
    // }

    for (int b = 0; b < block->symbols->table_size; b++)
    {
        // Empty bucket? => next
        if (block->symbols->buckets[b] == NULL)
            continue;
        for (int i = 0; i < block->symbols->buckets[b]->size; i++)
        {
            ps_symbol *symbol = block->symbols->buckets[b]->symbols[i];
            // Null or not a variable? => next
            if (symbol == NULL || symbol->kind != PS_SYMBOL_KIND_VARIABLE)
                continue;
            ps_handle handle = symbol->value->data.h;
            // Array? => allocate data
            ps_type_definition *type_def = ps_array_get_type_def(symbol);
            if (type_def != NULL)
            {
                // Allocate the array
                ps_array_data *data = ps_array_alloc_data(symbol);
                if (data == NULL)
                    return ps_interpreter_set_error_message(interpreter, PS_ERROR_OUT_OF_MEMORY,
                                                            "Could not allocate array data for %s", symbol->name);
                frame->data[handle].a = data;
                continue;
            }
            // String? => allocate data
            if (ps_value_is_string(symbol->value))
            {
                ps_string *data = ps_string_alloc(type_def->def.s.max);
                if (data == NULL)
                    return ps_interpreter_set_error_message(interpreter, PS_ERROR_OUT_OF_MEMORY,
                                                            "Could not allocate string data for %s", symbol->name);
                frame->data[handle].s = data;
            }
        }
    }

    // for (size_t i = 0; i < count; i++)
    // {
    //     ps_value_data *data = &frame->data[i];
    //     data->u = i % 2 == 0 ? 0xDEADDEAD : 0xBEEFBEEF;
    //     // if (block->variables[i].type->kind == PS_AST_TYPE_KIND_ARRAY)
    //     // {
    //     //    )

    return true;
}

bool ps_interpreter_enter_frame(ps_interpreter *interpreter, ps_ast_block *block)
{
    assert(NULL != interpreter);
    assert(NULL != block);

    // Increase interpreter's level
    interpreter->level += 1;
    ps_interpreter_log(interpreter, PS_DEBUG_INFO, "ENTER FRAME level=%d '%s' with %zu symbol%s\n", interpreter->level,
                       block->name, block->symbols == NULL ? 0 : block->symbols->used_buckets,
                       block->symbols != NULL && block->symbols->used_buckets > 1 ? "s" : "");

    // Allocate a new frame
    ps_frame *frame = ps_frame_alloc(block);
    if (frame == NULL)
        return ps_interpreter_return_false(interpreter, PS_ERROR_OUT_OF_MEMORY);

    // Allocate non simple variables: arrays, strings, records, etc.
    if (!ps_interpreter_allocate_variables(interpreter, block, frame))
    {
        ps_frame_free(frame);
        return false;
    }

    // Push the frame onto the stack
    if (ps_stack_is_full(interpreter->stack))
    {
        ps_frame_free(frame);
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_STACK_OVERFLOW,
                                                "Stack overflow at level %d for '%s'", interpreter->level, block->name);
    }
    if (NULL == ps_stack_push(interpreter->stack, frame))
    {
        ps_frame_free(frame);
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_STACK_ERROR, "Stack error at level %d for '%s'",
                                                interpreter->level, block->name);
    }

    return true;
}

bool ps_interpreter_exit_frame(ps_interpreter *interpreter)
{
    assert(NULL != interpreter);

    // Decrease interpreter's level
    interpreter->level -= 1;
    ps_interpreter_log(interpreter, PS_DEBUG_INFO, "EXIT FRAME level=%d\n", interpreter->level);

    // Pop the frame from the stack
    if (ps_stack_is_empty(interpreter->stack))
    {
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_STACK_UNDERFLOW, "Stack underflow at level %d",
                                                interpreter->level);
    }
    ps_frame *frame = ps_stack_pop(interpreter->stack);
    if (frame == NULL)
    {
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_STACK_ERROR,
                                                "Stack error at level %d: pop failed", interpreter->level);
    }
    ps_frame_free(frame);

    return true;
}

ps_frame *ps_interpreter_get_block_frame(ps_interpreter *interpreter, const ps_ast_variable *variable_node)
{
    assert(NULL != interpreter);
    assert(NULL != variable_node);
    assert(NULL != variable_node->owner);
    assert(NULL != variable_node->variable);

    if (variable_node->variable->kind != PS_SYMBOL_KIND_VARIABLE)
    {
        ps_interpreter_set_error_message(interpreter, PS_ERROR_EXPECTED_VARIABLE, "Symbol '%s' is a %s, not a variable",
                                         variable_node->variable->name,
                                         ps_symbol_get_kind_name(variable_node->variable->kind));
        return NULL;
    }
    ps_frame *frame = ps_stack_find_frame_for_block(interpreter->stack, variable_node->owner);
    if (frame == NULL)
    {
        ps_interpreter_set_error_message(interpreter, PS_ERROR_SYMBOL_NOT_FOUND, "Variable '%s' not found in any frame",
                                         variable_node->variable->name);
        return NULL;
    }

    return frame;
}

bool ps_interpreter_set_variable_value_array(ps_interpreter *interpreter, ps_frame *frame,
                                             const ps_ast_variable *variable_node, const ps_value *value)
{
    assert(NULL != interpreter);
    assert(NULL != variable_node);
    assert(NULL != value);

    // Evaluate indexes
    ps_symbol *variable = variable_node->variable;
    int dimensions = ps_array_get_dimensions(variable->value->type);
    ps_value indexes[PS_ARRAY_MAX_DIMENSIONS] = {0};
    for (int i = 0; i < dimensions; i++)
    {
        ps_ast_value index_node = {.line = 0,
                                   .column = 0,
                                   .group = PS_AST_EXPRESSION,
                                   .kind = PS_AST_RVALUE,
                                   .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
        if (!ps_ast_eval_expression(interpreter, variable_node->indexes[i], &index_node))
            return false;
        indexes[i].allocated = false;
        indexes[i].type = index_node.value.type;
        indexes[i].data = index_node.value.data;
    }

    // Set value in array data
    ps_handle handle = variable_node->variable->value->data.h;
    ps_value_data array_data = frame->data[handle];
    ps_error error = ps_array_set_value(variable, &array_data, dimensions, indexes, value, interpreter->range_check);
    if (error != PS_ERROR_NONE)
    {
        ps_interpreter_set_error_message(interpreter, error, "Error setting array value");
        return false;
    }
    frame->data[variable_node->variable->value->data.h] = array_data;

    return true;
}

bool ps_interpreter_set_variable_value(ps_interpreter *interpreter, const ps_ast_variable *ast_variable,
                                       const ps_value *value)
{
    assert(NULL != interpreter);
    assert(NULL != ast_variable);
    assert(NULL != value);

    ps_interpreter_log(interpreter, PS_DEBUG_INFO, "SET VARIABLE VALUE: %s <= %s\n", ast_variable->variable->name,
                       ps_value_get_debug_string(value));

    // Retrieve the frame containing the variable
    ps_frame *frame = ps_interpreter_get_block_frame(interpreter, ast_variable);
    if (frame == NULL)
        return false;
    ps_interpreter_log(interpreter, PS_DEBUG_INFO, "Found block %s of type %s\n", frame->block->name,
                       ps_ast_node_get_kind_name(frame->block->kind));

    // Copy the value to the variable
    ps_interpreter_log(interpreter, PS_DEBUG_INFO, "Copying value %s\n", ps_value_get_debug_string(value));
    if (ps_value_is_array(ast_variable->variable->value))
    {
        ps_interpreter_log(interpreter, PS_DEBUG_INFO, "Variable %s is an array\n", ast_variable->variable->name);
        return ps_interpreter_set_variable_value_array(interpreter, frame, ast_variable, value);
    }
    else
    {
        ps_value variable_value = {.allocated = false, .type = ast_variable->variable->value->type, .data = {0}};
        if (!ps_interpreter_copy_value(interpreter, value, &variable_value))
            return false;
        ps_interpreter_log(interpreter, PS_DEBUG_INFO, "Copied value %s\n", ps_value_get_debug_string(&variable_value));
        frame->data[ast_variable->variable->value->data.h] = variable_value.data;
        ps_value debug_value = {.allocated = false, .type = ast_variable->variable->value->type, .data = {0}};
        debug_value.data = frame->data[ast_variable->variable->value->data.h];
        ps_interpreter_log(interpreter, PS_DEBUG_INFO, "Variable %s set to %s\n", ast_variable->variable->name,
                           ps_value_get_debug_string(&debug_value));
    }

    return true;
}

bool ps_interpreter_get_variable_value_simple(ps_interpreter *interpreter, const ps_ast_variable *variable_node,
                                              ps_value *value)
{
    ps_frame *frame = ps_interpreter_get_block_frame(interpreter, variable_node);
    if (frame == NULL)
        return false;
    ps_value variable_value = {.allocated = false,
                               .type = variable_node->variable->value->type,
                               .data = frame->data[variable_node->variable->value->data.h]};

    return ps_interpreter_copy_value(interpreter, &variable_value, value);
}

bool ps_interpreter_get_variable_value_array(ps_interpreter *interpreter, const ps_ast_variable *variable_node,
                                             ps_value *value)
{
    // Retrieve the frame containing the variable
    ps_frame *frame = ps_interpreter_get_block_frame(interpreter, variable_node);
    if (frame == NULL)
        return false;
    // Get the item type of the array
    ps_symbol *item_type = ps_array_get_item_type(variable_node->variable);
    if (item_type == NULL)
        return false;
    // Evaluate the indexes
    ps_value index_values[PS_ARRAY_MAX_DIMENSIONS] = {0};
    for (size_t i = 0; i < variable_node->dimensions; i++)
    {
        ps_ast_value index_value = {.line = 0,
                                    .column = 0,
                                    .group = PS_AST_EXPRESSION,
                                    .kind = PS_AST_LITERAL_VALUE,
                                    .value = {.allocated = false, .type = &ps_system_none, .data = {0}}};
        if (!ps_ast_eval_expression(interpreter, variable_node->indexes[i], &index_value))
            return false;
        index_values[i] = index_value.value;
    }
    // Get the value
    ps_handle handle = variable_node->variable->value->data.h;
    ps_array_data *array_data = frame->data[handle].a;
    ps_error error = ps_array_get_value(variable_node->variable, array_data, variable_node->dimensions, index_values,
                                        value, interpreter->range_check);
    if (error != PS_ERROR_NONE)
        return ps_interpreter_return_false(interpreter, error);

    return true;
}

bool ps_interpreter_get_variable_value(ps_interpreter *interpreter, const ps_ast_variable *variable_node,
                                       ps_value *value)
{
    assert(NULL != interpreter);
    assert(NULL != variable_node);
    assert(NULL != value);

    if (variable_node->dimensions == 0)
        return ps_interpreter_get_variable_value_simple(interpreter, variable_node, value);
    else
        return ps_interpreter_get_variable_value_array(interpreter, variable_node, value);
}

bool ps_interpreter_copy_value(ps_interpreter *interpreter, const ps_value *from, ps_value *to) // NOSONAR
{
    assert(NULL != interpreter);
    assert(NULL != from);
    assert(NULL != to);

    ps_interpreter_log(interpreter, PS_DEBUG_VERBOSE, "ps_interpreter_copy_value: FROM %s (%s) TO %s (%s)\n",
                       ps_value_get_debug_string(from), from == NULL || from->type == NULL ? "NULL!" : from->type->name,
                       ps_value_get_debug_string(to), to == NULL || to->type == NULL ? "NULL!" : to->type->name);
    ps_error error = ps_value_copy(from, to, interpreter->range_check);
    if (error == PS_ERROR_NONE)
        return true;
    if (error != PS_ERROR_TYPE_MISMATCH)
        return ps_interpreter_return_false(interpreter, error);

    return ps_interpreter_set_error_message(
        interpreter, PS_ERROR_TYPE_MISMATCH,
        "Cannot convert value from type '%s' (based on '%s') to type '%s' (based on '%s')",
        ps_value_type_get_name(ps_value_get_type(from)), ps_value_type_get_name(ps_value_get_base(from)),
        ps_value_type_get_name(ps_value_get_type(to)), ps_value_type_get_name(ps_value_get_base(to)));
}

bool ps_interpreter_run(ps_interpreter *interpreter, const ps_ast_block *program)
{
    assert(NULL != interpreter);
    assert(NULL != program);
    assert(PS_AST_BLOCK == program->group && PS_AST_PROGRAM == program->kind);

    ps_interpreter_log(interpreter, PS_DEBUG_VERBOSE, "ps_interpreter_run: %s\n", program->name);
    bool result = ps_ast_execute_program(interpreter, program);
    if (!result)
        fprintf(stderr, "ERROR %d %s at line %u, column %u\n", interpreter->error,
                ps_error_get_message(interpreter->error), interpreter->error_line + 1, interpreter->error_column + 1);
    ps_interpreter_log(interpreter, PS_DEBUG_VERBOSE, "ps_interpreter_run: %s => %s\n", program->name,
                       result ? "OK" : "KO");

    return result;
}
