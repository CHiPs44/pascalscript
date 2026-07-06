/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <string.h>

#include "ps_array.h"
#include "ps_ast.h"
#include "ps_ast_execute.h"
#include "ps_debug.h"
#include "ps_error.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_memory.h"
#include "ps_procedures.h"
#include "ps_stack.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_value.h"

void ps_interpreter_log(ps_interpreter *interpreter, ps_debug_level debug_level, const char *format, ...) // NOSONAR
{
    if (interpreter->debug >= debug_level)
    {
        va_list args;
        va_start(args, format);
        vfprintf(stderr, format, args); // NOSONAR
        va_end(args);
    }
}

ps_interpreter *ps_interpreter_alloc(ps_symbol_table *system, ps_string_heap *string_heap, bool range_check,
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
    interpreter->debug = PS_DEBUG_FATAL;
    interpreter->range_check = range_check;
    interpreter->bool_eval = bool_eval;
    interpreter->io_check = io_check;
    // Allocate stack
    interpreter->stack = ps_stack_alloc(PS_INTERPRETER_STACK_SIZE);
    if (interpreter->stack == NULL)
        return ps_interpreter_free(interpreter);
    // Allocate system variables
    ps_frame *frame = ps_frame_alloc(system->vars);
    if (frame == NULL)
        return ps_interpreter_free(interpreter);
    for (size_t i = 0; i < system->vars; i++)
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
        if (interpreter->stack != NULL)
            interpreter->stack = ps_stack_free(interpreter->stack);
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

bool ps_interpreter_enter_frame(ps_interpreter *interpreter, const ps_ast_block *block, const ps_ast_block *parent)
{
    assert(NULL != interpreter);

    interpreter->level += 1;
    ps_interpreter_log(interpreter, PS_DEBUG_INFO, "ENTER FRAME level=%d '%s' with %zu symbol%s\n", interpreter->level,
                       block->name, block->symbols == NULL ? 0 : block->symbols->used,
                       block->symbols != NULL && block->symbols->used > 1 ? "s" : "");
    ps_frame *frame = ps_frame_alloc(block->symbols->vars);
    if (frame == NULL)
        return false;
    if (ps_stack_is_full(interpreter->stack))
    {
        ps_frame_free(frame);
        interpreter->error = PS_ERROR_STACK_OVERFLOW;
        return ps_interpreter_set_message(interpreter, "Stack overflow at level %d for '%s'", name);
    }
    if (NULL == ps_stack_push(interpreter->stack, frame))
    {
        ps_frame_free(frame);
        interpreter->error = PS_ERROR_STACK_ERROR;
        return ps_interpreter_set_message(interpreter, "Stack error at level %d for '%s': push failed", name);
    }
    return true;
}

bool ps_interpreter_exit_frame(ps_interpreter *interpreter)
{
    assert(NULL != interpreter);
    interpreter->level -= 1;
    ps_interpreter_log(interpreter, PS_DEBUG_INFO, "EXIT FRAME level=%d\n", interpreter->level);
    if (ps_stack_is_empty(interpreter->stack))
    {
        interpreter->error = PS_ERROR_STACK_UNDERFLOW;
        return ps_interpreter_set_message(interpreter, "Stack underflow at level %d", interpreter->level);
    }
    ps_frame *frame = ps_stack_pop(interpreter->stack);
    if (frame == NULL)
    {
        interpreter->error = PS_ERROR_STACK_ERROR;
        return ps_interpreter_set_message(interpreter, "Stack error at level %d: pop failed", interpreter->level);
    }
    ps_frame_free(frame);
    return true;
}

// ps_symbol *ps_interpreter_find_symbol(ps_interpreter *interpreter, ps_ast_block *block, const char *name, bool local)
// {
//     assert(interpreter != NULL);
//     assert(name != NULL);
//     ps_symbol *symbol = NULL;

//     // No block => search into SYSTEM
//     if (block == NULL)
//     {
//         symbol = ps_symbol_table_get(interpreter->system, name);
//         if (interpreter->debug >= PS_DEBUG_VERBOSE)
//             fprintf(stderr, " DEBUG\tps_interpreter_find_symbol('%s', '%s', %s) => '%s'\n", "SYSTEM", name,
//                     local ? "Local" : "Global", symbol == NULL ? "Not found" : symbol->name);
//     }
//     else
//     {
//         // Search in current block
//         symbol = ps_symbol_table_get(block->symbols, name);
//         if (!local && symbol == NULL)
//             // Not found => search in parent
//             return ps_interpreter_find_symbol(interpreter, block->parent, name, false);
//         if (interpreter->debug >= PS_DEBUG_VERBOSE)
//             fprintf(stderr, " DEBUG\tps_interpreter_find_symbol('%s', '%s', %s) => '%s'\n", block->name, name,
//                     local ? "Local" : "Global", symbol == NULL ? "Not found" : symbol->name);
//     }

//     return symbol;
// }

bool ps_interpreter_copy_value(ps_interpreter *interpreter, const ps_value *from, ps_value *to) // NOSONAR
{
    assert(NULL != interpreter);
    assert(NULL != from);
    assert(NULL != to);
    ps_interpreter_log(interpreter, PS_DEBUG_VERBOSE, "ps_interpreter_copy_value: FROM %s (%s) TO %s (%s)\n",
                       ps_value_get_debug_string(from), ps_value_type_get_name(from->type->value->data.t->type),
                       ps_value_get_debug_string(to), ps_value_type_get_name(to->type->value->data.t->type));
    ps_error error = ps_value_copy(from, to, interpreter->range_check);
    if (error == PS_ERROR_NONE)
        return true;
    if (error != PS_ERROR_TYPE_MISMATCH)
        return ps_interpreter_return_false(interpreter, error);
    ps_interpreter_set_message(
        interpreter, "Cannot convert value from type '%s' (based on '%s') to type '%s' (based on '%s')",
        ps_value_type_get_name(from->type->value->data.t->type), ps_value_type_get_name(ps_value_get_base(from)),
        ps_value_type_get_name(ps_value_get_type(to)), ps_value_type_get_name(ps_value_get_base(to)));
    return ps_interpreter_return_false(interpreter, PS_ERROR_TYPE_MISMATCH);
}

bool ps_interpreter_run(ps_interpreter *interpreter, const ps_ast_block *program)
{
    assert(NULL != interpreter);
    assert(NULL != program);

    ps_interpreter_log(interpreter, PS_DEBUG_VERBOSE, "ps_interpreter_run: %s\n", program->name);
    bool result = ps_ast_execute_program(interpreter, program);
    ps_interpreter_log(interpreter, PS_DEBUG_VERBOSE, "ps_interpreter_run: %s => %s\n", program->name,
                       result ? "OK" : "KO");
    return result;
}
