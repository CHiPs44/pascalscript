/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_VM_H
#define _PS_VM_H

#include "ps_buffer.h"
#include "ps_error.h"
#include "ps_lexer.h"
#include "ps_parser.h"
#include "ps_symbol_stack.h"
#include "ps_symbol_table.h"
#include "ps_token.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum e_ps_vm_opcode
    {
        OP_NOP = 0x00, // do nothing
        OP_PUSH,       // push value to stack
        OP_POP,        // pop value from stack
        OP_CALL,       // call procedure or function
        OP_RETURN,     // return from procedure or function
        OP_SYS,        // system call
        // Test operators => boolean
        OP_TEST_EQ, // "="
        OP_TEST_NE, // "<>"
        OP_TEST_GT, // ">"
        OP_TEST_GE, // ">="
        OP_TEST_LT, // "<"
        OP_TEST_LE, // "<="
        // Jump
        OP_JUMP,       // Inconditionnally
        OP_JUMP_TRUE,  // Top of stack is strictly boolean true
        OP_JUMP_FALSE, // Top of stack is strictly boolean false
        // Unary operators
        OP_NEG,
        // Binary operators
        OP_ADD,      // +
        OP_SUB,      // -
        OP_MUL,      // *
        OP_DIV,      // integer "div"
        OP_MOD,      // %
        OP_REAL_DIV, // "/"
        // Bit operators
        OP_BIT_NOT,
        OP_BIT_AND,
        OP_BIT_OR,
        OP_BIT_XOR,
        OP_BIT_SHL,
        OP_BIT_SHR,
        // Boolean operators
        OP_BOOL_NOT,
        OP_BOOL_AND,
        OP_BOOL_OR,
        OP_BOOL_XOR,
    } __attribute__((__packed__)) ps_vm_opcode;

    typedef enum e_ps_vm_sys_command
    {
        SYS_FILE_OPEN,
        SYS_FILE_CLOSE,
        SYS_FILE_SEEK,
        SYS_FILE_TRUNCATE,
        SYS_FILE_PUT_BYTE,
        SYS_FILE_GET_BYTE,
        SYS_FILE_PUT_BYTES,
        SYS_FILE_GET_BYTES,
    } __attribute__((__packed__)) ps_vm_sys_command;

    typedef struct s_ps_vm_t
    {
        ps_parser *parser;
        ps_symbol_table *symbols;
        ps_symbol_stack *stack;
        ps_error error;
    } ps_vm;

    /**@brief Initialize VM: reset source, global table & stack */
    ps_vm *ps_vm_init_runtime(ps_vm *vm);

    // extern bool vm_exec(ps_vm *vm);

    /** @brief Get global symbol */
    ps_symbol *ps_vm_global_get(ps_vm *vm, char *name);
    /** @brief Add global symbol */
    int ps_vm_global_add(ps_vm *vm, ps_symbol *symbol);
    /** @brief Delete global symbol */
    int ps_vm_global_delete(ps_vm *vm, char *name);

    /** @brief Push symbol on top of stack */
    int ps_vm_stack_push(ps_vm *vm, ps_symbol *symbol);
    /** @brief Pop symbol from top of stack */
    ps_symbol *ps_vm_stack_pop(ps_vm *vm);

    ps_symbol *vm_auto_add_integer(ps_vm *vm, int value);
    int ps_vm_auto_free(ps_vm *vm, char *name);
    int ps_vm_auto_gc(ps_vm *vm);

    bool ps_vm_load_source(ps_vm *vm, char *source, size_t length);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VM_H */
