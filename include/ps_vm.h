/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_VM_H
#define _PS_VM_H

// #include "ps_buffer.h"
#include "ps_error.h"
// #include "ps_lexer.h"
// #include "ps_parser.h"
#include "ps_symbol_stack.h"
#include "ps_symbol_table.h"
#include "ps_token.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum
    {
        OP_NOP = 0x00,
        OP_PUSH,
        OP_POP,
        OP_CALL,
        OP_RETURN,
        OP_SYS,
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
        OP_ADD,
        OP_SUB,
        OP_MUL,
        OP_DIV, // "div"
        OP_MOD,
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
    } ps_vm_opcode;

    typedef enum
    {
        SYS_FILE_OPEN,
        SYS_FILE_CLOSE,
        SYS_FILE_SEEK,
        SYS_FILE_TRUNCATE,
        SYS_FILE_PUT_BYTE,
        SYS_FILE_GET_BYTE,
        SYS_FILE_PUT_BYTES,
        SYS_FILE_GET_BYTES,
    } ps_vm_sys_command;

    typedef struct _vm_instruction
    {
        uint8_t opcode : 8;
        uint8_t param1_type : 3;
        uint8_t result_type : 3;
    } ps_vm_instruction;

    typedef enum
    {
        OP_FLAG_ZERO,
        // OP_FLAG_,
        // ???
    } ps_vm_flags;

    typedef struct _vm_t
    {
        // ps_parser parser;
        ps_symbol_table symbols;
        ps_symbol_stack stack;
        ps_error error;
    } ps_vm;

    /**@brief Initialize VM: reset source, global table & stack */
    void vm_init(ps_vm *vm);

    // extern bool vm_exec(ps_vm *vm);

    /** @brief Get global symbol */
    ps_symbol *vm_global_get(ps_vm *vm, char *name);
    /** @brief Add global symbol */
    int vm_global_add(ps_vm *vm, ps_symbol *symbol);
    /** @brief Delete global symbol */
    int vm_global_delete(ps_vm *vm, char *name);

    /** @brief Push symbol on top of stack */
    int vm_stack_push(ps_vm *vm, ps_symbol *symbol);
    /** @brief Pop symbol from top of stack */
    ps_symbol *vm_stack_pop(ps_vm *vm);

    ps_symbol *vm_auto_add_integer(ps_vm *vm, int value);
    int vm_auto_free(ps_vm *vm, char *name);
    int vm_auto_gc(ps_vm *vm);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VM_H */
