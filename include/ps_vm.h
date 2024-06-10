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
    } ps_vm_opcode_t;

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
    } vm_instruction;

    typedef enum
    {
        OP_FLAG_ZERO,
        // OP_FLAG_,
        // ???
    } ps_vm_flags;

    typedef struct _vm_t
    {
        // parser_t parser;
        symbol_table_t symbols;
        symbol_stack_t stack;
        error_t error;
    } vm_t;

    /**@brief Initialize VM: reset source, global table & stack */
    void vm_init(vm_t *vm);

    // extern bool vm_exec(vm_t *vm);

    /** @brief Get global symbol */
    symbol_t *vm_global_get(vm_t *vm, char *name);
    /** @brief Add global symbol */
    int vm_global_add(vm_t *vm, symbol_t *symbol);
    /** @brief Delete global symbol */
    int vm_global_delete(vm_t *vm, char *name);

    /** @brief Push symbol on top of stack */
    int vm_stack_push(vm_t *vm, symbol_t *symbol);
    /** @brief Pop symbol from top of stack */
    symbol_t *vm_stack_pop(vm_t *vm);

    symbol_t *vm_auto_add_integer(vm_t *vm, int value);
    int vm_auto_free(vm_t *vm, char *name);
    int vm_auto_gc(vm_t *vm);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VM_H */
