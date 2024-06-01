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

    typedef enum
    {
        OPCODE_NOP = 0x00,
        OPCODE_JUMP,
        OPCODE_CALL,
        OPCODE_PUSH,
        OPCODE_POP,
        OPCODE_TEST,
        OPCODE_SYS,
        // Unary operators
        OPCODE_NEG,
        // Binary operators
        OPCODE_ADD,
        OPCODE_SUB,
        OPCODE_MUL,
        OPCODE_DIV,
        OPCODE_MOD,
        // Bit operators
        OPCODE_BIT_NOT,
        OPCODE_BIT_AND,
        OPCODE_BIT_OR,
        OPCODE_BIT_XOR,
        OPCODE_BIT_SHL,
        OPCODE_BIT_SHR,
        // Boolean operators
        OPCODE_BOOL_NOT,
        OPCODE_BOOL_AND,
        OPCODE_BOOL_OR,
        OPCODE_BOOL_XOR,
    } ps_vm_opcode;

    typedef enum {
        SYS_PUT_CHAR,
        SYS_GET_CHAR,
        SYS_OPEN_FILE,
        SYS_CLOSE_FILE,
    } ps_vm_sys_command;

    typedef enum {
        OPCODE_FLAG_ZERO,
        // OPCODE_FLAG_,
        // ???
    } ps_vm_flags;

    typedef struct _vm_t
    {
        parser_t parser;
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

    symbol_t *vm_auto_add_int(vm_t *vm, int value);
    int vm_auto_free(vm_t *vm, char *name);
    int vm_auto_gc(vm_t *vm);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VM_H */
