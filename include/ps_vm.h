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
#include "ps_symbol_stack.h"
#include "ps_token.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum e_ps_vm_opcode
    {
        OP_NOP,        // do nothing
        OP_PUSH,       // push value to stack
        OP_POP,        // pop value from stack
        OP_JUMP,       // inconditionnally
        OP_JUMP_TRUE,  // top of stack is strictly boolean true
        OP_JUMP_FALSE, // top of stack is strictly boolean false
        OP_CALL,       // call procedure or function
        OP_RETURN,     // return from procedure or function
        // Test operators => boolean
        OP_TEST_EQ, // "=="
        OP_TEST_NE, // "<>"
        OP_TEST_GT, // ">"
        OP_TEST_GE, // ">="
        OP_TEST_LT, // "<"
        OP_TEST_LE, // "<="
        // Unary operators
        OP_NEG,
        // Binary operators
        OP_ADD,      // +
        OP_SUB,      // -
        OP_MUL,      // *
        OP_DIV,      // "div"
        OP_MOD,      // %
        OP_DIV_REAL, // "/"
        // Bit operators
        OP_BIT_NOT, // !
        OP_BIT_AND, // &
        OP_BIT_OR,  // |
        OP_BIT_XOR, // ^
        OP_BIT_SHL, // <<
        OP_BIT_SHR, // >>
        // Boolean operators
        OP_BOOL_NOT, // !
        OP_BOOL_AND, // &&
        OP_BOOL_OR,  // ||
        OP_BOOL_XOR, // no ^^ but != should do it
        // Specials
        OP_SYS = 0xf0, // system call
        OP_DEBUG = UINT8_MAX,
    } __attribute__((__packed__)) ps_vm_opcode;

#define PS_VM_OPCODE_SIZE sizeof(ps_vm_opcode)

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
        ps_symbol_stack *stack;
        bool allocated;
        bool range_check;
        ps_error error;
    } __attribute__((__packed__)) ps_vm;

#define PS_VM_SIZE sizeof(ps_vm)

    ps_vm *ps_vm_init(ps_vm *vm);
    void ps_vm_free(ps_vm *vm);
    // extern bool ps_vm_exec(ps_vm *vm);

    /** @brief Push symbol on top of stack */
    bool ps_vm_push(ps_vm *vm, ps_symbol *symbol);
    /** @brief Pop symbol from top of stack */
    ps_symbol *ps_vm_pop(ps_vm *vm);

    /** @brief Get absolute value of integer / unsigned / real */
    ps_value *ps_vm_func_abs(ps_vm *vm, ps_value *value);

    /** @brief true if integer/unsigned value is odd, false if even */
    ps_value *ps_vm_func_odd(ps_vm *vm, ps_value *value);

    /** @brief true if integer/unsigned value is even, false if odd */
    ps_value *ps_vm_func_even(ps_vm *vm, ps_value *value);

    /** @brief Get ordinal value of boolean / char */
    ps_value *ps_vm_func_ord(ps_vm *vm, ps_value *value);

    /** @brief Get char value of unsigned / integer or subrange value */
    ps_value *ps_vm_func_chr(ps_vm *vm, ps_value *value);

    /** @brief Get previous value of ordinal value */
    ps_value *ps_vm_func_pred(ps_vm *vm, ps_value *value);

    /** @brief Get next value of ordinal value */
    ps_value *ps_vm_func_succ(ps_vm *vm, ps_value *value);

    // ps_vm_func_sizeof?

#ifdef __cplusplus
}
#endif

#endif /* _PS_VM_H */
