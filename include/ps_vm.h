/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_VM_H
#define _PS_VM_H

#include "ps_error.h"
#include "ps_value_stack.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /*
        #
        1   OPCODE
        1+1 OPCODE
    */

    typedef enum e_ps_vm_opcode
    {
        OP_NOP,        // do nothing
        OP_LOAD,       // load constant value (C/I/U/B/R/S) into stack
        OP_JUMP,       // jump inconditionnally
        OP_JUMP_TRUE,  // jump to if top of stack is strictly boolean true
        OP_JUMP_FALSE, // jump to if top of stack top of stack is strictly boolean false
        OP_CALL,       // call procedure or function
        OP_RETURN,     // return from procedure or function
        OP_ENTER,      // enter into environment
        OP_EXIT,       // exit from environment
        OP_HALT,       // stop VM
        OP_DEBUG,      // stop to debugger
        /* Unary operators */
        OP_NEG, // + (int/real)
        OP_NOT, // ! (integer/unsigned/boolean)
        /* Binary operators */
        OP_ADD, // + (integer/unsigned/real/string/char)
        OP_SUB, // - (integer/unsigned/real)
        OP_MUL, // * (integer/unsigned/real)
        OP_DIV, // / (integer/unsigned/real)
        OP_MOD, // % (integer/unsigned)
        OP_AND, // & (integer/unsigned)
        OP_OR,  // | (integer/unsigned)
        OP_XOR, // ^ (integer/unsigned)
        OP_SHL, // << (integer/unsigned)
        OP_SHR, // >> (integer/unsigned)
        /* Comparison operators => boolean */
        OP_TEST_EQ, // == (boolean/integer/unsigned/real/string)
        OP_TEST_NE, // <> (boolean/integer/unsigned/real/string)
        OP_TEST_GT, // >  (integer/unsigned/real/string)
        OP_TEST_GE, // >= (integer/unsigned/real/string)
        OP_TEST_LT, // <  (integer/unsigned/real/string)
        OP_TEST_LE, // <= (integer/unsigned/real/string)
        // Run Time Library
        OP_RTL,
    } __attribute__((__packed__)) ps_vm_opcode;

#define PS_VM_OPCODE_SIZE sizeof(ps_vm_opcode)

    typedef enum e_ps_vm_rtl
    {
        // Pascal
        RTL_FUNC_SUCC,
        RTL_FUNC_PRED,
        RTL_FUNC_ORD,
        RTL_FUNC_CHR,
        RTL_PROC_INC,
        RTL_PROC_DEC,
        // I/O procedures & functions
        RTL_FUNC_EOF,
        RTL_FUNC_EOLN,
        RTL_FUNC_FILEPOS,
        RTL_FUNC_FILESIZE,
        RTL_FUNC_SEEKEOF,
        RTL_FUNC_SEEKEOLN,
        RTL_PROC_ASSIGN,
        RTL_PROC_BLOCKREAD,
        RTL_PROC_BLOCKWRITE,
        RTL_PROC_CLOSE,
        RTL_PROC_ERASE,
        RTL_PROC_FLUSH,
        RTL_PROC_READ,
        RTL_PROC_READLN,
        RTL_PROC_RENAME,
        RTL_PROC_RESET,
        RTL_PROC_REWRITE,
        RTL_PROC_SEEK,
        RTL_PROC_TRUNCATE,
        RTL_PROC_WRITE,
        RTL_PROC_WRITELN,
        // Standard procedures & functions
        RTL_PROC_RANDOMIZE,
        RTL_FUNC_ODD,
        RTL_FUNC_EVEN,
        RTL_FUNC_ORD,
        RTL_FUNC_CHR,
        RTL_FUNC_PRED,
        RTL_FUNC_SUCC,
        RTL_FUNC_RANDOM,
        RTL_FUNC_ABS,
        RTL_FUNC_TRUNC,
        RTL_FUNC_ROUND,
        RTL_FUNC_INT,
        RTL_FUNC_FRAC,
        RTL_FUNC_SIN,
        RTL_FUNC_COS,
        RTL_FUNC_TAN,
        RTL_FUNC_ARCTAN,
        RTL_FUNC_SQR,
        RTL_FUNC_SQRT,
        RTL_FUNC_EXP,
        RTL_FUNC_LN,
        RTL_FUNC_LOG,
    } __attribute__((__packed__)) ps_vm_sys_command;

    typedef union u_ps_vm_instruction {
        uint8_t byte;
        struct
        {
            ps_vm_opcode opcode : 5; // 00-31
            uint8_t size : 3;        // 00-07
        } instruction;
    } ps_vm_instruction;

    typedef struct s_ps_vm
    {
        uint32_t size;
        uint32_t used;
        uint8_t *code;
        uint32_t pc;
        ps_value_stack *stack;
        ps_error error;
        bool range_check;
    } __attribute__((__packed__)) ps_vm;

#define PS_VM_SIZE sizeof(ps_vm)

    ps_vm *ps_vm_init();
    ps_vm *ps_vm_free(ps_vm *vm);
    void ps_vm_reset(ps_vm *vm);
    bool ps_vm_exec(ps_vm *vm);

    /** @brief Append opcode to code array and increment PC */
    bool ps_vm_emit_opcode(ps_vm *vm, ps_vm_opcode opcode);
    /** @brief Append OP_LOAD + sizeof(value) bytes of value and sets PC accordingly */
    bool ps_vm_emit_load(ps_vm *vm, ps_value *value);

    /** @brief Push symbol on top of stack */
    bool ps_vm_push(ps_vm *vm, ps_value *value);
    /** @brief Pop symbol from top of stack */
    ps_value *ps_vm_pop(ps_vm *vm);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VM_H */
