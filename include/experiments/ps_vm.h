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

#define PS_VM_OPCODE_BITS 5
#define PS_VM_OPCODE_MASK 0b11111000
#define PS_VM_TYPE_BITS 3
#define PS_VM_TYPE_MASK 0b000000111

    typedef enum e_ps_vm_opcode
    {
        // clang-format off
        /*  */
        OP_NOP      = (0x00 << PS_VM_TYPE_BITS), // do nothing
        OP_LIT      = (0x01 << PS_VM_TYPE_BITS), // load literal/constant value (C/I/U/B/R/S) into stack
        OP_JMP      = (0x02 << PS_VM_TYPE_BITS), // jump inconditionnally
        OP_JPT      = (0x03 << PS_VM_TYPE_BITS), // jump to if top of stack is strictly boolean true
        OP_JPF      = (0x04 << PS_VM_TYPE_BITS), // jump to if top of stack top of stack is strictly boolean false
        OP_CUP      = (0x05 << PS_VM_TYPE_BITS), // call subroutine: procedure or function
        OP_RET      = (0x06 << PS_VM_TYPE_BITS), // return from procedure or function
        OP_ENTER    = (0x07 << PS_VM_TYPE_BITS), // enter into environment
        OP_EXIT     = (0x08 << PS_VM_TYPE_BITS), // exit from environment
        OP_HLT      = (0x09 << PS_VM_TYPE_BITS), // stop VM
        OP_DBG      = (0x0a << PS_VM_TYPE_BITS), // stop to debugger
        /* Unary operators */
        OP_NEG      = (0x0b << PS_VM_TYPE_BITS), // + (int/real)
        OP_NOT      = (0x0c << PS_VM_TYPE_BITS), // ! (integer/unsigned/boolean)
        /* Binary operators */
        OP_ADD      = (0x0d << PS_VM_TYPE_BITS), // + (integer/unsigned/real/string/char)
        OP_SUB      = (0x0e << PS_VM_TYPE_BITS), // - (integer/unsigned/real)
        OP_MUL      = (0x0f << PS_VM_TYPE_BITS), // * (integer/unsigned/real)
        OP_DIV      = (0x10 << PS_VM_TYPE_BITS), // / (integer/unsigned/real)
        OP_MOD      = (0x11 << PS_VM_TYPE_BITS), // % (integer/unsigned)
        OP_AND      = (0x12 << PS_VM_TYPE_BITS), // & (integer/unsigned)
        OP_OR       = (0x13 << PS_VM_TYPE_BITS), // | (integer/unsigned)
        OP_XOR      = (0x14 << PS_VM_TYPE_BITS), // ^ (integer/unsigned)
        OP_SHL      = (0x15 << PS_VM_TYPE_BITS), // << (integer/unsigned)
        OP_SHR      = (0x16 << PS_VM_TYPE_BITS), // >> (integer/unsigned)
        /* Comparison operators (=> boolean) */
        OP_CEQ      = (0x17 << PS_VM_TYPE_BITS), // == (boolean/integer/unsigned/real/char/string)
        OP_CNE      = (0x18 << PS_VM_TYPE_BITS), // <> (boolean/integer/unsigned/real/char/string)
        OP_CGT      = (0x19 << PS_VM_TYPE_BITS), // >  (integer/unsigned/real/char/string)
        OP_CGE      = (0x1a << PS_VM_TYPE_BITS), // >= (integer/unsigned/real/char/string)
        OP_CLT      = (0x1b << PS_VM_TYPE_BITS), // <  (integer/unsigned/real/char/string)
        OP_CLE      = (0x1c << PS_VM_TYPE_BITS), // <= (integer/unsigned/real/char/string)
        /* Run Time Library */
        OP_SYS      = (0x1d << PS_VM_TYPE_BITS), // next byte is RTL prodedure/function number
        // clang-format on
    } __attribute__((__packed__)) ps_vm_opcode;

#define PS_VM_OPCODE_SIZE sizeof(ps_vm_opcode)

    // clang-format off
    typedef enum e_ps_vm_sys
    {
        SYS_FILE_ASSIGN,   SYS_FILE_BLOCKREAD, SYS_FILE_BLOCKWRITE,
        SYS_FILE_CLOSE,    SYS_FILE_EOF,       SYS_FILE_EOLN,
        SYS_FILE_ERASE,    SYS_FILE_FILEPOS,   SYS_FILE_FILESIZE,
        SYS_FILE_FLUSH,    SYS_FILE_READ,      SYS_FILE_READLN,
        SYS_FILE_RENAME,   SYS_FILE_RESET,     SYS_FILE_REWRITE,
        SYS_FILE_SEEK,     SYS_FILE_SEEKEOF,   SYS_FILE_SEEKEOLN,
        SYS_FILE_TRUNCATE, SYS_FILE_WRITE,     SYS_FILE_WRITELN,
        SYS_MATH_ABS,      SYS_MATH_ARCTAN,    SYS_MATH_COS,
        SYS_MATH_EVEN,     SYS_MATH_EXP,       SYS_MATH_FRAC,
        SYS_MATH_INT,      SYS_MATH_LN,        SYS_MATH_LOG,
        SYS_MATH_ODD,      SYS_MATH_RANDOM,    SYS_MATH_RANDOMIZE,
        SYS_MATH_ROUND,    SYS_MATH_SIN,       SYS_MATH_SQR,
        SYS_MATH_SQRT,     SYS_MATH_TAN,       SYS_MATH_TRUNC,
        SYS_TYPE_CHR,      SYS_TYPE_DEC,       SYS_TYPE_INC,
        SYS_TYPE_ORD,      SYS_TYPE_PRED,      SYS_TYPE_SUCC,
    } __attribute__((__packed__)) ps_vm_sys_command;
    // clang-format on

    typedef struct s_ps_vm
    {
        uint32_t size;         /** @brief Code total size  */
        uint32_t used;         /** @brief Code used size   */
        uint8_t *code;         /** @brief Code itself      */
        uint32_t pc;           /** @brief Program counter  */
        ps_value_stack *stack; /** @brief Value stack      */
        ps_error error;        /** @brief Error code       */
        bool debug;            /** @brief Debug mode       */
        bool trace;            /** @brief Trace mode       */
        bool range_check;      /** @brief Range check mode */
        bool io_check;         /** @brief I/O check mode   */
    } /*__attribute__((__packed__))*/ ps_vm;

#define PS_VM_SIZE sizeof(ps_vm)

    bool ps_vm_return_false(ps_vm *vm, ps_error error);
    void *ps_vm_return_null(ps_vm *vm, ps_error error);
    char *ps_vm_get_opcode_name(ps_vm_opcode opcode);

    ps_vm *ps_vm_init(uint32_t size);
    ps_vm *ps_vm_free(ps_vm *vm);
    void ps_vm_reset(ps_vm *vm);
    bool ps_vm_exec(ps_vm *vm);

    /** @brief Append opcode to code array and increment PC */
    bool ps_vm_emit(ps_vm *vm, ps_vm_opcode opcode, ps_value_type type);
    /** @brief Append OP_LIT + sizeof(value) bytes of value and sets PC accordingly */
    bool ps_vm_emit_load(ps_vm *vm, ps_value *value);

    /** @brief Push symbol on top of stack */
    bool ps_vm_push(ps_vm *vm, ps_value *value);
    /** @brief Pop symbol from top of stack */
    bool ps_vm_pop(ps_vm *vm, ps_value *value);

    void ps_vm_dump(ps_vm *vm, char *title);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VM_H */
