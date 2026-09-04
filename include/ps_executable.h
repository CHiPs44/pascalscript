/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdlib.h>

#include "ps_interpreter.h"

#ifndef _PS_EXECUTABLE_H
#define _PS_EXECUTABLE_H

#include "ps_error.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // Forward references
    typedef struct s_ps_interpreter ps_interpreter;
    // typedef struct s_ps_value ps_value;
    typedef struct s_ps_symbol ps_symbol;
    typedef struct s_ps_ast_block ps_ast_block;

    // clang-format off
    typedef ps_error (*ps_function_1arg_v     )(ps_interpreter *i, const ps_value *v, ps_value *r);
    typedef ps_error (*ps_function_1arg_s     )(ps_interpreter *i, ps_symbol *s, ps_value *r);
    typedef ps_error (*ps_function_2arg_v     )(ps_interpreter *i, const ps_value *a, const ps_value *b, ps_value *r);
    typedef bool     (*ps_procedure_1arg_v    )(ps_interpreter *i, const ps_value *v);
    typedef bool     (*ps_procedure_1arg_s    )(ps_interpreter *i, ps_symbol *s);
    typedef bool     (*ps_procedure_file_write)(ps_interpreter *i, FILE *f, const ps_value *v, int16_t w, int16_t p);
    typedef bool     (*ps_procedure_file_read )(ps_interpreter *i, FILE *f, ps_value *s);
    // clang-format on

    /** @brief Executable kind */
    typedef enum e_ps_executable_kind
    {
        PS_EXECUTABLE_FUNC_1ARG_V,
        PS_EXECUTABLE_FUNC_1ARG_S,
        PS_EXECUTABLE_FUNC_2ARG_V,
        PS_EXECUTABLE_PROC_1ARG_V,
        PS_EXECUTABLE_PROC_1ARG_S,
        PS_EXECUTABLE_PROC_FILE_READ,
        PS_EXECUTABLE_PROC_FILE_WRITE,
        PS_EXECUTABLE_FUNC_USER = 8,
        PS_EXECUTABLE_PROC_USER,
    } __attribute__((__packed__)) ps_executable_kind;

#define PS_EXECUTABLE_KIND_SIZE sizeof(ps_executable_kind)

    /** @brief Executable is a function or procedure, from system or user defined */
    typedef struct s_ps_executable
    {
        // clang-format off
        ps_executable_kind kind;  /** @brief Kind of executable (system function/procedure or user defined)           */
        bool system : 1;          /** @brief True if executable is a system function/procedure, false if user defined */
        unsigned int filler : 23; /** @brief Unused bits to fill 32 bits                                              */
        ps_symbol *return_type;   /** @brief Return type for system functions                                         */
        union {
            void                   *address;         /** @brief Generic pointer to function or procedure           */
            ps_function_1arg_v      func_1arg_v;     /** @brief Pointer to system function with 1 value argument   */
            ps_function_1arg_s      func_1arg_s;     /** @brief Pointer to system function with 1 byref argument   */
            ps_function_2arg_v      func_2arg_v;     /** @brief Pointer to system function with 2 value arguments  */
            ps_procedure_1arg_v     proc_1arg_v;     /** @brief Pointer to system procedure with 1 value argument  */
            ps_procedure_1arg_s     proc_1arg_s;     /** @brief Pointer to system procedure with 1 byref argument  */
            ps_procedure_file_read  proc_file_read;  /** @brief Pointer to "read(file, variable)" system procedure */
            ps_procedure_file_write proc_file_write; /** @brief Pointer to "write(file, value)"   system procedure */
            ps_ast_block           *block;           /** @brief AST block of user defined function or procedure    */
        };
        // clang-format on
    } __attribute__((__packed__)) ps_executable;

#define PS_EXECUTABLE_SIZE sizeof(ps_executable)

    ps_executable *ps_executable_alloc(ps_executable_kind kind, ps_ast_block *block);
    ps_executable *ps_executable_free(ps_executable *executable);
    char *ps_executable_get_kind_name(ps_executable_kind kind);
    void ps_executable_debug(FILE *output, char *message, ps_executable *executable);

#ifdef __cplusplus
}
#endif

#endif /* _PS_EXECUTABLE_H */
