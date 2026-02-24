/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdbool.h>
#include <stdlib.h>

#include "ps_signature.h"
#include "ps_type_definition.h"

#ifndef _PS_EXECUTABLE_H
#define _PS_EXECUTABLE_H

#ifdef __cplusplus
extern "C"
{
#endif

    // Forward references
    typedef struct s_ps_formal_signature ps_formal_signature;
    typedef struct s_ps_type_definition ps_type_definition;
    typedef struct s_ps_interpreter ps_interpreter;
    typedef struct s_ps_value ps_value;
    typedef struct s_ps_symbol ps_symbol;

    typedef ps_error (*ps_function_1arg)(ps_interpreter *i, const ps_value *v, ps_value *r);
    typedef ps_error (*ps_function_1arg_s)(ps_interpreter *i, const ps_symbol *t, ps_value *r);
    typedef ps_error (*ps_function_2args)(ps_interpreter *i, const ps_value *a, const ps_value *b, ps_value *r);
    typedef bool (*ps_procedure_1arg)(ps_interpreter *i, const ps_value *v);
    typedef bool (*ps_procedure_file_write)(ps_interpreter *i, FILE *f, const ps_value *v, int16_t w, int16_t p);
    typedef bool (*ps_procedure_file_read)(ps_interpreter *i, FILE *f, ps_value *s);

    /** @brief Executable is a function or procedure, address is NULL for user defined executables */
    typedef struct s_ps_executable
    {
        union
        {
            void *address;                           /** @brief Generic pointer to function/procedure */
            ps_function_1arg func_1arg;              /** @brief Pointer to system function with 1 value argument */
            ps_function_1arg_s func_1arg_s;          /** @brief Pointer to system function with 1 symbol argument */
            ps_function_2args func_2args;            /** @brief Pointer to system function with 2 value arguments */
            ps_procedure_1arg proc_1arg;             /** @brief Pointer to "procedure(value)" system procedure */
            ps_procedure_file_read proc_file_read;   /** @brief Pointer to "read(variable)" system procedure */
            ps_procedure_file_write proc_file_write; /** @brief Pointer to "write(value)" system procedure */
        };
        ps_formal_signature *formal_signature; /** @brief Parameters and return type for user executables */
        uint16_t line;                         /** @brief Line number in source code */
        uint16_t column;                       /** @brief Column number in source code */
    } __attribute__((__packed__)) ps_executable;

#define PS_EXECUTABLE_SIZE sizeof(ps_executable)

    ps_executable *ps_executable_alloc(ps_formal_signature *formal_signature, uint16_t line, uint16_t column);
    ps_executable *ps_executable_free(ps_executable *executable);
    void ps_executable_debug(FILE *output, char *message, ps_executable *executable);

#ifdef __cplusplus
}
#endif

#endif /* _PS_EXECUTABLE_H */
