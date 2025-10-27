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

    typedef ps_error (*ps_function_1arg)(ps_interpreter *interpreter, ps_value *value, ps_value *result);
    typedef bool (*ps_procedure_file_value)(ps_interpreter *interpreter, FILE *f, ps_value *value);
    typedef bool (*ps_procedure_file_variable)(ps_interpreter *interpreter, FILE *f, ps_symbol *symbol);

    /** @brief Executable is a function or procedure, address is NULL for user defined executables */
    typedef struct s_ps_executable
    {
        union {
            void *address;                            /** @brief Generic pointer to function/procedure */
            ps_function_1arg func_1arg;               /** @brief Pointer to system function with 1 argument */
            ps_procedure_file_value proc_file_val;    /** @brief Pointer to system procedure with file and value */
            ps_procedure_file_variable proc_file_var; /** @brief Pointer to system procedure with file and variable */
        };
        ps_formal_signature *formal_signature; /** @brief Parameters and return type of the executable */
        uint16_t line;                         /** @brief Line number in the source code */
        uint16_t column;                       /** @brief Column number in the source code */
    } __attribute__((__packed__)) ps_executable;

#define PS_EXECUTABLE_SIZE sizeof(ps_executable)

    ps_executable *ps_executable_alloc(ps_formal_signature *formal_signature, uint16_t line, uint16_t column);
    ps_executable *ps_executable_free(ps_executable *executable);
    void ps_executable_debug(FILE *output, char *message, ps_executable *executable);

#ifdef __cplusplus
}
#endif

#endif /* _PS_EXECUTABLE_H */
