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

    typedef ps_error (*ps_function_1arg)(ps_interpreter *interpreter, ps_value *value, ps_value *result);
    typedef bool (*ps_procedure_file)(ps_interpreter *interpreter, FILE *f, ps_value *value);

    /** @brief Executable is a function or procedure */
    typedef struct s_ps_executable
    {
        ps_formal_signature *formal_signature; /** @brief Parameters and return type of the executable */
        uint16_t line;                         /** @brief Line number in the source code */
        uint16_t column;                       /** @brief Column number in the source code */
        union {
            void *address;               /** @brief Generic pointer to function/procedure */
            ps_function_1arg func_1arg;  /** @brief Pointer to system function (NULL for user defined functions) */
            ps_procedure_file proc_file; /** @brief Pointer to system procedure (NULL for user defined procedures) */
        };
    } __attribute__((__packed__)) ps_executable;

#define PS_EXECUTABLE_SIZE sizeof(ps_executable)

    ps_executable *ps_executable_alloc(ps_formal_signature *formal_signature, uint16_t line, uint16_t column);
    ps_executable *ps_executable_free(ps_executable *executable);
    void ps_executable_debug(FILE *output, char *message, ps_executable *executable);

#ifdef __cplusplus
}
#endif

#endif /* _PS_EXECUTABLE_H */
