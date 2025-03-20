/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_RUNTIME_H
#define _PS_RUNTIME_H

#include <string.h>

#include "ps_error.h"
#include "ps_parser.h"
#include "ps_value.h"
#include "ps_vm.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // static ps_error runtime->errno = PS_ERROR_ZERO;

    typedef struct s_ps_runtime
    {
        ps_parser *parser;
        ps_vm *vm;
        ps_error error;
        bool allocated;
    } ps_runtime;

    ps_runtime *ps_runtime_init(ps_runtime *runtime);
    void ps_runtime_done(ps_runtime *runtime);

    /**
     * @brief Allocate new value
     * @return NULL if no free memory (errno = ENOMEM)
     */
    ps_value *ps_runtime_alloc_value(ps_runtime *runtime);

    /** @brief Free existing value */
    void ps_runtime_free_value(ps_runtime *runtime, ps_value *value);

    /** @brief Get global symbol */
    ps_symbol *ps_runtime_global_get(ps_runtime *runtime, char *name);

    /** @brief Add global symbol */
    int ps_runtime_global_add(ps_runtime *runtime, ps_symbol *symbol);

    /** @brief Delete global symbol */
    int ps_runtime_global_delete(ps_runtime *runtime, char *name);

    ps_symbol *runtime_auto_add_integer(ps_runtime *runtime, int value);
    ps_symbol *ps_runtime_auto_free(ps_runtime *runtime, char *name);
    int ps_runtime_auto_gc(ps_runtime *runtime);
    bool ps_runtime_load_string(ps_runtime *runtime, char *source, size_t length);

#ifdef __cplusplus
}
#endif

#endif /* _PS_RUNTIME_H */
