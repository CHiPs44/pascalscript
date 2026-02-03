/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SIGNATURE_H
#define _PS_SIGNATURE_H

#include <stdbool.h>
#include <stdint.h>

#include "ps_symbol.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    // Forward reference
    typedef struct s_ps_interpreter ps_interpreter;

    typedef struct s_ps_formal_parameter
    {
        bool byref;         // true if parameter is passed by reference
        ps_identifier name; // TODO? allocate dynamically
        ps_symbol *type;
    } ps_formal_parameter;

    typedef struct s_ps_formal_signature
    {
        ps_symbol *result_type; // NULL or ps_system_none for procedures
        uint8_t parameter_count;
        uint8_t size;
        ps_formal_parameter *parameters;
    } __attribute__((__packed__)) ps_formal_signature;

#define PS_FORMAL_PARAMETER_SIZE sizeof(ps_formal_parameter)
#define PS_FORMAL_SIGNATURE_SIZE sizeof(ps_formal_signature)

    /** @brief Initialize a new procedure or function *formal* signature */
    ps_formal_signature *ps_formal_signature_alloc(uint8_t size, ps_symbol *result_type);

    /** @brief Release a procedure or function *formal* signature */
    ps_formal_signature *ps_formal_signature_free(ps_formal_signature *signature);

    /** @brief Find a parameter in a formal signature by name */
    ps_formal_parameter * ps_formal_signature_find_parameter(ps_formal_signature *signature, ps_identifier *name);

    /** @brief Add parameter to formal signature */
    bool ps_formal_signature_add_parameter(ps_formal_signature *signature, bool byref, ps_identifier *name,
                                           ps_symbol *type);

    /** @brief Debug a formal signature */
    void ps_formal_signature_debug(FILE *output, char *message, ps_formal_signature *signature);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SIGNATURE_H */
