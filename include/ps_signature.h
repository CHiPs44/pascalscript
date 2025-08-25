/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SIGNATURE_H
#define _PS_SIGNATURE_H

#include <stdbool.h>
#include <stdint.h>

#include "ps_interpreter.h"
#include "ps_symbol.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct s_ps_formal_parameter
    {
        bool by_ref; // true if parameter is passed by reference
        ps_identifier name;
        ps_symbol *type;
    } ps_formal_parameter;

    typedef struct s_ps_formal_signature
    {
        ps_symbol *result_type; // NULL for procedures
        uint8_t parameter_count;
        ps_formal_parameter *parameters;
    } __attribute__((__packed__)) ps_formal_signature;

#define PS_FORMAL_PARAMETER_SIZE sizeof(ps_formal_parameter)
#define PS_FORMAL_SIGNATURE_SIZE sizeof(ps_formal_signature)

    typedef union s_ps_actual_parameter {
        bool by_ref;         // true if parameter is passed by reference
        ps_value *value;     // for by value parameters
        ps_symbol *variable; // for byref parameters
    } ps_actual_parameter;

    typedef struct s_ps_actual_signature
    {
        ps_symbol *result_type; // NULL for procedures
        uint8_t parameter_count;
        ps_formal_parameter *parameters;
    } __attribute__((__packed__)) ps_actual_signature;

#define PS_ACTUAL_PARAMETER_SIZE sizeof(ps_actual_parameter)
#define PS_ACTUAL_SIGNATURE_SIZE sizeof(ps_actual_signature)

    /** @brief Initialize a new procedure or function formal signature */
    ps_formal_signature *ps_formal_signature_alloc(uint8_t size, ps_symbol *result_type);

    /** @brief Release a procedure or function formal signature */
    ps_formal_signature *ps_formal_signature_free(ps_formal_signature *signature);

    /** @brief Add parameter to formal signature */
    bool ps_formal_signature_add_parameter(ps_formal_signature *signature, bool by_ref, ps_identifier *name,
                                           ps_symbol *type);

    /** @brief Add byval parameter to actual signature */
    bool ps_actual_signature_add_byval_parameter(ps_actual_signature *signature, ps_value value);

    /** @brief Add byref parameter to actual signature */
    bool ps_actual_signature_add_byref_parameter(ps_actual_signature *signature, ps_symbol *variable);

    /** @brief Compare formal and actual parameters */
    bool ps_signature_compare(ps_formal_signature *formal, ps_actual_signature *actual);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SIGNATURE_H */
