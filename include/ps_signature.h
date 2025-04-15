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
// #include "ps_value.h"
// #include "ps_interpreter.h"

#ifdef __cplusplus
extern "C"
{
#endif

#define PS_SIGNATURE_PARAMETER_COUNT (4)

    typedef struct s_ps_parameter
    {
        ps_symbol *value; // name & type
        bool byref;       // true if parameter is passed by reference
    } __attribute__((__packed__)) ps_parameter;

    #define PS_PARAMETER_SIZE sizeof(ps_parameter)

    typedef struct s_ps_signature
    {
        ps_symbol *result_type;   // NULL for procedures
        uint8_t size;            // size of the array
        uint8_t used;            // number of parameters
        ps_parameter *parameters; // array of parameters
    } __attribute__((__packed__)) ps_signature;

    #define PS_SIGNATURE_SIZE sizeof(ps_signature)

    /** @brief Initialize a new procedure or function signature */
    ps_signature *ps_signature_init(ps_symbol *result_type);

    /** @brief Release a procedure or function signature */
    void ps_signature_done(ps_signature *signature);

    /** @brief Compare formal and actual parameters */
    bool ps_signature_compare(ps_signature *formal_parameters, ps_signature *actual_parameters);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SIGNATURE_H */
