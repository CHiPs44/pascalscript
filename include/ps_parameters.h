/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PARAMETERS_H
#define _PS_PARAMETERS_H

#include <stdbool.h>

#include "ps_symbol.h"
#include "ps_value.h"
#include "ps_interpreter.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct s_ps_parameter
    {
        ps_symbol *parameter; // name & type
        bool byref;           // true if parameter is passed by reference
    } ps_parameter;

    typedef struct s_ps_parameters
    {
        uint16_t size;            // size of the array
        uint16_t count;           // number of parameters
        ps_parameter *parameters; // array of parameters
    } ps_parameters;

    /** @brief Initialize a new parameter list */
    ps_parameters *ps_parameters_init(uint16_t size);

    /** @brief Release a parameter list */
    void ps_parameters_done(ps_parameters *parameters);

    /** @brief Compare formal and actual parameters */
    bool ps_parameters_compare(ps_interpreter *interpreter, ps_parameters *formal_parameters, ps_parameters *actual_parameters);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PARAMETERS_H */
