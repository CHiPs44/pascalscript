/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_EXECUTABLE_H
#define _PS_EXECUTABLE_H

#include <stdbool.h>
#include <stdlib.h>

#include "ps_signature.h"
#include "ps_type_definition.h"
#include "ps_value_types.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct s_ps_executable
    {
        ps_signature *signature; /** @brief Parameters of the executable */
        uint16_t line;           /** @brief Line number in the source code */
        uint8_t column;          /** @brief Column number in the source code */
    } __attribute__((__packed__)) ps_executable;

#ifdef __cplusplus
}
#endif

#endif /* _PS_EXECUTABLE_H */
