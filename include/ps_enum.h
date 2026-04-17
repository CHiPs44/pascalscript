/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_ENUM
#define _PS_ENUM

#include <stdbool.h>

#include "ps_config.h"
#include "ps_symbol.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Create empty enumeration */
    ps_type_definition *ps_enum_create();

    /** @brief Set values of (empty) enumeration */
    bool ps_enum_set_values(ps_type_definition *type_def, ps_unsigned count, ps_symbol **values);

#ifdef __cplusplus
}
#endif

#endif /* _PS_ENUM */
