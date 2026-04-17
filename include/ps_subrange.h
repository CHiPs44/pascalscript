/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SUBRANGE
#define _PS_SUBRANGE

#include "ps_config.h"
#include "ps_symbol.h"

#ifdef __cplusplus
extern "C"
{
#endif

    ps_type_definition *ps_subrange_create_char(ps_char min, ps_char max);
    ps_type_definition *ps_subrange_create_integer(ps_integer min, ps_integer max);
    ps_type_definition *ps_subrange_create_unsigned(ps_unsigned min, ps_unsigned max);
    ps_type_definition *ps_subrange_create_enum(ps_symbol *symbol_enum, ps_enum_value min,
                                                                ps_enum_value max);
    ps_unsigned ps_subrange_get_count(const ps_type_definition *type_def);
    ps_unsigned ps_subrange_get_offset(const ps_type_definition *type_def, const ps_value *index);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SUBRANGE */
