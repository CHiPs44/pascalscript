/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_VALUE_H
#define _PS_VALUE_H

#include <stdbool.h>
#include <stdlib.h>

#include "ps_type_definition.h"
#include "ps_value_data.h"
#include "ps_value_types.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Value: type + data + allocated */
    typedef struct s_ps_value
    {
        ps_symbol *type;    /** @brief symbol with type definition */
        ps_value_data data; /** @brief current value */
        bool allocated : 1; /** @brief true if value was allocated (and must be freed) */
    } __attribute__((__packed__)) ps_value;

#define PS_VALUE_SIZE sizeof(ps_value)

    /** @brief Allocate a new value */
    ps_value *ps_value_alloc(ps_symbol *type, ps_value_data data);
    /** @brief Free allocated value */
    ps_value *ps_value_free(ps_value *value);

    /** @brief Scalar is: Integer, Unsigned, Integer or Unsigned subrange */
    bool ps_value_is_scalar(const ps_value *value);
    /** @brief Ordinal is: Boolean, Char or Enum */
    bool ps_value_is_ordinal(const ps_value *value);
    /** @brief Number is: Integer, Unsigned, Real, Integer or Unsigned subrange */
    bool ps_value_is_number(const ps_value *value);
    /** @brief is Real? */
    bool ps_value_is_real(const ps_value *value);
    /** @brief Is String? */
    bool ps_value_is_string(const ps_value *value);
    /** @brief Is Array? */
    bool ps_value_is_array(const ps_value *value);

    /** @brief Get type */
    /** @returns PS_TYPE_NONE if value is invalid */
    ps_value_type ps_value_get_type(const ps_value *value);
    /** @brief Get base type */
    /** @returns PS_TYPE_NONE if value is invalid */
    ps_value_type ps_value_get_base(const ps_value *value);
    /** @brief Copy value from "from" to "to" */
    /** @returns PS_ERROR_NONE if successful */
    ps_error ps_value_copy(const ps_value *from, ps_value *to, bool range_check);

    ps_value *ps_value_set_integer(ps_value *value, ps_integer i);
    ps_value *ps_value_set_unsigned(ps_value *value, ps_unsigned u);
    ps_value *ps_value_set_real(ps_value *value, ps_real r);
    ps_value *ps_value_set_boolean(ps_value *value, ps_boolean b);
    ps_value *ps_value_set_char(ps_value *value, ps_char c);
    ps_value *ps_value_set_string(ps_value *value, ps_string *s);

    char *ps_value_get_display_string(const ps_value *value, int16_t width, int16_t precision);
    char *ps_value_get_debug_string(const ps_value *value);
    void ps_value_debug(FILE *output, const char *message, const ps_value *value);

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_H */
