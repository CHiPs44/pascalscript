/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_VALUE_DATA_H
#define _PS_VALUE_DATA_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "ps_executable.h"
#include "ps_type_definition.h"
#include "ps_value_types.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct s_ps_type_definition ps_type_definition;

    /** @brief Value union */
    typedef union u_ps_value_data {
        // clang-format off
        ps_real             r; /** @brief "r" is for "_r_eal"       */
        ps_integer          i; /** @brief "i" is for "_i_nteger"    */
        ps_unsigned         u; /** @brief "u" is for "_u_nsigned"   */
        ps_boolean          b; /** @brief "b" is for "_b_oolean"    */
        ps_char             c; /** @brief "c" is for "_c_har"       */
        ps_set_handle       z; /** @brief "z" is for "set"          */
        ps_string          *s; /** @brief "s" is for "_s_tring"     */
        ps_type_definition *t; /** @brief "t" is for "_t_ype"       */
        ps_executable      *x; /** @brief "x" is for "e_x_ecutable" */
        ps_pointer          p; /** @brief "p" is for "_p_ointer"    */
        ps_file            *f; /** @brief "f" is for "_f_ile"       */
        void               *v; /** @brief "v" is for "_v_oid"       */
        // clang-format on
    } __attribute__((__packed__)) ps_value_data;

#define PS_VALUE_DATA_SIZE sizeof(ps_value_data)

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_DATA_H */
