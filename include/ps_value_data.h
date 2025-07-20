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

    typedef ps_integer ps_set_handle; /** @brief Set type handle */

    typedef struct s_ps_file
    {
        FILE *f;
        ps_type_definition *type; /** @brief Type definition of the file */
        bool is_text;
    } ps_file;

    /** @brief Value union */
    typedef union u_ps_value_data {
        // clang-format off
        /*                    Model/bytes 16 32 64 Explanation                */
        /*                                -- -- -- -------------------------- */
        ps_real             r; /** @brief  4  4  8  "r" is for "_r_eal"       */
        ps_integer          i; /** @brief  2  4  8  "i" is for "_i_nteger"    */
        ps_unsigned         u; /** @brief  2  4  8  "u" is for "_u_nsigned"   */
        ps_boolean          b; /** @brief  1  1  1  "b" is for "_b_oolean"    */
        ps_char             c; /** @brief  1  1  1  "c" is for "_c_har"       */
        ps_set_handle       z; /** @brief 32 32 32  "z" is for "set"          */
        ps_integer          g; /** @brief  2  4  8  "g" is for "subran_g_e"   */
        ps_unsigned         e; /** @brief  2  4  8  "e" is for "_e_num"       */
        ps_string          *s; /** @brief  2  4  8  "s" is for "_s_tring"     */
        ps_type_definition *t; /** @brief  2  4  8  "t" is for "_t_ype"       */
        ps_executable      *x; /** @brief  2  4  8  "x" is for "e_x_ecutable" */
        ps_pointer          p; /** @brief  2  4  8  "p" is for "_p_ointer"    */
        ps_file            *f; /** @brief  2  4  8  "f" is for "_f_ile"       */
        void               *v; /** @brief  2  4  8  "v" is for "_v_oid"       */
        // clang-format on
    } __attribute__((__packed__)) ps_value_data;

#define PS_VALUE_DATA_SIZE sizeof(ps_value_data)

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_DATA_H */
