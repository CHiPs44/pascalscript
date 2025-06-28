/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_VALUE_TYPES
#define _PS_VALUE_TYPES

#include <stdbool.h>
#include <stdlib.h>

#include "ps_config.h"
#include "ps_symbol.h"
#include "ps_system_types.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Base types */
    typedef enum e_ps_value_type
    {
        PS_TYPE_NONE,       // No type yet or unknown
        PS_TYPE_REAL,       // real /float
        PS_TYPE_INTEGER,    // signed integer
        PS_TYPE_UNSIGNED,   // unsigned integer
        PS_TYPE_BOOLEAN,    // boolean
        PS_TYPE_CHAR,       // char
        PS_TYPE_STRING,     // string, Pascal style
        PS_TYPE_DEFINITION, // Type definition
        PS_TYPE_EXECUTABLE, // procedure or function
        PS_TYPE_SUBRANGE,   // *FUTURE*
        PS_TYPE_ENUM,       // *FUTURE*
        PS_TYPE_SET,        // *FUTURE*
        PS_TYPE_POINTER,    // *FUTURE*
        PS_TYPE_ARRAY,      // *FUTURE*
        PS_TYPE_RECORD,     // *FUTURE*
        PS_TYPE_FILE,       // *FUTURE*
    } __attribute__((__packed__)) ps_value_type;

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_TYPES */
