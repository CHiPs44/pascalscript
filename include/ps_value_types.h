/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_VALUE_TYPES
#define _PS_VALUE_TYPES

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Base types: */
    typedef enum e_ps_value_type
    {
        PS_TYPE_NONE,       /** @brief [N]o type yet or unknown              */
        PS_TYPE_REAL,       /** @brief [R]eal                                */
        PS_TYPE_INTEGER,    /** @brief [I]nteger (Signed)                    */
        PS_TYPE_UNSIGNED,   /** @brief [U]nsigned integer                    */
        PS_TYPE_BOOLEAN,    /** @brief [B]oolean                             */
        PS_TYPE_CHAR,       /** @brief [C]har                                */
        PS_TYPE_SET,        /** @brief [Z] Set                      *FUTURE* */
        PS_TYPE_STRING,     /** @brief [S]tring                              */
        PS_TYPE_DEFINITION, /** @brief [T]ype definition                     */
        PS_TYPE_EXECUTABLE, /** @brief [X] executable: procedure or function */
        PS_TYPE_SUBRANGE,   /** @brief [G] subrange                 *FUTURE* */
        PS_TYPE_ENUM,       /** @brief [E]num                       *FUTURE* */
        PS_TYPE_POINTER,    /** @brief [P]ointer                    *FUTURE* */
        PS_TYPE_ARRAY,      /** @brief [A]rray                      *FUTURE* */
        PS_TYPE_RECORD,     /** @brief [R]ecord                     *FUTURE* */
        PS_TYPE_FILE,       /** @brief [F]ile                       *FUTURE* */
    } __attribute__((__packed__)) ps_value_type;

#ifdef __cplusplus
}
#endif

#endif /* _PS_VALUE_TYPES */
