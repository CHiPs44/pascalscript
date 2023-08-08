/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdlib.h>

#include "runtime_error.h"

const char *runtime_error_get_message(const runtime_error_t runtime_error)
{
    if  (runtime_error>=RUNTIME_OK&& runtime_error<RUNTIME_END_ENUM) {
        return runtime_error_messages[runtime_error];
    }
    return NULL;
}

/* EOF */
