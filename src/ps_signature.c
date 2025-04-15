/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdlib.h>

#include "ps_signature.h"

ps_signature *ps_signature_init(ps_symbol *result_type)
{
    ps_signature *signature = calloc(1, sizeof(ps_signature));
    if (signature == NULL)
        return NULL;
    signature->parameters = calloc(PS_SIGNATURE_PARAMETER_COUNT, sizeof(ps_parameter));
    if (signature->parameters == NULL)
    {
        ps_signature_done(signature);
        return NULL;
    }
    signature->result_type = result_type;
    signature->size = PS_SIGNATURE_PARAMETER_COUNT;
    signature->used = 0;
    return signature;
}

void ps_signature_done(ps_signature *signature)
{
    if (signature == NULL)
        return;
    if (signature->parameters != NULL)
        free(signature->parameters);
    free(signature);
}

bool ps_signature_add_parameter(ps_signature *signature, ps_symbol *value, bool byref)
{
    if (signature->used >= signature->size)
        return false;
    
    return true;
}

bool ps_signature_compare(ps_signature *formal_parameters, ps_signature *actual_parameters)
{
}