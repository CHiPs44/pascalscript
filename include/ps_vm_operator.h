/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_OPERATOR_H_
#define _PS_OPERATOR_H_

#include "ps_error.h"
#include "ps_vm.h"

#ifdef __cplusplus
extern "C"
{
#endif

extern ps_error_t vm_exec_op_unary(vm_t *vm, ps_vm_opcode_t op);
extern ps_error_t vm_exec_op_binary(vm_t *vm, ps_vm_opcode_t op);

#ifdef __cplusplus
}
#endif

#endif /* _PS_OPERATOR_H_ */
