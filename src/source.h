/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _SOURCE_H
#define _SOURCE_H

#include "vm.h"
#include "lexer.h"

#ifdef __cplusplus
extern "C"
{
#endif

bool vm_scan_source(vm_t *vm);
bool vm_load_file(vm_t *vm, char *filename);
bool vm_set_source(vm_t *vm, char *source, size_t length);
void vm_list_source(vm_t *vm, int from_line, int to_line);
void vm_reset_cursor(vm_t *vm);
char vm_peek_char(vm_t *vm);
char vm_read_char(vm_t *vm);
error_t vm_read_token(vm_t *vm, token_t *token);

#ifdef __cplusplus
}
#endif

#endif /* _SOURCE_H */
