/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _SOURCE_H
#define _SOURCE_H

#include "vm.h"
#include "error.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /**
     * @brief Scan source for lines
     */
    error_t source_scan_text(vm_t *vm);

    /**
     * @brief Load file into source buffer
     */
    error_t source_load_file(vm_t *vm, char *filename);

    /**
     * @brief Set source code from memory buffer
     */
    error_t source_set_text(vm_t *vm, char *source, size_t length);

    void source_list_text(vm_t *vm, int from_line, int to_line);

    void source_reset_cursor(vm_t *vm);

    char source_peek_char(vm_t *vm);

    char source_read_char(vm_t *vm);

#ifdef __cplusplus
}
#endif

#endif /* _SOURCE_H */
