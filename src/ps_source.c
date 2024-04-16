/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_error.h"
#include "ps_readall.h"
#include "ps_vm.h"

bool source_scan_text(vm_t *vm)
{
    int line = 0;
    int column = 0;
    char *text = vm->source;
    char *start = text;
    char c;
    // Count lines
    vm->line_count = 0;
    c = *text++;
    while (c)
    {
        if (c == '\n')
            vm->line_count += 1;
        c = *text++;
    }
    // Allocate arrays for line starts and lengths
    if (vm->line_starts != NULL)
        free(vm->line_starts);
    vm->line_starts = calloc(vm->line_count, sizeof(char *));
    if (vm->line_starts == NULL)
    {
        vm->error = ERROR_OUT_OF_MEMORY;
        return false;
    }
    if (vm->line_lengths != NULL)
        free(vm->line_lengths);
    vm->line_lengths = calloc(vm->line_count, sizeof(uint16_t));
    if (vm->line_lengths == NULL)
    {
        vm->error = ERROR_OUT_OF_MEMORY;
        return false;
    }
    // Find and memorize line starts
    text = vm->source;
    bool stop = false;
    while (!stop)
    {
        c = *text;
        switch (c)
        {
        case '\n':
            if (line == MAX_LINES)
            {
                return LEXER_ERROR_BUFFER_OVERFLOW;
            }
            else
            {
                vm->line_starts[line] = start;
                vm->line_lengths[line] = text - start;
                line += 1;
                column = 0;
                text += 1;
                start = text;
            }
            break;
        case '\0':
            stop = true;
            break;
        default:
            column += 1;
            text += 1;
            break;
        }
    }
    vm->line_count = line;
    vm->current_line = 0;
    vm->current_column = 0;
    vm->current_char = '\0';
    vm->error = ERROR_ZERO;
    return true;
}

bool source_load_file(vm_t *vm, char *filename)
{
    FILE *input = fopen(filename, "r");
    if (input == NULL)
    {
        vm->error = ERROR_OPENING_FILE;
        return false;
    }
    int result = readall(input, &(vm->source), &(vm->length));
    if (result != READALL_OK)
    {
        vm->error = ERROR_READING_FILE;
        return false;
    }
    return source_scan_text(vm);
}

bool source_set_text(vm_t *vm, char *source, size_t length)
{
    vm->source = source;
    vm->length = length;
    return source_scan_text(vm);
}

void source_list_text(vm_t *vm, int from_line, int to_line)
{
    char buffer[MAX_COLUMNS + 1];

    from_line =
        from_line < 0 ? 0 : from_line > vm->line_count ? vm->line_count
                                                       : from_line;
    to_line =
        to_line < 0 ? 0 : to_line > vm->line_count ? vm->line_count
                                                   : to_line;
    if (from_line > to_line)
    {
        int temp = from_line;
        from_line = to_line;
        to_line = temp;
    }
    printf("12345 (12345) |1234567890123456789012345678901234567890|\n");
    for (int line = from_line; line < to_line; line += 1)
    {
        strncpy(buffer, vm->line_starts[line], vm->line_lengths[line]);
        buffer[vm->line_lengths[line]] = '\0';
        printf("%05d (%05d) |%-40s|\n", line + 1, vm->line_lengths[line], buffer);
    }
}

void source_reset_cursor(vm_t *vm)
{
    vm->current_line = 0;
    vm->current_column = 0;
    vm->current_char = '\0';
}

char source_peek_char(vm_t *vm)
{
    return vm->current_char;
}

char source_read_next_char(vm_t *vm)
{
    if (vm->current_line >= 0 && vm->current_line < vm->line_count)
    {
        if (vm->current_column >= 0 && vm->current_column <= vm->line_lengths[vm->current_line])
        {
            vm->current_char = vm->line_starts[vm->current_line][vm->current_column];
            vm->current_column += 1;
            if (vm->current_column > vm->line_lengths[vm->current_line])
            {
                vm->current_line += 1;
                vm->current_column = 0;
            }
        }
    }
    return vm->current_char;
}

char source_peek_next_char(vm_t *vm)
{
    char next_char = '\0';
    if (vm->current_line >= 0 && vm->current_line < vm->line_count)
    {
        if (vm->current_column >= 0 && vm->current_column <= vm->line_lengths[vm->current_line])
        {
            next_char = vm->line_starts[vm->current_line][vm->current_column];
        }
    }
    return next_char;
}
