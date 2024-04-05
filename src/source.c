/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <ctype.h>

#include "vm.h"
#include "lexer.h"

#define READALL_CHUNK 1024
#include "readall.c"

bool vm_scan_source(vm_t *vm)
{
    bool ok = true;
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
    printf("VM: line_count=%d\n", vm->line_count);
    // Allocate arrays for line starts and lengths
    if (vm->line_starts != NULL)
        free(vm->line_starts);
    vm->line_starts = calloc(vm->line_count, sizeof(char *));
    if (vm->line_starts == NULL)
    {
        return false;
    }
    if (vm->line_lengths != NULL)
        free(vm->line_lengths);
    vm->line_lengths = calloc(vm->line_count, sizeof(size_t));
    if (vm->line_lengths == NULL)
    {
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
                ok = false;
                stop = true;
            }
            else
            {
                vm->line_starts[line] = start;
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
    printf("vm_scan_source: %d", ok);
    return ok;
}

bool vm_load_file(vm_t *vm, char *filename)
{
    FILE *input = fopen(filename, "r");
    if (input == NULL)
    {
        return false;
    }
    int result = readall(input, &(vm->source), &(vm->length));
    if (result != READALL_OK)
        return false;
    return vm_scan_source(vm);
}

bool vm_set_source(vm_t *vm, char *source, size_t length)
{
    vm->source = source;
    vm->length = length;
    return vm_scan_source(vm);
}

void vm_list_source(vm_t *vm, int from_line, int to_line)
{
    if (from_line < 0)
        from_line = 0;
    if (from_line > vm->line_count)
        from_line = vm->line_count;
    if (to_line < 0)
        to_line = 0;
    if (to_line > vm->line_count)
        to_line = vm->line_count;
    if (from_line > to_line)
    {
        int temp = from_line;
        from_line = to_line;
        to_line = temp;
    }
    for (int line = from_line; line < to_line; line += 1)
    {
        printf("%04d (%04ld) %s\n", line, vm->line_lengths[line], vm->line_starts[line]);
    }
}

void vm_reset_cursor(vm_t *vm)
{
    vm->current_line = 0;
    vm->current_column = 0;
    vm->current_char = '\0';
}

char vm_peek_char(vm_t *vm)
{
    // if (vm->current_line >= 0 && vm->current_line < vm->line_count)
    //     if (vm->current_column >= 0 && vm->current_column <= vm->line_lengths[vm->current_line])
    //         return vm->line_starts[vm->current_line][vm->current_column];
    // return '\0';
    return vm->current_char;
}

char vm_read_char(vm_t *vm)
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

error_t vm_read_token(vm_t *vm, token_t *token)
{
    char buffer[MAX_COLUMNS];
    char c;
    int pos = 0;

    c = vm_read_char(vm);
    if (c == '\0')
    {
        token->type = TOKEN_NONE;
        return false;
    }
    // skip whitespace
    while (isspace(c))
    {
        c = vm_read_char(vm);
        if (c == '\0')
        {
            token->type = TOKEN_NONE;
            return LEXER_ERROR_UNEXPECTED_EOF;
        }
    }
    if (isalpha(c))
    {
        do
        {
            buffer[pos] = toupper(c);
            pos += 1;
            if (pos > MAX_IDENTIFIER)
            {
                token->type = TOKEN_NONE;
                return LEXER_ERROR_IDENTIFIER_TOO_LONG;
            }
            buffer[pos] = '\0';
            c = vm_read_char(vm);
        } while (isalnum(c));
        // TODO keywords
        token->type = TOKEN_IDENTIFIER;
        strcpy(token->value.identifier, buffer);
        return ERROR_NONE;
    }
    else if (isdigit(c))
    {
        do
        {
            buffer[pos] = c;
            pos += 1;
            if (pos > 12)
            {
                token->type = TOKEN_NONE;
                return LEXER_ERROR_OVERFLOW;
            }
            buffer[pos] = '\0';
            c = vm_read_char(vm);
        } while (isdigit(c));
        if (c == '\0')
        {
            token->type = TOKEN_NONE;
            return LEXER_ERROR_UNEXPECTED_EOF;
        }
    }
    else if (c == '+')
    {
        token->type = TOKEN_ADD;
        return ERROR_NONE;
    }
    else if (c == '-')
    {
        token->type = TOKEN_SUB;
        return ERROR_NONE;
    }
    else if (c == '*')
    {
        token->type = TOKEN_MUL;
        return ERROR_NONE;
    }
    else if (c == '/')
    {
        token->type = TOKEN_DIV;
        return ERROR_NONE;
    }
    return true;
}
