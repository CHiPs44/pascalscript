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

bool vm_load_text(vm_t *vm, char *text)
{
    bool ok = true;
    int row, col;
    for (row = 0; row < MAX_ROWS; row += 1)
    {
        strcpy(vm->source[row], "");
    }
    row = 0;
    col = 0;
    char *start = text;
    bool stop = false;
    while (!stop)
    {
        char c = *text;
        switch (c)
        {
        case '\n':
            if (row == MAX_ROWS)
            {
                ok = false;
                stop = true;
            }
            else
            {
                strncpy(vm->source[row], start, text - start);
                vm->max_col[row] = col;
                row += 1;
                col = 0;
                text += 1;
                start = text;
            }
            break;
        case '\0':
            stop = true;
            break;
        default:
            col += 1;
            break;
        }
    }
    vm->max_row = row;
    return ok;
}

void vm_list_source(vm_t *vm, int from, int to)
{
    if (from < 0)
        from = 0;
    if (from > MAX_ROWS)
        from = MAX_ROWS;
    if (to < 0)
        to = 0;
    if (to > MAX_ROWS)
        to = MAX_ROWS;
    if (from > to)
    {
        int temp = from;
        from = to;
        to = temp;
    }
    for (int row = from; row < to; row++)
    {
        printf("%03d %s\n", row, vm->source[row]);
    }
}

void vm_reset_cursor(vm_t *vm)
{
    vm->row = 0;
    vm->col = 0;
}

char vm_peek_char(vm_t *vm)
{
    if (vm->row >= 0 && vm->row < vm->max_row)
        if (vm->col >= 0 && vm->col <= vm->max_col[vm->row])
            return vm->source[vm->row][vm->col];
    return '\0';
}

char vm_read_char(vm_t *vm)
{
    char c;
    if (vm->row >= 0 && vm->row < vm->max_row)
    {
        if (vm->col >= 0 && vm->col <= vm->max_col[vm->row])
        {
            c = vm->source[vm->row][vm->col];
            vm->col += 1;
            if (vm->col > vm->max_col[vm->row])
            {
                vm->row += 1;
                vm->col = 0;
            }
            return c;
        }
    }
    return '\0';
}

error_t vm_read_token(vm_t *vm, token_t *token)
{
    char buffer[MAX_COLS];
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
            buffer[pos] = to_upper(c);
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
