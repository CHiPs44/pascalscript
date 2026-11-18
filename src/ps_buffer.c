/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "ps_buffer.h"
#include "ps_error.h"
#include "ps_memory.h"
#include "ps_readall.h"
#include "ps_tools.h"

bool ps_buffer_debug_flag = false;

bool ps_buffer_alloc_lines(ps_buffer *buffer)
{
    buffer->line_starts = ps_memory_calloc(PS_MEMORY_BUFFER, buffer->line_count + 1, sizeof(char *));
    if (buffer->line_starts == NULL)
    {
        buffer->error = PS_ERROR_OUT_OF_MEMORY;
        return false;
    }
    buffer->line_lengths = ps_memory_calloc(PS_MEMORY_BUFFER, buffer->line_count + 1, sizeof(uint16_t));
    if (buffer->line_lengths == NULL)
    {
        ps_memory_free(PS_MEMORY_BUFFER, buffer->line_starts);
        buffer->line_starts = NULL;
        buffer->error = PS_ERROR_OUT_OF_MEMORY;
        return false;
    }
    return true;
}

void ps_buffer_free_lines(ps_buffer *buffer)
{
    if (buffer->line_starts != NULL)
    {
        ps_memory_free(PS_MEMORY_BUFFER, buffer->line_starts);
        buffer->line_starts = NULL;
    }
    if (buffer->line_lengths != NULL)
    {
        ps_memory_free(PS_MEMORY_BUFFER, buffer->line_lengths);
        buffer->line_lengths = NULL;
    }
}

ps_buffer *ps_buffer_alloc(void)
{
    ps_buffer *buffer = ps_memory_malloc(PS_MEMORY_BUFFER, sizeof(ps_buffer));
    if (buffer == NULL)
        return NULL;
    buffer->error = PS_ERROR_NONE;
    buffer->file_errno = 0;
    buffer->from_file = false;
    buffer->length = 0;
    buffer->line_count = 0;
    buffer->line_lengths = NULL;
    buffer->line_starts = NULL;
    buffer->text = NULL;
    ps_buffer_reset(buffer);
    return buffer;
}

ps_buffer *ps_buffer_free(ps_buffer *buffer)
{
    if (buffer->from_file)
        ps_memory_free(PS_MEMORY_BUFFER, buffer->text);
    ps_memory_free(PS_MEMORY_BUFFER, buffer->line_starts);
    ps_memory_free(PS_MEMORY_BUFFER, buffer->line_lengths);
    ps_memory_free(PS_MEMORY_BUFFER, buffer);
    return NULL;
}

void ps_buffer_reset(ps_buffer *buffer)
{
    buffer->current_line = 0;
    buffer->current_column = 0;
    buffer->current_char = '\0';
    buffer->next_char = '\0';
    buffer->error = PS_ERROR_NONE;
}

static void ps_buffer_clear_lines(ps_buffer *buffer)
{
    ps_buffer_free_lines(buffer);
    buffer->line_count = 0;
    ps_buffer_reset(buffer);
}

char *ps_buffer_show_error(const ps_buffer *buffer)
{
    static char error_message[128];
    snprintf(error_message, sizeof(error_message), "BUFFER: %d %s, line %d, column %d", buffer->error,
             ps_error_get_message(buffer->error), buffer->current_line, buffer->current_column);
    return error_message;
}

char *ps_buffer_debug_char(unsigned char c)
{
    static char tmp[8];
    // static char *ctrl[32] = {
    //     "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
    //     "BS ", "TAB", "LF ", "VT ", "FF ", "CR ", "SO ", "SI ",
    //     "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
    //     "CAN", "EM ", "SUB", "ESC", "FS ", "GS ", "RS ", "US "};
    if (c < ' ')
        // control characters
        snprintf(tmp, sizeof(tmp), "^%c/%02x", c + 'A' /*ctrl[c + 0]*/, c);
    else if (c < 0x7f)
        // ASCII printable characters (DEL excluded)
        snprintf(tmp, sizeof(tmp), "%c/%02x", c, c);
    else
        // DEL + extended characters
        snprintf(tmp, sizeof(tmp), "%02x", c);
    return tmp;
}

void ps_buffer_debug(FILE *output, const ps_buffer *buffer, char *message)
{
    if (output == NULL)
        output = stderr;
    fprintf(output, "%s line=%05d col=%03d current=%s, next=%s, error=%d %s\n", message, buffer->current_line,
            buffer->current_column, ps_buffer_debug_char(buffer->current_char), ps_buffer_debug_char(buffer->next_char),
            buffer->error, ps_error_get_message(buffer->error));
}

static int ps_buffer_count_lines(const unsigned char *text, size_t length)
{
    int line_count = 0;
    size_t start = 0;

    size_t position = 0;
    while (position < length)
    {
        unsigned char current_char = text[position];
        position += 1;
        if (current_char == '\r' || current_char == '\n')
        {
            line_count += 1;
            if (current_char == '\r' && position < length && text[position] == '\n')
                position += 1;
            start = position;
        }
    }
    if (start < length)
        line_count += 1;
    return line_count;
}

static bool ps_buffer_index_lines(ps_buffer *buffer)
{
    int line = 0;
    size_t start = 0;

    for (size_t position = 0; position < buffer->length; position += 1)
    {
        unsigned char current_char = buffer->text[position];
        if (current_char == '\r' || current_char == '\n')
        {
            ptrdiff_t line_length = (ptrdiff_t)(position - start);
            if (line_length > PS_BUFFER_MAX_COLUMNS)
            {
                ps_buffer_free_lines(buffer);
                buffer->error = PS_ERROR_OVERFLOW;
                return false;
            }
            buffer->line_starts[line] = buffer->text + start;
            buffer->line_lengths[line] = (uint16_t)line_length;
            line += 1;
            if (current_char == '\r' && position + 1 < buffer->length && buffer->text[position + 1] == '\n')
                position += 1;
            start = position + 1;
        }
    }
    if (start < buffer->length)
    {
        ptrdiff_t line_length = (ptrdiff_t)(buffer->length - start);
        if (line_length > PS_BUFFER_MAX_COLUMNS)
        {
            ps_buffer_free_lines(buffer);
            buffer->error = PS_ERROR_OVERFLOW;
            return false;
        }
        buffer->line_starts[line] = buffer->text + start;
        buffer->line_lengths[line] = (uint16_t)line_length;
    }
    buffer->error = PS_ERROR_NONE;
    return true;
}

bool ps_buffer_scan_text(ps_buffer *buffer)
{
    ps_buffer_clear_lines(buffer);
    uint16_t line = 0;
    uint16_t column = 0;
    for (size_t position = 0; position < buffer->length; position += 1)
    {
        unsigned char current_char = buffer->text[position];
        if (current_char == '\0')
        {
            buffer->current_line = line;
            buffer->current_column = column;
            buffer->error = PS_ERROR_UNEXPECTED_CHARACTER;
            return false;
        }
        if (current_char == '\r' || current_char == '\n')
        {
            line += 1;
            column = 0;
            if (current_char == '\r' && position + 1 < buffer->length && buffer->text[position + 1] == '\n')
                position += 1;
        }
        else
            column += 1;
    }

    int line_count = ps_buffer_count_lines(buffer->text, buffer->length);
    if (line_count > PS_BUFFER_MAX_LINES || line_count > UINT16_MAX)
    {
        buffer->error = PS_ERROR_OVERFLOW;
        return false;
    }
    // No lines?
    if (line_count == 0)
        return true;

    // Allocate arrays for line starts and lengths
    buffer->line_count = (uint16_t)line_count;
    if (!ps_buffer_alloc_lines(buffer))
    {
        return false;
    }
    // Find and memorize line starts
    // should work for Macintosh CR only files and DOS/Windows/Internet CR+LF files.
    return ps_buffer_index_lines(buffer);
}

bool ps_buffer_load_file(ps_buffer *buffer, const char *filename)
{
    int result;
    size_t length;
    long file_length;
    if (buffer->from_file)
    {
        ps_memory_free(PS_MEMORY_BUFFER, buffer->text);
        buffer->text = NULL;
        buffer->from_file = false;
    }
    ps_buffer_clear_lines(buffer);
    buffer->text = NULL;
    buffer->length = 0;
    // Open file
    FILE *input = fopen(filename, "r");
    if (input == NULL)
    {
        buffer->error = PS_ERROR_OPENING_FILE;
        buffer->file_errno = errno;
        return false;
    }
    // Check length of file
    if (fseek(input, 0, SEEK_END))
    {
        buffer->error = PS_ERROR_READING_FILE;
        buffer->file_errno = errno;
        fclose(input);
        return false;
    }
    file_length = ftell(input);
    if (file_length < 0)
    {
        buffer->error = PS_ERROR_READING_FILE;
        buffer->file_errno = errno;
        fclose(input);
        return false;
    }
    length = (size_t)file_length;
    if (length > PS_BUFFER_MAX_SIZE)
    {
        buffer->error = PS_ERROR_OVERFLOW;
        fclose(input);
        return false;
    }
    if (fseek(input, 0, 0))
    {
        buffer->file_errno = errno;
        buffer->error = PS_ERROR_READING_FILE;
        fclose(input);
        return false;
    }
    // Read file
    result = ps_readall(input, (char **)&(buffer->text), &length);
    buffer->file_errno = errno;
    fclose(input);
    switch (result)
    {
    case PS_READALL_OK:
        buffer->from_file = true;
        if (length > PS_BUFFER_MAX_SIZE)
        {
            ps_memory_free(PS_MEMORY_BUFFER, buffer->text);
            buffer->text = NULL;
            buffer->from_file = false;
            buffer->error = PS_ERROR_OVERFLOW;
            return false;
        }
        buffer->length = (uint16_t)length;
        buffer->error = PS_ERROR_NONE;
        return ps_buffer_scan_text(buffer);
    case PS_READALL_NOMEM:
        buffer->error = PS_ERROR_OUT_OF_MEMORY;
        return false;
    default:
        buffer->error = PS_ERROR_READING_FILE;
        return false;
    }
}

bool ps_buffer_load_string(ps_buffer *buffer, unsigned char *text, size_t length)
{
    if (buffer->from_file)
    {
        ps_memory_free(PS_MEMORY_BUFFER, buffer->text);
        buffer->text = NULL;
    }
    buffer->from_file = false;
    ps_buffer_clear_lines(buffer);
    buffer->length = 0;
    buffer->text = text;
    if (length > PS_BUFFER_MAX_SIZE)
    {
        buffer->error = PS_ERROR_OVERFLOW;
        return false;
    }
    buffer->length = (uint16_t)length;
    return ps_buffer_scan_text(buffer);
}

int ps_buffer_dump(FILE *output, const ps_buffer *buffer, uint16_t from_line, uint16_t line_count)
{
    char line[80 + 1];
    int length;

    if (output == NULL)
        output = stderr;

    fprintf(output, "\n");
    if (buffer->line_count == 0)
    {
        fprintf(output, "Buffer is EMPTY!\n");
        return 0;
    }
    fprintf(output,
            "              |         1         2         3         4         5         6         7         8|\n");
    fprintf(output,
            //   123456789012345 <== margin guide for 80 columns
            "Line   (Len)  |12345678901234567890123456789012345678901234567890123456789012345678901234567890|\n");
    for (int line_number = from_line; line_number < from_line + line_count - 1; line_number += 1)
    {
        if (line_number >= buffer->line_count)
            break;
        if (buffer->line_lengths[line_number] > 80)
            length = 80;
        else
            length = 1 + buffer->line_lengths[line_number];
        (void)ps_strscpy(line, (char *)buffer->line_starts[line_number], length);
        fprintf(output, "%05d (%05d) |%-80s|\n", line_number + 1, buffer->line_lengths[line_number], line);
    }
    return 15; // <== margin guide for 80 columns
}

int ps_buffer_peek_char(const ps_buffer *buffer) { return buffer->error == PS_ERROR_NONE ? buffer->current_char : EOF; }

int ps_buffer_peek_next_char(const ps_buffer *buffer)
{
    return buffer->error == PS_ERROR_NONE ? buffer->next_char : EOF;
}

static int ps_buffer_get_current_text_char(const ps_buffer *buffer)
{
    if (buffer->current_line >= buffer->line_count)
        return EOF;
    const unsigned char *line_start = buffer->line_starts[buffer->current_line];
    uint16_t line_length = buffer->line_lengths[buffer->current_line];
    if (buffer->current_column < line_length)
        return line_start[buffer->current_column];
    size_t separator_position = (size_t)(line_start - buffer->text) + line_length;
    if (separator_position >= buffer->length)
        return EOF;
    if (buffer->current_column == line_length)
        return buffer->text[separator_position];
    if (buffer->current_column == line_length + 1 && buffer->text[separator_position] == '\r' &&
        separator_position + 1 < buffer->length && buffer->text[separator_position + 1] == '\n')
        return '\n';
    return EOF;
}

bool ps_buffer_read_next_char(ps_buffer *buffer)
{
    if (buffer->error != PS_ERROR_NONE)
        return false;
    int current_char = ps_buffer_get_current_text_char(buffer);
    if (current_char == EOF)
    {
        buffer->current_char = '\0';
        buffer->next_char = '\0';
        buffer->error = PS_ERROR_EOF;
        if (buffer->current_line < buffer->line_count &&
            buffer->current_column == buffer->line_lengths[buffer->current_line] &&
            (size_t)(buffer->line_starts[buffer->current_line] - buffer->text) +
                            buffer->line_lengths[buffer->current_line] ==
                    buffer->length)
            return true;
        return false;
    }
    if (current_char == '\0')
    {
        buffer->current_char = '\0';
        buffer->next_char = '\0';
        buffer->error = PS_ERROR_UNEXPECTED_CHARACTER;
        return false;
    }
    buffer->current_char = (unsigned char)current_char;
    uint16_t line_length = buffer->line_lengths[buffer->current_line];
    if (buffer->current_column < line_length)
        buffer->current_column += 1;
    else if (buffer->current_column == line_length && current_char == '\r' &&
             (size_t)(buffer->line_starts[buffer->current_line] - buffer->text) + line_length + 1 < buffer->length &&
             buffer->line_starts[buffer->current_line][line_length + 1] == '\n')
        buffer->current_column += 1;
    else
    {
        buffer->current_line += 1;
        buffer->current_column = 0;
    }
    int next_char = ps_buffer_get_current_text_char(buffer);
    buffer->next_char = next_char == EOF ? '\0' : (unsigned char)next_char;
    if (ps_buffer_debug_flag)
    {
        fprintf(stderr, "BUFFER: CURRENT line=%5d, column=%5d, current_char = '%-5s'\n", buffer->current_line,
                buffer->current_column, ps_buffer_debug_char(buffer->current_char));
        fprintf(stderr, "BUFFER: NEXT    line=%5d, column=%5d, current_char = '%-5s', next_char = '%-5s'\n",
                buffer->current_line, buffer->current_column, ps_buffer_debug_char(buffer->current_char),
                ps_buffer_debug_char(buffer->next_char));
    }
    return true;
}

/* EOF */
