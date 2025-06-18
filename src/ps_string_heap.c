/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_string_heap.h"

ps_string_heap *ps_string_heap_init(size_t size)
{
    ps_string_heap *heap;
    heap = (ps_string_heap *)calloc(1, sizeof(ps_string_heap));
    if (heap == NULL)
        return NULL; // errno = ENOMEM
    heap->size = size > 0 ? size : PS_STRING_HEAP_SIZE;
    heap->more = heap->size;
    heap->used = 0;
    heap->data = (ps_string **)calloc(heap->size, sizeof(ps_string *));
    if (heap->data == NULL)
    {
        free(heap);
        return NULL; // errno = ENOMEM
    }
    return heap;
}

void ps_string_heap_done(ps_string_heap *heap)
{
    if (heap == NULL)
        return;
    if (heap->data != NULL)
    {
        for (size_t i = 0; i < heap->size; i++)
        {
            if (heap->data[i] != NULL)
            {
                ps_string_free(heap->data[i]);
            }
        }
        free(heap->data);
    }
    free(heap);
}

bool ps_string_heap_grow(ps_string_heap *heap)
{
    if (heap->more == 0)
        return false;
    ps_string **new_data = (ps_string **)realloc(heap->data, (heap->size + heap->more) * sizeof(ps_string *));
    if (new_data == NULL)
        return false; // errno = ENOMEM
    heap->data = new_data;
    heap->size += heap->more;
    return true;
}

ps_string *ps_string_heap_alloc(ps_string_heap *heap, ps_string_len max, char *z)
{
    if (heap->used >= heap->size && !ps_string_heap_grow(heap))
        return NULL;
    size_t len = strlen(z);
    ps_string *s = (ps_string *)calloc(1, sizeof(ps_string_len) * 2 + max + 1); // +1 for null-terminator
    if (s == NULL)
        return NULL; // errno = ENOMEM
    s->max = max;
    if (z == NULL)
    {
        // Initialize to empty string
        s->len = 0;
        s->str[0] = '\0';
    }
    else
    {
        // Copy the string if z is provided
        strncpy((char *)s->str, z, max);
        s->str[max] = '\0'; // Ensure null-termination
        s->len = strlen((char *)s->str);
    }
    // Find a free slot in the heap
    for (size_t i = 0; i < heap->size; i++)
    {
        if (heap->data[i] == NULL)
        {
            heap->data[i] = s;
            heap->used++;
            return s;
        }
    }
    free(s); // If we reach here, it means we didn't find a slot
    return NULL;
}

bool ps_string_heap_free(ps_string_heap *heap, ps_string *s)
{
    for (size_t i = 0; i < heap->used; i++)
    {
        if (heap->data[i] == s)
        {
            ps_string_free(s);
            heap->data[i] = NULL;
            heap->used -= 1;
            free(s);
            return true;
        }
    }
    return false; // String not found in the heap
}

void ps_string_heap_dump(ps_string_heap *heap, FILE *f)
{
    size_t free = 0;
    size_t used = 0;
    if (f == NULL)
        f = stderr;
    fprintf(f, "String Heap: size=%zu, used=%zu, more=%zu\n", heap->size, heap->used, heap->more);
    for (size_t i = 0; i < heap->size; i++)
    {
        if (heap->data[i] != NULL)
        {
            used += 1;
            fprintf(f, "  [%zu] %s (max=%zu, len=%zu)\n", i, heap->data[i]->str, heap->data[i]->max,
                    heap->data[i]->len);
        }
        else
        {
            free += 1;
        }
    }
    fprintf(f, "  Free slots: %s, Used slots: %s\n", free == heap->size - heap->used ? "OK" : "KO",
            used == heap->used ? "OK" : "KO");
}
