/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <string.h>

#include "ps_memory.h"
#include "ps_string_heap.h"

ps_string_heap *ps_string_heap_alloc(size_t size, size_t more)
{
    ps_string_heap *heap = (ps_string_heap *)ps_memory_malloc(PS_MEMORY_STRING, sizeof(ps_string_heap));
    if (heap == NULL)
        return NULL; // errno = ENOMEM
    heap->size = size > 0 ? size : PS_STRING_HEAP_SIZE;
    heap->more = more > 0 ? more : heap->size;
    heap->used = 0;
    heap->data = (ps_string **)ps_memory_calloc(PS_MEMORY_STRING, heap->size, sizeof(ps_string *));
    if (heap->data == NULL)
    {
        ps_memory_free(PS_MEMORY_STRING, heap);
        return NULL; // errno = ENOMEM
    }
    return heap;
}

ps_string_heap *ps_string_heap_free(ps_string_heap *heap)
{
    if (heap != NULL && heap->data != NULL)
    {
        for (size_t i = 0; i < heap->size; i++)
        {
            if (heap->data[i] != NULL)
            {
                heap->data[i] = ps_string_free(heap->data[i]);
            }
        }
        ps_memory_free(PS_MEMORY_STRING, heap->data);
    }
    ps_memory_free(PS_MEMORY_STRING, heap);
    return NULL;
}

bool ps_string_heap_grow(ps_string_heap *heap)
{
    if (heap->more == 0)
        return false;
    ps_string **data = (ps_string **)ps_memory_realloc(
        PS_MEMORY_STRING,
        heap->data,
        (heap->size + heap->more) * sizeof(ps_string *));
    if (data == NULL)
        return false; // errno = ENOMEM
    heap->data = data;
    heap->size += heap->more;
    return true;
}

unsigned int ps_string_heap_get_hash_key(const char *z)
{
    // DJB2, cf. https://en.wikipedia.org/wiki/Universal_hashing#Hashing_strings
    // 33 * x => 32 * x + x => x << 5 + x
    unsigned int hash = 5381u;
    unsigned int c = (unsigned int)(*z);
    while (c)
    {
        hash = (hash << 5) + hash + c;
        z++;
        c = (unsigned int)(*z);
    }
    return hash;
}

ps_string *ps_string_heap_create(ps_string_heap *heap, const char *z)
{
    if (z == NULL)
        return NULL;
    size_t len = strlen(z);
    if (len > PS_STRING_MAX_LEN)
    {
        errno = EINVAL;
        return NULL;
    }
    if (heap->used >= heap->size) // && !ps_string_heap_grow(heap))
    {
        errno = EOVERFLOW;
        return NULL;
    }
    unsigned int hash = ps_string_heap_get_hash_key(z);
    size_t index = hash % heap->size;
    size_t start = index;
    // Loop until we find an empty slot or looped back to the start
    do
    {
        if (heap->data[index] == NULL)
        {
            // +1 for null-terminator
            ps_string *s = (ps_string *)ps_memory_malloc(
                PS_MEMORY_STRING, sizeof(ps_string_len) * 2 + len + 1);
            if (s == NULL)
                return NULL; // errno = ENOMEM
            s->max = (ps_string_len)len;
            s->len = (ps_string_len)len;
            memcpy((char *)s->str, z, len);
            s->str[len] = '\0'; // Ensure null-termination
            heap->data[index] = s;
            heap->used++;
            // fprintf(stderr, "ps_string_heap_create: created string '%s' at index %zu\n", s->str, index);
            return s;
        }
        else if (strcmp((char *)(heap->data[index]->str), z) == 0)
        {
            return heap->data[index];
        }
        index = (index + 1) % heap->size;
    } while (index != start);
    return NULL;
}

bool ps_string_heap_free_string(ps_string_heap *heap, ps_string *s)
{
    for (size_t i = 0; i < heap->used; i++)
    {
        if (heap->data[i] == s)
        {
            ps_string_free(s);
            heap->data[i] = NULL;
            heap->used -= 1;
            return true;
        }
    }
    return false; // String not found in the heap
}

void ps_string_heap_dump(const ps_string_heap *heap, FILE *f)
{
    size_t free = 0;
    size_t used = 0;
    if (f == NULL)
        f = stderr;
    fprintf(f, "String Heap: size=%zu, used=%zu\n, more=%zu\n", heap->size, heap->used, heap->more);
    for (size_t i = 0; i < heap->size; i++)
    {
        if (heap->data[i] != NULL)
        {
            used += 1;
            fprintf(f, "  [%zu] %s (max=%u, len=%u)\n", i, heap->data[i]->str, heap->data[i]->max, heap->data[i]->len);
        }
        else
        {
            free += 1;
        }
    }
    fprintf(f, "  Free slots: %s, Used slots: %s\n", free == heap->size - heap->used ? "OK" : "KO",
            used == heap->used ? "OK" : "KO");
}
