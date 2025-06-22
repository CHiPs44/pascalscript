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
    // heap->more = heap->size;
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

// bool ps_string_heap_grow(ps_string_heap *heap)
// {
//     if (heap->more == 0)
//         return false;
//     ps_string **data = (ps_string **)realloc(heap->data, (heap->size + heap->more) * sizeof(ps_string *));
//     if (data == NULL)
//         return false; // errno = ENOMEM
//     heap->data = data;
//     heap->size += heap->more;
//     return true;
// }

unsigned int ps_string_heap_get_hash_key(char *z)
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

ps_string *ps_string_heap_create(ps_string_heap *heap, char *z)
{
    if (z == NULL)
        return NULL;
    size_t len = strlen(z);
    if (len > PS_STRING_MAX_LEN)
        return NULL;
    if (heap->used >= heap->size) // && !ps_string_heap_grow(heap))
        return NULL;
    unsigned int hash = ps_string_heap_get_hash_key(z);
    size_t index = hash % heap->size;
    size_t start = index;
    // Loop until we find an empty slot or looped back to the start
    do
    {
        if (heap->data[index] == NULL)
        {
            ps_string *s = (ps_string *)calloc(1, sizeof(ps_string_len) * 2 + len + 1); // +1 for null-terminator
            if (s == NULL)
                return NULL; // errno = ENOMEM
            s->max = len;
            s->len = len;
            strncpy((char *)s->str, z, len);
            s->str[len] = '\0'; // Ensure null-termination
            heap->data[index] = s;
            heap->used++;
            // fprintf(stderr, "ps_string_heap_create: created string '%s' at index %zu\n", s->str, index);
            return s;
        }
        else if (strcmp((char *)(heap->data[index]->str), z) == 0)
        {
            // fprintf(stderr, "ps_string_heap_create: found existing string '%s' at index %zu\n",
            // heap->data[index]->str, index);
            return heap->data[index];
        }
        index = (index + 1) % heap->size;
    } while (index != start);
    return NULL;
}

// bool ps_string_heap_free(ps_string_heap *heap, ps_string *s)
// {
//     for (size_t i = 0; i < heap->used; i++)
//     {
//         if (heap->data[i] == s)
//         {
//             ps_string_free(s);
//             heap->data[i] = NULL;
//             heap->used -= 1;
//             free(s);
//             return true;
//         }
//     }
//     return false; // String not found in the heap
// }

void ps_string_heap_dump(ps_string_heap *heap, FILE *f)
{
    size_t free = 0;
    size_t used = 0;
    if (f == NULL)
        f = stderr;
    fprintf(f, "String Heap: size=%zu, used=%zu\n" /*, more=%zu\n"*/, heap->size, heap->used /*, heap->more*/);
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
