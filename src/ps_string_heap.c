/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "ps_memory.h"
#include "ps_string_heap.h"

bool ps_string_heap_debug = false;

ps_string_heap *ps_string_heap_alloc(size_t size, size_t more)
{
    ps_string_heap *heap = (ps_string_heap *)ps_memory_malloc(PS_MEMORY_STRING, sizeof(ps_string_heap));
    if (heap == NULL)
        return NULL; // errno = ENOMEM
    heap->size = size > 0 ? size : PS_STRING_HEAP_SIZE;
    heap->more = more > 0 ? more : heap->size;
    heap->used = 0;
    heap->data = (ps_string_heap_entry *)ps_memory_calloc(PS_MEMORY_STRING, heap->size, sizeof(ps_string_heap_entry));
    if (heap->data == NULL)
    {
        ps_memory_free(PS_MEMORY_STRING, heap);
        return NULL; // errno = ENOMEM
    }
    return heap;
}

ps_string_heap *ps_string_heap_free(ps_string_heap *heap)
{
    if (heap == NULL)
        return NULL;
    if (heap->data != NULL)
    {
        if (ps_string_heap_debug)
            ps_string_heap_dump(stderr, heap);
        for (size_t i = 0; i < heap->size; i++)
            if (heap->data[i].string != NULL)
                heap->data[i].string = ps_string_free(heap->data[i].string);
        ps_memory_free(PS_MEMORY_STRING, heap->data);
    }
    ps_memory_free(PS_MEMORY_STRING, heap);

    return NULL;
}

static unsigned int ps_string_heap_get_hash_key(const char *z)
{
    // DJB2, cf. https://en.wikipedia.org/wiki/Universal_hashing#Hashing_strings
    // 33 * x => 32 * x + x => x << 5 + x
    if (z == NULL)
        return 0;
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

static size_t ps_string_heap_add_internal(ps_string_heap *heap, ps_string *s)
{
    bool debug = false;

    if (s == NULL || heap == NULL)
    {
        errno = EINVAL;
        return SIZE_MAX;
    }
    if (heap->used >= heap->size && !ps_string_heap_grow(heap))
    {
        errno = EOVERFLOW;
        return SIZE_MAX;
    }

    unsigned int hash = ps_string_heap_get_hash_key((const char *)s->str);
    size_t index = hash % heap->size;
    if (debug)
        fprintf(stderr, "s='%s' h=%d i=%zu sz=%zu\n", s->str, hash, index, heap->size);
    size_t start = index;
    // Loop until we find an empty slot or looped back to the start
    do
    {
        if (heap->data[index].string == NULL)
        {
            heap->data[index].string = s;
            heap->data[index].ref_count = 1;
            heap->used++;
            if (debug)
                fprintf(stderr, "ps_string_heap_create: created string '%s' at index %zu\n", (char *)s->str, index);
            return index;
        }
        if (heap->data[index].string == s || strcmp((char *)(heap->data[index].string->str), (char *)s->str) == 0)
        {
            heap->data[index].ref_count += 1;
            if (debug)
                fprintf(stderr, "ps_string_heap_create: found string '%s' at index %zu, ref_count=%d\n",
                        (char *)heap->data[index].string->str, index, heap->data[index].ref_count);
            if (heap->data[index].string != s)
                ps_string_free(s);
            return index;
        }
        index = (index + 1) % heap->size;
    } while (index != start);

    return SIZE_MAX;
}

bool ps_string_heap_grow(ps_string_heap *heap)
{
    if (heap == NULL || heap->more == 0)
        return false;
    size_t old_size = heap->size;
    size_t new_size = heap->size + heap->more;
    if (new_size <= old_size)
        return false;
    ps_string_heap_entry *new_data = (ps_string_heap_entry *)ps_memory_calloc(
            PS_MEMORY_STRING, new_size, sizeof(ps_string_heap_entry));
    if (new_data == NULL)
        return false;
    for (size_t i = 0; i < old_size; i++)
    {
        if (heap->data[i].string == NULL)
            continue;
        size_t index = ps_string_heap_get_hash_key((const char *)heap->data[i].string->str) % new_size;
        while (new_data[index].string != NULL)
            index = (index + 1) % new_size;
        new_data[index] = heap->data[i];
    }
    ps_memory_free(PS_MEMORY_STRING, heap->data);
    heap->data = new_data;
    heap->size = new_size;
    return true;
}

ps_string *ps_string_heap_add(ps_string_heap *heap, ps_string *s)
{
    size_t index = ps_string_heap_add_internal(heap, s);
    return index == SIZE_MAX ? NULL : heap->data[index].string;
}

ps_string *ps_string_heap_create(ps_string_heap *heap, const char *z)
{
    if (z == NULL || heap == NULL)
    {
        errno = EINVAL;
        return NULL;
    }
    size_t len = strnlen(z, PS_STRING_MAX_LEN + 1);
    if (len > PS_STRING_MAX_LEN)
    {
        errno = EINVAL;
        return NULL;
    }
    ps_string *s = ps_string_create(z, (ps_string_len)len);

    ps_string *result = ps_string_heap_add(heap, s);
    if (result == NULL)
        ps_string_free(s);
    return result;
}

bool ps_string_heap_free_string(ps_string_heap *heap, ps_string *s)
{
    if (heap == NULL || s == NULL)
    {
        return false;
    }
    unsigned int hash = ps_string_heap_get_hash_key((const char *)s->str);
    size_t index = hash % heap->size;
    size_t start = index;
    // Loop until we find an empty slot or looped back to the start
    do
    {
        if (heap->data[index].string == s)
        {
            fprintf(stderr, "HEAP FREE STRING: %s %u/%u\n", (char *)s->str, s->len, s->max);
            ps_string_free(s);
            heap->data[index].string = NULL;
            heap->data[index].ref_count = 0;
            heap->used -= 1;
            return true;
        }
    } while (index != start);
    return false; // String not found in the heap
}

ps_string_heap *ps_string_heap_alloc(size_t size, size_t more);

void ps_string_heap_dump(FILE *f, const ps_string_heap *heap)
{
    f = f == NULL ? stderr : f;
    if (heap == NULL)
    {
        fprintf(f, "String Heap: (null)\n");
        return;
    }
    size_t free = 0;
    size_t used = 0;
    fprintf(f, "String Heap: size=%zu, used=%zu, more=%zu\n", heap->size, heap->used, heap->more);
    if (heap->data != NULL)
    {
        for (size_t i = 0; i < heap->size; i++)
        {
            if (heap->data[i].string != NULL)
            {
                used += 1;
                fprintf(f, "  %08zu %08d %-100s max=%03u len=%03u\n", i, heap->data[i].ref_count,
                        (char *)heap->data[i].string->str, heap->data[i].string->max, heap->data[i].string->len);
            }
            else
            {
                free += 1;
            }
        }
    }
    fprintf(f, "  Free slots: %s, Used slots: %s\n", free == heap->size - heap->used ? "OK" : "KO",
            used == heap->used ? "OK" : "KO");
}
