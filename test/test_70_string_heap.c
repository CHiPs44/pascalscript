/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_string_heap.h"

static bool check(bool condition, const char *name)
{
    printf("TEST STRING HEAP: %s %s\n", name, condition ? "OK" : "FAILED!");
    return condition;
}

int main(void)
{
    bool ok = true;
    ps_string_heap *heap = ps_string_heap_alloc(2, 1);
    if (!check(heap != NULL && heap->size == 2 && heap->more == 1 && heap->used == 0, "ALLOC"))
    {
        ps_string_heap_free(heap);
        return EXIT_FAILURE;
    }

    ok &= check(ps_string_heap_grow(NULL) == false, "GROW NULL");
    ok &= check(ps_string_heap_grow(heap) && heap->size == 3 && heap->used == 0, "GROW");

    ps_string *alpha = ps_string_create("alpha", 5);
    ps_string *beta = ps_string_create("beta", 4);
    ps_string *gamma = ps_string_create("gamma", 5);
    ps_string *delta = ps_string_create("delta", 5);
    if (!check(alpha != NULL && beta != NULL && gamma != NULL && delta != NULL, "CREATE INPUTS"))
    {
        ps_string_free(alpha);
        ps_string_free(beta);
        ps_string_free(gamma);
        ps_string_free(delta);
        ps_string_heap_free(heap);
        return EXIT_FAILURE;
    }

    ok &= check(ps_string_heap_add(heap, alpha) == alpha && heap->used == 1, "ADD");
    ok &= check(ps_string_heap_add(heap, alpha) == alpha && heap->data[0].ref_count +
                    heap->data[1].ref_count + heap->data[2].ref_count == 2,
                "ADD SAME POINTER");

    ps_string *duplicate = ps_string_create("alpha", 5);
    const ps_string *canonical = ps_string_heap_add(heap, duplicate);
    ok &= check(canonical == alpha, "ADD DUPLICATE VALUE");

    const ps_string *created = ps_string_heap_create(heap, "alpha");
    ok &= check(created == alpha, "CREATE DUPLICATE");
    ps_string *created_beta = ps_string_heap_create(heap, "beta");
    ok &= check(created_beta != NULL && strcmp((char *)created_beta->str, "beta") == 0, "CREATE NEW");
    ps_string_free(beta);
    beta = NULL;
    ok &= check(ps_string_heap_add(heap, gamma) == gamma && heap->used == 3 && heap->size == 3,
                "ADD BEFORE AUTOMATIC GROW");
    ok &= check(ps_string_heap_add(heap, delta) == delta && heap->size == 4 && heap->used == 4,
                "ADD AUTOMATIC GROW");
    ps_string *created_new = ps_string_heap_create(heap, "new");
    ok &= check(created_new != NULL && strcmp((char *)created_new->str, "new") == 0 &&
                    heap->size == 5 && heap->used == 5,
                "CREATE ANOTHER WITH GROW");

    ps_string_heap *defaults = ps_string_heap_alloc(0, 0);
    ok &= check(defaults != NULL && defaults->size == PS_STRING_HEAP_SIZE &&
                    defaults->more == PS_STRING_HEAP_SIZE,
                "ALLOC DEFAULTS");
    ok &= check(ps_string_heap_grow(defaults) && defaults->size == 2 * PS_STRING_HEAP_SIZE,
                "GROW DEFAULTS");
    defaults->more = 0;
    ok &= check(!ps_string_heap_grow(defaults), "GROW WITHOUT MORE");
    ps_string_heap_free(defaults);

    errno = 0;
    ok &= check(ps_string_heap_create(NULL, "invalid") == NULL && errno == EINVAL, "CREATE NULL HEAP");
    errno = 0;
    ok &= check(ps_string_heap_create(heap, NULL) == NULL && errno == EINVAL, "CREATE NULL VALUE");

    char too_long[PS_STRING_MAX_LEN + 2];
    memset(too_long, 'x', sizeof(too_long) - 1);
    too_long[sizeof(too_long) - 1] = '\0';
    errno = 0;
    ok &= check(ps_string_heap_create(heap, too_long) == NULL && errno == EINVAL, "CREATE TOO LONG");

    ps_string *orphan = ps_string_create("orphan", 6);
    errno = 0;
    ok &= check(orphan != NULL && ps_string_heap_add(NULL, orphan) == NULL && errno == EINVAL, "ADD NULL HEAP");
    ps_string_free(orphan);
    errno = 0;
    ok &= check(ps_string_heap_add(heap, NULL) == NULL && errno == EINVAL, "ADD NULL STRING");

    FILE *dump = tmpfile();
    ok &= check(dump != NULL, "DUMP FILE");
    if (dump != NULL)
    {
        ps_string_heap_dump(dump, heap);
        rewind(dump);
        char output[1024] = {0};
        size_t count = fread(output, 1, sizeof(output) - 1, dump);
        output[count] = '\0';
        ok &= check(strstr(output, "String Heap: size=5, used=5, more=1") != NULL &&
                        strstr(output, "Free slots: OK, Used slots: OK") != NULL,
                    "DUMP CONTENT");
        fclose(dump);
    }

    ps_string_heap_dump(NULL, NULL);
    ok &= check(ps_string_heap_free(NULL) == NULL, "FREE NULL");

    ps_string_heap *removal_heap = ps_string_heap_alloc(8, 2);
    ps_string *removed = ps_string_heap_create(removal_heap, "remove me");
    ps_string *unknown = ps_string_create("not in heap", 11);
    ok &= check(removal_heap != NULL && removed != NULL && unknown != NULL, "FREE STRING SETUP");
    if (removal_heap != NULL && removed != NULL && unknown != NULL)
    {
        ok &= check(!ps_string_heap_free_string(NULL, removed), "FREE STRING NULL HEAP");
        ok &= check(!ps_string_heap_free_string(removal_heap, NULL), "FREE STRING NULL VALUE");
        ok &= check(!ps_string_heap_free_string(removal_heap, unknown), "FREE STRING UNKNOWN");
        ok &= check(ps_string_heap_free_string(removal_heap, removed) && removal_heap->used == 0,
                    "FREE STRING");
        ps_string_free(unknown);
    }
    else
    {
        ps_string_free(unknown);
    }
    ps_string_heap_free(removal_heap);

    ok &= check(ps_string_heap_free(heap) == NULL, "FREE");
    printf("TEST STRING HEAP %s\n", ok ? "OK" : "FAILED!");
    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
