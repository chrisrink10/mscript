/*****************************************************************************
 * libds :: array.c
 *
 * Dynamic array/stack data structure.
 *
 * Author:  Chris Rink <chrisrink10@gmail.com>
 *
 * License: MIT (see LICENSE document at source tree root)
 *****************************************************************************/

#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>
#include "libds/array.h"
#include "iterpriv.h"

struct DSArray {
    void **data;
    size_t len;
    size_t cap;
    dsarray_compare_fn cmp;
    dsarray_free_fn free;
};

static bool dsarray_resize(DSArray *array, size_t cap);
static void dsarray_free(DSArray *array);

/*
 * ARRAY PUBLIC FUNCTIONS
 */

DSArray* dsarray_new(dsarray_compare_fn cmpfn, dsarray_free_fn freefn) {
    return dsarray_new_cap(DSARRAY_DEFAULT_CAPACITY, cmpfn, freefn);
}

DSArray* dsarray_new_cap(size_t cap, dsarray_compare_fn cmpfn, dsarray_free_fn freefn) {
    assert(cap > 0);
    DSArray *array = malloc(sizeof(DSArray));
    if (!array) {
        return NULL;
    }

    array->data = calloc(cap, sizeof(void *));
    if (!array->data) {
        free(array);
        return NULL;
    }

    array->len = 0;
    array->cap = cap;
    array->cmp = cmpfn;
    array->free = freefn;
    return array;
}

DSArray *dsarray_new_lit(void **list, size_t len, size_t cap, dsarray_compare_fn cmpfn, dsarray_free_fn freefn) {
    if ((!list) || (cap < len)) { return NULL; }

    DSArray *array = dsarray_new_cap(cap, cmpfn, freefn);
    if (!array) {
        return NULL;
    }

    for (size_t i = 0; i < cap; i++) {
        array->data[i] = list[i];
        array->len++;
    }

    return array;
}

void dsarray_destroy(DSArray *array) {
    if (!array) { return; }
    dsarray_free(array);
    free(array->data);
    free(array);
}

size_t dsarray_len(const DSArray *array) {
    assert(array);
    return array->len;
}

size_t dsarray_cap(const DSArray *array) {
    assert(array);
    return array->cap;
}

void* dsarray_get(const DSArray *array, size_t index) {
    if (!array) { return NULL; }
    if (index > array->len) { return NULL; }
    return array->data[index];
}

void* dsarray_top(const DSArray *array) {
    if (!array) { return NULL; }
    if (array->len == 0) { return NULL; }
    return array->data[array->len-1];
}

void dsarray_foreach(DSArray *array, void (*func)(void*)) {
    if ((!array) || (!func)) { return; }

    for (size_t i = 0; i < array->len; i++) {
        func(array->data[i]);
    }
}

bool dsarray_append(DSArray *array, void *elem) {
    return (!array) ? (false) : dsarray_insert(array, elem, array->len);
}

bool dsarray_extend(DSArray *array, DSArray *other) {
    if ((!array) || (!other)) { return false; }
    if (other->len == 0) { return true; }

    size_t len = other->len;
    size_t i = 0;
    for (i = 0; i < len; i++) {
        if (!dsarray_append(array, dsarray_get(other, i))) {
            other->len = other->len - i;
            return false;
        }
        other->data[i] = NULL;
    }

    other->len = other->len - i;
    return true;
}

bool dsarray_insert(DSArray *array, void *elem, size_t index) {
    if ((!array) || (!elem)) { return false; }

    if (index > (array->len)) {
        return false;
    }

    if ((array->len + 1) > array->cap) {
        if (!dsarray_resize(array, array->cap * DSARRAY_CAPACITY_FACTOR)) {
            return false;
        }
    }

    for (size_t i = array->len; i > index; i--) {
        array->data[i] = array->data[i-1];
    }

    array->data[index] = elem;
    array->len++;
    return true;
}

void* dsarray_remove(DSArray *array, void *elem) {
    if ((!array) || (!elem)) { return NULL; }

    int i = dsarray_index(array, elem);
    if (i < 0) { return NULL; }
    return dsarray_remove_index(array, (size_t)i);
}

void* dsarray_remove_index(DSArray *array, size_t index) {
    if (!array) { return NULL; }

    if (index >= array->len) {
        return NULL;
    }

    void* cache = array->data[index];

    for (size_t i = index; i < array->len; i++) {
        array->data[i] = array->data[i+1];
    }

    array->data[array->len] = NULL;
    array->len--;
    return cache;
}

void* dsarray_pop(DSArray *array) {
    if (!array) { return NULL; }
    return dsarray_remove_index(array, array->len-1);
}

void dsarray_clear(DSArray *array) {
    if (!array) { return; }
    dsarray_free(array);
    array->len = 0;
}

int dsarray_index(const DSArray *array, void *elem) {
    if ((!array) || (!elem)) {
        return DSARRAY_NULL_POINTER;
    }
    if (!array->cmp) {
        return DSARRAY_NO_CMP_FUNC;
    }

    for(size_t i = 0; i < array->len; i++) {
        if (array->cmp(&array->data[i], &elem) == 0) {
            return (int)i;
        }
    }

    return DSARRAY_NOT_FOUND;
}

void dsarray_sort(DSArray *array) {
    if ((!array) || (array->len == 0) || (!array->cmp)) {
        return;
    }
    qsort(array->data, array->len, sizeof(void *), array->cmp);
}

void dsarray_reverse(DSArray *array) {
    if (!array) { return; }

    // Number of operations required, assuming integer truncation
    int lim = ((int)array->len / 2);

    // Upper index, which will count down
    int up = (int)array->len - 1;

    // Swap i index with up index as they both tick towards the middle
    for (int i = 0; i < lim; i++) {
        void *cache = array->data[i];
        array->data[i] = array->data[up];
        array->data[up] = cache;
        up--;
    }
}

DSIter* dsarray_iter(DSArray *array) {
    if (!array) { return NULL; }

    DSIter *iter = dsiter_priv_new(ITER_ARRAY, array);
    if (!iter) {
        return NULL;
    }

    return iter;
}

/*
 * PRIVATE FUNCTIONS
 */

// Resize a garray upwards
static bool dsarray_resize(DSArray *array, size_t cap) {
    assert(array);

    if ((cap < 1) || (array->cap >= cap)) {
        return false;
    }

    void **cache = array->data;
    array->data = calloc(cap, sizeof(void *));
    if (!array->data) {
        array->data = cache;
        return false;
    }

    for (size_t i = 0; i < array->len; i++) {
        array->data[i] = cache[i];
    }

    free(cache);
    array->cap = cap;
    return true;
}

// Free all of the value pointers in a DSArray if a free function was given.
static void dsarray_free(DSArray *array) {
    assert(array);
    bool has_free = (array->free) ? true : false;

    for (size_t i = 0; i < array->len; i++) {
        if (has_free) {
            array->free(array->data[i]);
        }
        array->data[i] = NULL;
    }
}

// Iterate on the next array entry.
bool dsiter_dsarray_next(DSIter *iter, bool advance) {
    assert(iter);
    assert(iter->type == ITER_ARRAY);

    if (DSITER_IS_FINISHED(iter)) {
        return false;
    } else if (DSITER_IS_NEW_ITER(iter)) {
        if (!advance) {
            return (iter->target.array->len > 0);
        } else {
            void *data = dsarray_get(iter->target.array, iter->cur);
            iter->stat = (data) ? DSITER_NORMAL : DSITER_NO_MORE_ELEMENTS;
            return (data != NULL);
        }
    } else {
        if (advance) {
            iter->cur++;
            iter->stat = DSITER_NORMAL;
        }
        void *data = dsarray_get(iter->target.array, iter->cur);
        if (data) {
            return true;
        }

        if (advance) {
            iter->stat = DSITER_NO_MORE_ELEMENTS;
        }
        return false;
    }
}
