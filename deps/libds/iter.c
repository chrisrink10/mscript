/*****************************************************************************
 * libds :: iter.c
 *
 * Generic iterator for sequence types.
 *
 * Author:  Chris Rink <chrisrink10@gmail.com>
 *
 * License: MIT (see LICENSE document at source tree root)
 *****************************************************************************/

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include "iterpriv.h"

static bool set_target(DSIter *iter, void *val);
static bool set_node(DSIter *iter, void *val);

/*
 * ITERATOR PUBLIC FUNCTIONS
 */

bool dsiter_next(DSIter *iter) {
    if (!iter) { return false; }

    switch (iter->type) {
        case ITER_ARRAY:
            return dsiter_dsarray_next(iter, true);
        case ITER_DICT:
            return dsiter_dsdict_next(iter, true);
        case ITER_LIST:
            return dsiter_dslist_next(iter, true);
    }

    return false;
}

bool dsiter_has_next(DSIter *iter) {
    if (!iter) { return false; }

    switch (iter->type) {
        case ITER_ARRAY:
            return dsiter_dsarray_next(iter, false);
        case ITER_DICT:
            return dsiter_dsdict_next(iter, false);
        case ITER_LIST:
            return dsiter_dslist_next(iter, false);
    }

    return false;
}

void *dsiter_key(DSIter *iter) {
    if (!iter) { return NULL; }

    switch(iter->type) {
        case ITER_ARRAY:
            return NULL;
        case ITER_DICT:
            return (iter->node.dict) ? (iter->node.dict->key) : NULL;
        case ITER_LIST:
            return NULL;
    }

    return NULL;
}

void *dsiter_value(DSIter *iter) {
    if (!iter) { return NULL; }

    switch(iter->type) {
        case ITER_ARRAY:
            return dsarray_get(iter->target.array, iter->cur);
        case ITER_DICT:
            return (iter->node.dict) ? (iter->node.dict->data) : NULL;
        case ITER_LIST:
            return (iter->node.list) ? (iter->node.list->data) : NULL;
    }

    return NULL;
}

size_t dsiter_index(const DSIter *iter) {
    if (!iter) { return SIZE_MAX; }
    return iter->cur;
}

void dsiter_reset(DSIter *iter) {
    if (!iter) { return; }

    iter->cur = 0;
    iter->stat = DSITER_NEW_ITERATOR;
    set_node(iter, NULL);
}

void dsiter_destroy(DSIter *iter) {
    if (!iter) { return; }

    set_target(iter, NULL);
    set_node(iter, NULL);
    free(iter);
}

/*
 * PRIVATE FUNCTIONS
 */

// Create a new DSIter of the given type on the given target.
DSIter* dsiter_priv_new(enum IterType type, void *target) {
    DSIter *iter = malloc(sizeof(DSIter));
    if (!iter) {
        return NULL;
    }

    iter->type = type;
    iter->cur = 0;
    iter->stat = DSITER_NEW_ITERATOR;
    set_node(iter, NULL);

    if (!set_target(iter, target)) {
        dsiter_destroy(iter);
        return NULL;
    }

    return iter;
}

// Set the iterator target based on the iterator type
static bool set_target(DSIter *iter, void *val) {
    assert(iter);

    switch (iter->type) {
        case ITER_ARRAY:
            iter->target.array = val;
            return true;
        case ITER_DICT:
            iter->target.dict = val;
            return true;
        case ITER_LIST:
            iter->target.list = val;
            return true;
        default:
            return false;
    }
}

// Set the current iterator node based on iterator type
static bool set_node(DSIter *iter, void *val) {
    assert(iter);

    switch (iter->type) {
        case ITER_ARRAY:
            return true;
        case ITER_DICT:
            iter->node.dict = val;
            return true;
        case ITER_LIST:
            iter->node.list = val;
            return true;
        default:
            return false;
    }
}
