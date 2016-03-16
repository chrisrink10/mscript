/*****************************************************************************
 * libds :: dict.c
 *
 * Dictionary/hash table data structure.
 *
 * Author:  Chris Rink <chrisrink10@gmail.com>
 *
 * License: MIT (see LICENSE document at source tree root)
 *****************************************************************************/

#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include "dictpriv.h"
#include "iterpriv.h"

static const double DSDICT_DEFAULT_LOAD = 0.66;
static const size_t DSDICT_DEFAULT_CAP = 64;
static const size_t DSDICT_DEFAULT_CAPACITY_FACTOR = 2;

/*
 * Prime moduli for hash table capacity
 * - Array index is power of 2 (i.e. index 1 is 2^1 = 2)
 * - Value at index is modulus to use for capacity at indexed power of 2
 * - Powers of 2 given at: https://primes.utm.edu/lists/2small/0bit.html
 */
#define POW2(n, k) ((1 << n) - k)
static const size_t DSDICT_MOD_TABLE[] = {
        1, 2, 3, 7, 13, 31, 61, 127, 251,                           /* Powers 0 through 8 */
        POW2(9, 3), POW2(10, 3), POW2(11, 9), POW2(12, 3),          /* Powers 9 through 12 */
        POW2(13, 1), POW2(14, 3), POW2(15, 19), POW2(16, 15),       /* Powers 13 through 16 */
        POW2(17, 1), POW2(18, 5), POW2(19, 1), POW2(20, 3),         /* Powers 17 through 20 */
        POW2(21, 9), POW2(22, 3), POW2(23, 15), POW2(24, 3),        /* Powers 21 through 24 */
        POW2(25, 39), POW2(26, 5), POW2(27, 39), POW2(28, 57),      /* Powers 25 through 28 */
        POW2(29, 3), POW2(30, 35), INT32_MAX,                       /* Powers 29 through 31 */
};

struct DSDict {
    struct bucket **vals;
    size_t cnt;
    size_t cap;
    dsdict_hash_fn hash;
    dsdict_free_fn keyfree;
    dsdict_free_fn valfree;
    dsdict_compare_fn cmp;
};

static bool dsdict_resize(DSDict *dict, size_t newcap);
static bool transfer_vals(struct bucket **old, size_t oldcap, struct bucket **new, size_t newcap, dsdict_hash_fn hashfn);
static void dsdict_free(DSDict *dict);
static inline size_t compute_index(uint32_t hash, size_t cap);

/*
 * DICTIONARY PUBLIC FUNCTIONS
 */

DSDict *dsdict_new(dsdict_hash_fn hash, dsdict_compare_fn cmpfn, dsdict_free_fn keyfree, dsdict_free_fn valfree) {
    if ((!hash) || (!cmpfn)) { return NULL; }

    DSDict *dict = malloc(sizeof(DSDict));
    if (!dict) {
        return NULL;
    }

    size_t cap = DSDICT_DEFAULT_CAP;
    dict->vals = calloc(cap, sizeof(struct bucket));
    if (!dict->vals) {
        free(dict);
        return NULL;
    }

    dict->cnt = 0;
    dict->cap = DSDICT_DEFAULT_CAP;
    dict->hash = hash;
    dict->keyfree = keyfree;
    dict->valfree = valfree;
    dict->cmp = cmpfn;
    return dict;
}

void dsdict_destroy(DSDict *dict) {
    if (!dict) { return; }
    dsdict_free(dict);
    free(dict->vals);
    free(dict);
}

size_t dsdict_count(const DSDict *dict) {
    assert(dict);
    return dict->cnt;
}

size_t dsdict_cap(const DSDict *dict) {
    assert(dict);
    return dict->cap;
}

void dsdict_foreach(DSDict *dict, dsdict_foreach_fn func) {
    if ((!dict) || (!func)) { return; }

    for (size_t i = 0; i < dict->cnt; i++) {
        if (!dict->vals[i]) { continue; }
        func(dict->vals[i]->key, dict->vals[i]->data);

        struct bucket *next = dict->vals[i]->next;
        while ((next)){
            func(next->key, next->data);
            next = next->next;
        }
    }
}

void dsdict_put(DSDict *dict, void *key, void *val) {
    if ((!dict) || (!key)) { return; }

    unsigned int hash = dict->hash(key);
    size_t place = compute_index(hash, dict->cap);

    // Get reference to place and see if there is data there;
    // if not, just set the data
    struct bucket *cur = dict->vals[place];
    if (!cur) {
        dict->vals[place] = malloc(sizeof(struct bucket));
        if (!dict->vals[place]) { return; }
        cur = dict->vals[place];
        goto dsdict_put_op;
    }

    // If there was data, check if it's the same value;
    // if so, we can overwrite it and we're done
    if ((cur->hash == hash) && (dict->cmp(cur->key, key) == 0)) {
        if (dict->valfree) { dict->valfree(cur->data); }
        cur->data = val;
        goto cleanup_dsdict_put;
    }

    // Otherwise traverse linked list and check if we can
    // overwrite any connected nodes
    struct bucket *prev = cur;
    cur = cur->next;
    while ((cur)) {
        prev = cur;
        if ((cur->hash == hash) && (dict->cmp(cur->key, key) == 0)) {
            if (dict->valfree) { dict->valfree(cur->data); }
            cur->data = val;
            goto cleanup_dsdict_put;
        }
        cur = cur->next;
    }

    // If we made it this far, we need to add a new node at cur
    prev->next = malloc(sizeof(struct bucket));
    cur = prev->next;

    // Actually perform the titular "put"
dsdict_put_op:
    if (!cur) { return; }

    cur->hash = hash;
    cur->key = key;
    cur->data = val;
    cur->next = NULL;
    dict->cnt++;

    // Clean up and decide if we need to resize no
cleanup_dsdict_put: {
        double load = ((double)dict->cnt / dict->cap);
        if (load >= DSDICT_DEFAULT_LOAD) {
            dsdict_resize(dict, dict->cap * DSDICT_DEFAULT_CAPACITY_FACTOR);
        }
    }
    return;
}

void *dsdict_get(const DSDict *dict, void *key) {
    if ((!dict) || (!key)) { return NULL; }

    unsigned int hash = dict->hash(key);
    size_t place = compute_index(hash, dict->cap);

    struct bucket *cur = dict->vals[place];
    if (!cur) { return NULL; }
    if ((cur->hash == hash) && (dict->cmp(cur->key, key) == 0)) {
        return cur->data;
    }

    cur = cur->next;
    while ((cur)) {
        if ((cur->hash == hash) && (dict->cmp(cur->key, key) == 0)) {
            return cur->data;
        }
        cur = cur->next;
    }

    return NULL;
}

void *dsdict_del(DSDict *dict, void *key) {
    if ((!dict) || (!key)) { return NULL; }

    unsigned int hash = dict->hash(key);
    size_t place = compute_index(hash, dict->cap);

    struct bucket *cur = dict->vals[place];
    if (!cur) { return NULL; }
    if ((cur->hash == hash) && (dict->cmp(cur->key, key) == 0)) {
        void *cache = cur->data;
        dict->vals[place] = cur->next;
        free(cur);
        dict->cnt--;
        return cache;
    }

    cur = cur->next;
    while ((cur)) {
        if ((cur->hash == hash) && (dict->cmp(cur->key, key) == 0)) {
            void *cache = cur->data;
            dict->vals[place] = cur->next;
            free(cur);
            dict->cnt--;
            return cache;
        }
        cur = cur->next;
    }

    return NULL;
}

DSIter* dsdict_iter(DSDict *dict) {
    if (!dict) { return NULL; }

    DSIter *iter = dsiter_priv_new(ITER_DICT, dict);
    if (!iter) {
        return NULL;
    }

    return iter;
}

/*
 * PRIVATE FUNCTIONS
 */

// Resize a DSDict upwards
static bool dsdict_resize(DSDict *dict, size_t newcap) {
    assert(dict);

    if ((newcap < 1) || (dict->cap >= newcap)) {
        return false;
    }

    // Make a new bucket and cache the old values so we can transfer them
    struct bucket **cache = dict->vals;
    dict->vals = calloc(newcap, sizeof(struct bucket));
    if (!dict->vals) {
        dict->vals = cache;
        return false;
    }

    // Transfer all of the old values into the new buckets
    size_t oldcap = dict->cap;
    if (!transfer_vals(cache, oldcap, dict->vals, newcap, dict->hash)) {
        free(dict->vals);
        dict->vals = cache;
        return false;
    }
    dict->cap = newcap;

    // Free the cached buckets, but do not free key/value pairs
    free(cache);
    return true;
}

// Given a hash value and a capacity, compute the place of the element in the array.
static inline size_t compute_index(uint32_t hash, size_t cap) {
    double powerf = floor(log2((double)cap));
    assert(powerf >= 0);
    size_t power = (size_t)powerf;
    size_t mod = (power <= 31) ? DSDICT_MOD_TABLE[power] : (size_t)cap;
    return (hash % mod);
}

// Transfer values from the old DSDict bucket cache to the new bucket
static bool transfer_vals(struct bucket **old, size_t oldcap, struct bucket **new, size_t newcap, dsdict_hash_fn hashfn) {
    assert(old);
    assert(new);
    assert(hashfn);

    // Iterate on every element of the old bucket
    for (size_t i = 0; i < oldcap; i++) {
        // Iterate on every hash table element in the old dictionary
        struct bucket *curold = old[i];
        while (curold) {
            // Compute the new placement for the current element
            size_t place = compute_index(curold->hash, newcap);

            // Get reference to place and see if there is data there;
            // if so, we need to traverse the linked list to get the last
            // element and insert the hash table into that
            struct bucket *curnew = new[place];
            if (curnew) {
                struct bucket *prev = curnew;
                while ((curnew)) {
                    prev = curnew;
                    curnew = curnew->next;
                }
                prev->next = curold;
                curold = prev->next->next;
                prev->next->next = NULL;
            } else {
                // Transfer the old element to the new slot
                new[place] = curold;
                curold = new[place]->next;
                new[place]->next = NULL;
            }
        }

        old[i] = NULL;
    }

    return true;
}

// Free all of the value pointers in a DSDict if a free function was given.
static void dsdict_free(DSDict *dict) {
    assert(dict);
    bool free_keys = (dict->keyfree) ? true : false;
    bool free_vals = (dict->valfree) ? true : false;

    for (size_t i = 0; i < dict->cap; i++) {
        if (!dict->vals[i]) { continue; }
        if (free_keys) {
            dict->keyfree(dict->vals[i]->key);
        }
        if (free_vals) {
            dict->valfree(dict->vals[i]->data);
        }

        struct bucket *cur = NULL;
        struct bucket *next = dict->vals[i]->next;
        while ((next)){
            if (free_keys) {
                dict->keyfree(next->key);
            }
            if (free_vals) {
                dict->valfree(next->data);
            }
            cur = next;
            next = next->next;
            free(cur);
        }

        free(dict->vals[i]);
        dict->vals[i] = NULL;
    }
}

// Iterate on the next dictionary entry.
bool dsiter_dsdict_next(DSIter *iter, bool advance) {
    assert(iter);
    assert(iter->type == ITER_DICT);

    // If we already know there are no more elements, quit
    if (DSITER_IS_FINISHED(iter)) {
        return false;
    }

    // Get the initial node pointer
    if (DSITER_IS_NEW_ITER(iter)) {
        DSDict *dict = iter->target.dict;

        // We do not need to traverse any linked lists
        // since this is explicitly the first node
        for (size_t i = 0; i < dict->cap; i++) {
            if (dict->vals[i]) {
                if (advance) {
                    iter->node.dict = dict->vals[i];
                    iter->cur = i;
                    iter->stat = DSITER_NORMAL;
                }
                return true;
            }
        }

        // No elements found in this dictionary
        if (advance) {
            iter->stat = DSITER_NO_MORE_ELEMENTS;
        }
        return false;
    }

    // If there is a next node, set our next pointer to that
    if (iter->node.dict->next) {
        if (advance) {
            iter->node.dict = iter->node.dict->next;
        }
        return true;
    }

    // Otherwise, traverse through the array to find the next pointer
    DSDict *dict = iter->target.dict;
    for (size_t i = (iter->cur + 1); i < dict->cap; i++) {
        if (dict->vals[i]) {
            if (advance) {
                iter->cur = i;
                iter->node.dict = dict->vals[i];
            }
            return true;
        }
    }

    // If we get here, there is no more data in the dict
    if (advance) {
        iter->stat = DSITER_NO_MORE_ELEMENTS;
        iter->node.dict = NULL;
    }
    return false;
}
