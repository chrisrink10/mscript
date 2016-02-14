/*****************************************************************************
 * libds :: iterpriv.h
 *
 * Private header for the iterator object.
 *
 * Author:  Chris Rink <chrisrink10@gmail.com>
 *
 * License: MIT (see LICENSE document at source tree root)
 *****************************************************************************/

#ifndef LIBDS_ITERPRIV_H
#define LIBDS_ITERPRIV_H

#include "arraypriv.h"
#include "dictpriv.h"
#include "listpriv.h"

enum IterType {
    ITER_ARRAY,
    ITER_DICT,
    ITER_LIST,
};

union IterTarget {
    DSArray *array;
    DSDict *dict;
    DSList *list;
};

union IterNode {
    struct bucket *dict;
    struct node *list;
};

struct DSIter {
    enum IterType type;
    union IterTarget target;
    size_t cur;
    union IterNode node;
    int stat;
};

DSIter* dsiter_priv_new(enum IterType type, void *target);
#define DSITER_IS_NEW_ITER(iter) (iter->stat == DSITER_NEW_ITERATOR)
#define DSITER_IS_FINISHED(iter) (iter->stat == DSITER_NO_MORE_ELEMENTS)

#endif //LIBDS_ITERPRIV_H
