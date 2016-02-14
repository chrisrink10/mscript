/*****************************************************************************
 * libds :: listpriv.c
 *
 * Private header for list data type.
 *
 * Author:  Chris Rink <chrisrink10@gmail.com>
 *
 * License: MIT (see LICENSE document at source tree root)
 *****************************************************************************/

#ifndef LIBDS_LISTPRIV_H
#define LIBDS_LISTPRIV_H

#include "libds/list.h"

struct node {
    void *data;
    struct node *next;
    struct node *prev;
};

bool dsiter_dslist_next(DSIter *iter, bool advance);

#endif //LIBDS_LISTPRIV_H
