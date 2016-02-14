/*****************************************************************************
 * libds :: arraypriv.c
 *
 * Private header for the array data structure.
 *
 * Author:  Chris Rink <chrisrink10@gmail.com>
 *
 * License: MIT (see LICENSE document at source tree root)
 *****************************************************************************/

#ifndef LIBDS_ARRAYPRIV_H
#define LIBDS_ARRAYPRIV_H

#include "libds/array.h"

bool dsiter_dsarray_next(DSIter *iter, bool advance);

#endif //LIBDS_ARRAYPRIV_H
