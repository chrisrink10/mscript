/**
 * @file iter.h
 *
 * @brief Generic iterator for sequence types.
 *
 * Callers should _not_ modify containers during iteration and behavior
 * will be undefined if a container is modified during iteration. All
 * @c libds containers automatically resize as needed. A resize operation
 * on a container will not update the associated iterator objects (which
 * will leave the iterators in an invalid state).
 *
 * @author Chris Rink <chrisrink10@gmail.com>
 *
 * @copyright 2015 Chris Rink. MIT Licensed.
 */

#ifndef LIBDS_ITER_H
#define LIBDS_ITER_H

#include <stdbool.h>

/**
* @brief Generic iterator object.
*/
typedef struct DSIter DSIter;

/**
* @brief Constants used as sentinel values for @c DSIter objects.
*/
static const int DSITER_NORMAL = 0;
static const int DSITER_NEW_ITERATOR = (1 << 0);
static const int DSITER_NO_MORE_ELEMENTS = (1 << 1);

/**
* @brief Advance the pointer to next element in the collection.
*
* @param iter a @c DSIter object
* @returns @c true if there is another object; @c false otherwise
*/
bool dsiter_next(DSIter *iter);

/**
* @brief Indicates whether there is a next element in the collection without
* advancing the pointer to that element.
*
* @param iter a @c DSIter object
* @returns @c true if there is another object; @c false otherwise
*/
bool dsiter_has_next(DSIter *iter);

/**
* @brief Return the key associated with the current element.
*
* Since DSIter objects are shared generically between DSDict and DSList,
* and keys do not semantically make sense for DSList, this function will
* always return @c NULL if this is a DSList iterator.
*
* @param iter a @c DSIter object
* @returns the current key if @c dsiter_next returned @c true; @c false
*          otherwise
*/
void *dsiter_key(DSIter *iter);

/**
* @brief Return the current value.
*
* Unlike @c dsiter_key, this function should always return a value as
* long as the iterator is still valid (i.e. @c dsiter_next returned @c true).
* Note that @c NULL is a valid value for @c DSIter values, though not for
* @c DSList values.
*
* @param iter a @c DSIter object
* @returns the value associated with the current element pointer
*/
void *dsiter_value(DSIter *iter);

/**
* @brief Return the current enumerated index for the current element.
*
* This is functionally equivalent to keeping your own counter as you iterate.
* For @c DSDict iterators, this value has no meaning as it relates to the
* current element. As the @c DSDict expands over time, the index associated
* with a given element is very likely to change.
*
* @param iter a @c DSIter object
* @returns the enumerated index of the current iteration
*/
size_t dsiter_index(const DSIter *iter);

/**
* @brief Reset a @c DSIter object to a new state.
*
* @param iter a @c DSIter object
*/
void dsiter_reset(DSIter *iter);

/**
* @brief Destroy an iterator.
*
* @param iter a @c DSIter object
*/
void dsiter_destroy(DSIter *iter);

#endif //LIBDS_ITER_H
