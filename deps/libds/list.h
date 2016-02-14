/**
 * @file list.h
 *
 * @brief Linked list/queue implementation.
 *
 * @author Chris Rink <chrisrink10@gmail.com>
 *
 * @copyright 2015 Chris Rink. MIT Licensed.
 */

#ifndef LIBDS_LIST_H
#define LIBDS_LIST_H

#include <stdbool.h>
#include <stddef.h>
#include "iter.h"

/**
* @brief Linked list object.
*/
typedef struct DSList DSList;

/**
* @brief Comparator function used in a @c DSList to sort and search.
*/
typedef int (*dslist_compare_fn)(void *, void *);

/**
* @brief Free function used in a @c DSList free remaining elements when
* the list is destroyed.
*/
typedef void (*dslist_free_fn)(void*);

/**
* @brief Errors returned from @c DSList functions returning indices.
*/
static const int DSLIST_NOT_FOUND = -1;
static const int DSLIST_NULL_POINTER = -2;
static const int DSLIST_NO_CMP_FUNC = -3;

/**
* @brief Create a new @c DSList object with the given comparator and
* free function.
*
* Neither function pointer is required to create a new list. If the caller
* does not specify a @c dslist_compare_fn, then @c dslist_index will become
* a no-op. Likewise, if no @c dslist_free_fn is specified, then the list
* will not free list elements when it is destroyed.
*
* @param cmpfn a function which can compare two list elements
* @param freefn a function which can free a list element
* @returns a new @c DSList object or @c NULL if memory could not be
*          allocated
*/
DSList *dslist_new(dslist_compare_fn cmpfn, dslist_free_fn freefn);

/**
* @brief Destroy a @c DSList object.
*
* If a @c dslist_free_fn was specified when the list was created, it will
* be called on each element in the array. Otherwise, only the list object
* itself and any references it owned will be destroyed.
*/
void dslist_destroy(DSList *list);

/**
* @brief Return the length of a @c DSList.
*
* @param list a @c DSList object
* @returns the number of elements in @c list
*/
size_t dslist_len(DSList *list);

/**
* @brief Return the object stored at the given index.
*
* @param list a @c DSList object
* @param index numeric index of the element to get
* @returns @c NULL if the index has no element or is invalid; the
*          object otherwise
*/
void* dslist_get(DSList *list, size_t index);

/**
* @brief Perform the given function on each object in the list.
*
* @param list a @c DSList object
* @param func a function taking a void pointer which returns nothing
*/
void dslist_foreach(DSList *list, void (*func)(void*));

/**
* @brief Append an element to the end of the list.
*
* @param list a @c DSList object
* @param elem the element to add to the array
* @returns @c false if the element is @c NULL or the node cannot be created;
*          @c true otherwise
*/
bool dslist_append(DSList *list, void *elem);

/**
* @brief Extend the first @c DSList with the elements of the second.
*
* This function sets each element pointer in @c other to @c NULL
* and sets the length to 0, to avoid any attempts to double-free
* memory, which would result in undefined behavior.
*
* Callers will still be required to destroy the @c other list object,
* though it will no longer contain any references.
*
* @param list the destination @c DSList object
* @param other the source @c DSList object
* @returns @c false if @c list or @c other were @c NULL ; @c true otherwise
*/
bool dslist_extend(DSList *list, DSList *other);

/**
* @brief Insert the given element at the specified index.
*
* Elements may only be inserted between index 0 and index length.
*
* @param list a @c DSList object
* @param elem the element to insert into @c list
* @param index the index to insert @c elem into @c list
* @returns @c false if @c elem is @c NULL or @c index is invalid or the
*          node could not be created; @c true otherwise
*/
bool dslist_insert(DSList *list, void *elem, size_t index);

/**
* @brief Remove the first element in the list matching the given element
* and free that element.
*
* To remove an element with this function, the caller would have had to
* specify a comparator function when this list was created. The element
* will not be freed when removed from the list by this function.
*
* @param list a @c DSList object
* @param elem the element to remove from the list
* @returns a pointer to the element or @c NULL if the element cannot be
*          found or no comparator function was specified
*/
void* dslist_remove(DSList *list, void *elem);

/**
* @brief Remove the element at the given index and return it.
*
* The element will not be freed when removed from the list by this function.
*
* @param list a @c DSList object
* @param index the index of the element to remove
* @returns a pointer to the element or @c NULL if the element cannot be
*          found or no comparator function was specified or the index was
*          invalid
*/
void* dslist_remove_index(DSList *list, size_t index);

/**
* @brief Enqueue the given element in the list.
*
* @param list a @c DSList object
* @param elem the element to insert into @c list
* @returns @c false if @c elem is @c NULL or @c index is invalid or the
*          node could not be created; @c true otherwise
*/
bool dslist_enqueue(DSList *list, void *elem);

/**
* @brief Dequeue the front (head) element in the list.
*
* Callers are responsible for freeing the element after dequeueing.
*
* @param list a @c DSList object
* @returns @c NULL if the list is empty; a pointer to the head element
*          otherwise
*/
void* dslist_dequeue(DSList *list);

/**
* @brief Pop the top (foot) element from the list.
*
* The element will not be freed when removed from the list by this function.
*
* @param list a @c DSList object
* @returns a pointer to the top element or @c NULL if the list is empty
*/
void* dslist_pop(DSList *list);

/**
* @brief Clear the entire list, freeing elements as they are removed.
*
* List elements are only freed if the free function was provided by the
* caller when the list was created.
*
* @param list a @c DSList object
*/
void dslist_clear(DSList *list);

/**
* @brief Return the first index of the given element.
*
* To find an element with this function, the caller would have had to
* specify a comparator function when this array was created.
*
* @param list a @c DSList object
* @param elem the element to find in the list
* @returns the index of the element or @c DSLIST_NOT_FOUND if the element
*          is not found in the list
*/
int dslist_index(DSList *list, void *elem);

/**
* @brief Reverse the list in place.
*
* @param list a @c DSList object
*/
void dslist_reverse(DSList *list);

/**
* @brief Create a new iterator for the given list.
*
* @param list a @c DSList object
* @returns NULL if no list was provided or memory could not be allocated;
*          a new @c DSIter otherwise
*/
DSIter *dslist_iter(DSList *list);

#endif //LIBDS_LIST_H
