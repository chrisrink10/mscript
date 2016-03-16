/**
 * @file array.h
 *
 * @brief Automatically resizing array/stack implementation.
 *
 * @author Chris Rink <chrisrink10@gmail.com>
 *
 * @copyright 2015 Chris Rink. MIT Licensed.
 */

#ifndef LIBDS_ARRAY_H
#define LIBDS_ARRAY_H

#include <stdbool.h>
#include <stddef.h>
#include "libds/iter.h"

/**
* @brief Auto-resizing generic array object.
*
* DSArray objects are typically resized by @c DSARRAY_CAPACITY_FACTOR
* whenever a resize is necessary using the API.
*/
typedef struct DSArray DSArray;

/**
* @brief The default capacity of a @c DSArray.
*/
static const size_t DSARRAY_DEFAULT_CAPACITY = 10;

/**
* @brief The factor by which a @c DSArray is resized when needed
*/
static const size_t DSARRAY_CAPACITY_FACTOR = 2;

/**
* @brief Comparator function used in a @c DSArray to sort and search.
*/
typedef int (*dsarray_compare_fn)(const void*, const void*);

/**
* @brief Free function used in a @c DSArray free remaining elements when
* the array is destroyed.
*/
typedef void (*dsarray_free_fn)(void*);

/**
* @brief Errors returned from @c DSArray functions returning indices.
*/
static const int DSARRAY_NOT_FOUND = -1;
static const int DSARRAY_NULL_POINTER = -2;
static const int DSARRAY_NO_CMP_FUNC = -3;

/**
* @brief Create a new @c DSArray object with @c DSARRAY_DEFAULT_CAPACITY
* slots and the given comparator and free function.
*
* Neither function pointer is required to create a new array. If the caller
* does not specify a @c dsarray_compare_fn, then @c dsarray_sort will become
* a no-op. Likewise, if no @c dsarray_free_fn is specified, then the array
* will not free array elements when it is destroyed.
*
* @param cmpfn a function which can compare two array elements
* @param freefn a function which can free a array element
* @returns a new @c DSArray object or @c NULL if memory could not be
*          allocated
*/
DSArray *dsarray_new(dsarray_compare_fn cmpfn, dsarray_free_fn freefn);

/**
* @brief Create a new @c DSArray object with @c cap slots and the given
* comparator and free function.
*
* Neither function pointer is required to create a new array. If the caller
* does not specify a @c dsarray_compare_fn, then @c dsarray_sort will become
* a no-op. Likewise, if no @c dsarray_free_fn is specified, then the array
* will not free array elements when it is destroyed.
*
* @param cap the starting capacity of the @c DSArray
* @param cmpfn a function which can compare two array elements
* @param freefn a function which can free a array element
* @returns a new @c DSArray object or @c NULL if memory could not be
*          allocated
*/
DSArray *dsarray_new_cap(size_t cap, dsarray_compare_fn cmpfn, dsarray_free_fn freefn);

/**
* @brief Destroy a @c DSArray object.
*
* If a @c dsarray_free_fn was specified when the array was created, it will
* be called on each element in the array. Otherwise, only the array object
* itself and any references it owned will be destroyed.
*/
void dsarray_destroy(DSArray *array);

/**
* @brief Return the length of a @c DSArray.
*
* @param array a @c DSArray object
* @returns the number of elements in @c array
*/
size_t dsarray_len(const DSArray *array);

/**
* @brief Return the capacity of a @c DSArray .
*
* @param array a @c DSArray object
* @returns the number of elements that @c array can hold
*/
size_t dsarray_cap(const DSArray *array);

/**
* @brief Return the object stored at the given index.
*
* @param array a @c DSArray object
* @param index numeric index of the element to get
* @returns @c NULL if the index has no element or is invalid; the
*          object otherwise
*/
void *dsarray_get(const DSArray *array, size_t index);

/**
* @brief Return the top element in the array without popping it.
*
* @param array a @c DSArray object
* @returns @c NULL if there are no elements in the array; the top of
*          the stack (array) otherwise
*/
void *dsarray_top(const DSArray *array);

/**
* @brief Perform the given function on each object in the array.
*
* The given function is called for every array element including @c NULL
* elements. The given function should be able to operate on @c NULL
* pointers.
*
* @param array a @c DSArray object
* @param func a function taking a void pointer which returns nothing
*/
void dsarray_foreach(DSArray *array, void (*func)(void*));

/**
* @brief Append an element to the end of the array.
*
* @param array a @c DSArray object
* @param elem the element to add to the array
* @returns @c false if the element is @c NULL or the array cannot be resized;
*          @c true otherwise
*/
bool dsarray_append(DSArray *array, void *elem);

/**
* @brief Extend the first @c DSArray with the elements of the second.
*
* This function sets each element pointer in @c other to @c NULL
* and sets the length to 0, to avoid any attempts to double-free
* memory, which would result in undefined behavior.
*
* Callers will still be required to destroy the @c other array object,
* though it will no longer contain any references.
*
* @param array the destination @c DSArray object
* @param other the source @c DSArray object
* @returns @c false if @c array or @c other were @c NULL or if @c array could
*          not be resized; @c true otherwise
*/
bool dsarray_extend(DSArray *array, DSArray *other);

/**
* @brief Insert the given element at the specified index.
*
* Elements may only be inserted between index 0 and index (length - 1).
*
* @param array a @c DSArray object
* @param elem the element to insert into @c array
* @param index the index to insert @c elem into @c array
* @returns @c false if @c elem is @c NULL or @c index is invalid or the
*          array could not be resized; @c true otherwise
*/
bool dsarray_insert(DSArray *array, void *elem, size_t index);

/**
* @brief Remove the first element in the array matching the given element
* and free that element.
*
* To remove an element with this function, the caller would have had to
* specify a comparator function when this array was created. The element
* will not be freed when removed from the array by this function.
*
* @param array a @c DSArray object
* @param elem the element to remove from the array
* @returns a pointer to the element or @c NULL if the element cannot be
*          found or no comparator function was specified
*/
void *dsarray_remove(DSArray *array, void *elem);

/**
* @brief Remove the element at the given index and return it.
*
* The element will not be freed when removed from the array by this function.
*
* @param array a @c DSArray object
* @param index the index of the element to remove
* @returns a pointer to the element or @c NULL if the element cannot be
*          found or no comparator function was specified or the index was
*          invalid
*/
void *dsarray_remove_index(DSArray *array, size_t index);

/**
* @brief Pop the top element from the array.
*
* The element will not be freed when removed from the array by this function.
*
* @param array a @c DSArray object
* @returns a pointer to the top element or @c NULL if the array is empty
*/
void *dsarray_pop(DSArray *array);

/**
* @brief Clear the entire array, freeing elements as they are removed.
*
* Array elements are only freed if the free function was provided by the
* caller when the array was created.
*
* @param array a @c DSArray object
*/
void dsarray_clear(DSArray *array);

/**
* @brief Return the first index of the given element.
*
* To find an element with this function, the caller would have had to
* specify a comparator function when this array was created.
*
* @param array a @c DSArray object
* @param elem the element to find in the array
* @returns the index of the element or @c DSARRAY_NOT_FOUND if the element
*          is not found in the array
*/
int dsarray_index(const DSArray *array, void *elem);

/**
* @brief Sort the array in ascending order using the given comparator function.
*
* To sort the array, the caller would have had to specify a comparator
* function when this array was created. The sort will be performed using the
* comparator and the C standard library function @c qsort.
*
* @param array a @c DSArray object
*/
void dsarray_sort(DSArray *array);

/**
* @brief Reverse the array in place.
*
* @param array a @c DSArray object
*/
void dsarray_reverse(DSArray *array);

/**
* @brief Create a new @c DSIter on this array.
*
* @param array a @c DSArray object
*/
DSIter* dsarray_iter(DSArray *array);

#endif //LIBDS_ARRAY_H
