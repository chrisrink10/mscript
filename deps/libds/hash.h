/**
 * @file hash.h
 *
 * @brief String hashing algorithms.
 *
 * @author Chris Rink <chrisrink10@gmail.com>
 *
 * @copyright 2015 Chris Rink. MIT Licensed.
 */

#ifndef LIBDS_HASH_H
#define LIBDS_HASH_H

#include <stdint.h>

/**
* @brief Hash a string.
*
* This algorithm was taken from a StackOverflow post and attributed to
* Paul Larson at Microsoft Research. The origin post is
* [here](http://stackoverflow.com/a/629127/1582301).
*
* @param str a @c NUL terminated C string
* @returns a hash value
*/
uint32_t hash_larson(const char *str);

/**
* @brief Hash a string.
*
* The FNV and FNV1a hashing algorithms are explored in more detail
* [here](http://www.isthe.com/chongo/tech/comp/fnv/index.html).
*
* @param str a @c NUL terminated C string
* @returns a hash value
*/
uint32_t hash_fnv1(const char *str);

/**
* @brief Hash a string.
*
* The djb2 hashing algorithm was described
* [here](http://www.cse.yorku.ca/~oz/hash.html).
*
* @param str a @c NUL terminated C string
* @returns a hash value
*/
uint32_t hash_djb2(const char *str);

/**
* @brief Hash a string.
*
* The sdbm hashing algorithm was described
* [here](http://www.cse.yorku.ca/~oz/hash.html).
*
* @param str a @c NUL terminated C string
* @returns a hash value
*/
uint32_t hash_sdbm(const char *str);

#endif //LIBDS_HASH_H
