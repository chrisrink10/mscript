/**
 * @file buffer.h
 *
 * @brief Automatically resizing string/binary buffer.
 *
 * @author Chris Rink <chrisrink10@gmail.com>
 *
 * @copyright 2015 Chris Rink. MIT Licensed.
 */

#ifndef LIBDS_BUFFER_H
#define LIBDS_BUFFER_H

#include <stdbool.h>
#include <stddef.h>

/**
* @brief Auto-resizing character buffer object.
*
* DSBuffer objects are typically resized by @c DSBUFFER_CAPACITY_FACTOR
* whenever a resize is necessary using the API.
*/
typedef struct DSBuffer DSBuffer;

/**
* @brief The factor by which a @c DSBuffer is resized when needed
*/
static const size_t DSBUFFER_CAPACITY_FACTOR = 2;

/**
* @brief The minimum size of a @c DSBuffer.
*/
static const size_t DSBUFFER_MINIMUM_CAPACITY = 20;

/**
* @brief Error codes for @c DSBuffer functions.
*/
static const int DSBUFFER_CHAR_NOT_FOUND = -1;

/**
* @brief Create a new @c DSBuffer from the given character array.
*
* It is safe to use @c DSBuffer objects with string literals, since
* the contents are copied into the buffer's internal memory and
* thus a call to @c DSBuffer will not attempt to free static
* or stack memory.
*
* @param value a standard C @c NUL terminated string; may be a static
*              or stack-allocated string
* @returns a new @c DSBuffer object or @c NULL if memory could not
*          be allocated or if the length of @c value is less than 1
*/
DSBuffer *dsbuf_new(const char *value);

/**
* @brief Create a new @c DSBuffer from the given character array with the
* given length. This will permit non-null terminated strings.
*
* It is safe to use @c DSBuffer objects with string literals, since
* the contents are copied into the buffer's internal memory and
* thus a call to @c dsbuf_destroy will not attempt to free static
* or stack memory.
*
* @param value a standard C @c NUL terminated string; may be a static
*              or stack-allocated string
* @param len the length of the input string
* @returns a new @c DSBuffer object or @c NULL if memory could not
*          be allocated or if the length of @c value is less than 1
*/
DSBuffer *dsbuf_new_l(const char *value, size_t len);

/**
* @brief Create a new @c DSBuffer with the given capacity which can be used
* as a character buffer.
*
* The bytes of the buffer will be set to 0. The minimum capacity permitted
* is 20.
*
* @param cap the requested capacity for the @c DSBuffer
* @returns a new @c DSBuffer object with the requested capacity or @c NULL
*          if memory could not be allocated or if the length of @c value is less than 1
*/
DSBuffer *dsbuf_new_buffer(size_t cap);

/**
* @brief Dispose of the @c DSBuffer object.
*
* Calling this function multiple times on the same pointer may have
* undefined behavior (much like @c free ).
*
* @param str a @c DSBuffer object to free
*/
void dsbuf_destroy(DSBuffer *str);

/**
* @brief Duplicate a @c DSBuffer object
*
* @param str a @c DSBuffer object to copy
* @returns a copy of the given @c DSBuffer object or NULL if a copy could
*          not be made for some reason (e.g. memory allocation issues)
*/
DSBuffer *dsbuf_dup(const DSBuffer *str);

/**
* @brief Return the length in number of bytes in the @c DSBuffer.
*
* Note that this is not the UTF-8 length.
*
* @param str a @c DSBuffer object
* @returns the number of used bytes in @c str
*/
size_t dsbuf_len(const DSBuffer *str);

/**
* @brief Return the capacity in number of bytes in the @c DSBuffer.
*
* @param str a @c DSBuffer object
* @returns the number of available bytes in @c str
*/
size_t dsbuf_cap(const DSBuffer *str);

/**
* @brief Append @c newc to @c str, resizing @c str if necessary.
*
* Callers will still be responsible for destroying both of @c str and
* @c newc. The append operation may fail if the buffer in @c str cannot
* be resized. This operation does not guarantee to @c NUL terminate the
* resultant filled buffer portion. However, the length of @c str will
* correctly reflect the new combined length.
*
* @param str a @c DSBuffer object
* @param newc a @c DSBuffer object append to @c str
* @returns @c true if the append succeeds; @c false otherwise
*/
bool dsbuf_append(DSBuffer *str, DSBuffer *newc);

/**
* @brief Append the new @c char to @c str, resizing if necessary.
*
* The append operation may fail if the buffer in @c str cannot be
* resized.
*
* @param str @c DSBuffer object
* @param newc a @c char to append to @c str
* @returns @c true if the append succeeds; @c false otherwise
*/
bool dsbuf_append_char(DSBuffer *str, int newc);

/**
* @brief Append an existing C-string to a @c DSBuffer object.
*
* The append operation may fail if the buffer in @c str cannot be
* resized. It is safe to use @c DSBuffer objects with string literals,
* since the contents are copied into the buffer's internal memory and
* thus a call to @c dsbuf_destroy will not attempt to free static
* or stack memory.
*
* @param str a @c DSBuffer object
* @param newstr a @c NUL terminated C string
* @param @c true if the append succeeds; @c false otherwise
*/
bool dsbuf_append_str(DSBuffer *str, const char *newstr);

/**
* @brief Return the @c char at the given position.
*
* @param str a @c DSBuffer object
* @param pos a valid index in [0, len)
* @returns @c DSBUFFER_CHAR_NOT_FOUND if the position is greater than or
*          equal to the length of the string or is 0; the character otherwise
*/
int dsbuf_char_at(const DSBuffer *str, size_t pos);

/**
* @brief Return a substring of the given buffer.
*
* Buffer starts at the given position and returning up to @c len
* characters.
*
* @param str a @c DSBuffer object
* @param start the starting index of the substring
* @param len the length of the substring in bytes
* @returns @c NULL if (@c start < 0) or (@c start > length of string) or
*          the length would exceed the length of the string or memory could
*          not be allocated; the substring as a @c DSBuffer otherwise
*/
DSBuffer *dsbuf_substr(const DSBuffer *str, size_t start, size_t len);

/**
* @brief Check if two @c DSBuffer objects are equal (but not the same).
*
* This function first checks basic things such as the internal length
* and capacity properties first before performing a @c strcmp.
*
* @param str a @c DSBuffer object
* @param other another @c DSBuffer object
* @returns @c true if @c str is equal to @c other
*/
bool dsbuf_equals(const DSBuffer *str, const DSBuffer *other);

/**
* @brief Check if the internal buffer of a @c DSBuffer is equal to a standard
* C string.
*
* @param str a @c DSBuffer object
* @param other a standard C string
* @returns @c true if the internal buffer of @c str is binary equal to @c other
*/
bool dsbuf_equals_char(const DSBuffer *str, const char *other);

/**
* @brief Return the pointer to the internal character array.
*
* @c DSBuffer objects are not bound to be terminated with @c NUL bytes
* like a C string. Other string functions will not be able to read
* beyond any interspersed @c NUL bytes.
*
* @param str a @c DSBuffer object
* @returns a pointer to the internal character buffer
*/
const char *dsbuf_char_ptr(const DSBuffer *str);

/**
* @brief Return a @c char array corresponding to the underlying string.
*
* @c DSBuffer objects are not bound to be terminated with @c NUL bytes
* like a C string. This function returns the body of the string up until
* the first @c NUL byte. Callers are required to call @c free on the
* return value from this function.
*
* @param str a @c DSBuffer object
* @returns a copy of the internal buffer as a C string up to the first
*          @c NUL byte
*/
char *dsbuf_to_char_array(const DSBuffer *str);

/**
* @brief Return a hash of the underlying string.
*
* This function is intended to be used for hashing @c DSBuffer objects
* for a @c DSDict key, so it uses a generic void pointer parameter.
*
* @param str a @c DSBuffer object
* @returns a hash of the internal buffer
*/
unsigned int dsbuf_hash(const DSBuffer *str);

/**
* @brief Compare two DSBuffers.
*
* @param left a @c DSBuffer object
* @param right a @c DSBuffer object
* @returns @c INT_MIN if left is @c NULL or @c INT_MAX if right is @c NULL;
*          a value less than zero if @c left is lexicographically less than
*          @c right or @c left is shorter than @c right; a value greater than
*          zero if @c right is lexicographically less than @c left or
*          @c right is shorter than @c left; 0 if the two strings are the
*          same length and equal
*/
int dsbuf_compare(const DSBuffer *left, const DSBuffer *right);

/*
* @brief Check if the internal buffer contains a valid UTF-8 sequence.
*
* @param buf a @c DSBuffer object
* @param l output the length in UTF-8 characters; use @c NULL to ignore
* @returns @c true if the buffer is valid UTF-8; @c false otherwise
*/
bool dsbuf_utf8_validate(const DSBuffer *buf, size_t *l);

/*
* @brief Return the length of the string in UTF-8 characters.
*
* This function will return before reaching the end of the string if it
* encounters invalid UTF-8 characters. It may be more useful to use the
* function @c dsbuf_utf8_validate and read the output size parameter if
* it returns @c true since you will know that the string contains valid
* UTF-8 and the output parameter represents the true UTF-8 length.
*
* @param buf a @c DSBuffer object
* @returns 0 if @c buf is @c NULL; the length in UTF-8 characters otherwise
*/
size_t dsbuf_utf8_len(const DSBuffer *buf);

#endif //LIBDS_BUFFER_H
