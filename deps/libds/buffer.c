/**
 * @file buffer.c
 *
 * @brief Automatically resizing string/binary buffer.
 *
 * @author Chris Rink <chrisrink10@gmail.com>
 *
 * @copyright 2015 Chris Rink. MIT Licensed.
 */

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include "libds/buffer.h"
#include "libds/hash.h"

struct DSBuffer {
    char* str;
    size_t len;
    size_t cap;
};

static bool dsbuf_resize(DSBuffer *str, size_t size);
static int utf8_validate_char(const char *s, const char *e);

/*
 * BUFFER PUBLIC FUNCTIONS
 */

DSBuffer *dsbuf_new(const char *value) {
    size_t len = strlen(value);
    return dsbuf_new_l(value, len);
}

DSBuffer *dsbuf_new_l(const char *value, size_t len) {
    if (len < 1) {
        return NULL;
    }

    DSBuffer *s = malloc(sizeof(DSBuffer));
    if (!s) {
        return NULL;
    }

    s->len = len;
    s->cap = len * DSBUFFER_CAPACITY_FACTOR;
    s->str = malloc(s->cap);
    if (!s->str) {
        goto cleanup_dsbuf;
    }

    memcpy(s->str, value, len);
    memset(&s->str[len], '\0', (s->cap - len));
    return s;

cleanup_dsbuf:
    free(s);
    return NULL;
}

DSBuffer *dsbuf_new_buffer(size_t cap) {
    cap = (cap < DSBUFFER_MINIMUM_CAPACITY) ? DSBUFFER_MINIMUM_CAPACITY : cap;

    DSBuffer *s = malloc(sizeof(DSBuffer));
    if (!s) {
        return NULL;
    }

    s->len = 0;
    s->cap = cap;
    s->str = calloc(s->cap, 1);
    if (!s->str) {
        goto cleanup_dsbuf_buffer;
    }
    return s;

cleanup_dsbuf_buffer:
    free(s);
    return NULL;
}

void dsbuf_destroy(DSBuffer *str) {
    if (!str) { return; }
    free(str->str);
    str->str = NULL;
    free(str);
}

DSBuffer *dsbuf_dup(const DSBuffer *str) {
    if (!str) { return NULL; }

    DSBuffer *s = malloc(sizeof(DSBuffer));
    if (!s) {
        return NULL;
    }

    s->len = str->len;
    s->cap = str->cap;
    s->str = calloc(s->cap, 1);
    if (!s->str) {
        goto cleanup_dsbuf_dup;
    }

    memcpy(s->str, &str->str[0], s->len);
    return s;

cleanup_dsbuf_dup:
    free(s);
    return NULL;
}

size_t dsbuf_len(const DSBuffer *str) {
    assert(str);
    return str->len;
}

size_t dsbuf_cap(const DSBuffer *str) {
    assert(str);
    return str->cap;
}

bool dsbuf_append(DSBuffer *str, DSBuffer *newc) {
    if ((!str) || (!newc)) {
        return false;
    }

    size_t size = str->len + newc->len;
    size_t threshold = size * DSBUFFER_CAPACITY_FACTOR;
    if (str->cap < threshold) {
        if (!dsbuf_resize(str, threshold)) {
            return false;
        }
    }

    memcpy(&str->str[str->len], newc->str, newc->len);
    str->len += newc->len;
    return true;
}

bool dsbuf_append_char(DSBuffer *str, int newc) {
    if (!str) {
        return false;
    }

    size_t size = str->len + 1;
    if (str->cap < size) {
        if (!dsbuf_resize(str, str->cap * DSBUFFER_CAPACITY_FACTOR)) {
            return false;
        }
    }

    str->str[str->len] = (char)newc;
    str->len++;
    return true;
}

bool dsbuf_append_str(DSBuffer *str, const char *newstr) {
    if ((!str) || (!newstr)) {
        return false;
    }

    size_t addlen = strlen(newstr);
    size_t size = str->len + addlen;
    if (str->cap < size) {
        if (!dsbuf_resize(str, str->cap * DSBUFFER_CAPACITY_FACTOR)) {
            return false;
        }
    }

    memcpy(&str->str[str->len], newstr, addlen);
    str->len = size;
    return true;
}

int dsbuf_char_at(const DSBuffer *str, size_t pos) {
    if ((!str) || (pos >= str->len)) {
        return DSBUFFER_CHAR_NOT_FOUND;
    }

    return str->str[pos];
}

DSBuffer *dsbuf_substr(const DSBuffer *str, size_t start, size_t len) {
    if ((!str) || (start > str->len) || (len > (str->len - start))) {
        return NULL;
    }

    DSBuffer * sub = dsbuf_new_buffer(len * DSBUFFER_CAPACITY_FACTOR);
    if (!sub) {
        return NULL;
    }

    strncpy(sub->str, &str->str[start], len);
    sub->len = len;
    return sub;
}

bool dsbuf_equals(const DSBuffer *str, const DSBuffer *other) {
    if ((!str) || (!other)) {
        return false;
    }

    if (str->len != other->len) {
        return false;
    }

    if (str->cap != other->cap) {
        return false;
    }

    int res = strcmp(str->str, other->str);
    return (res == 0);
}

bool dsbuf_equals_char(const DSBuffer *str, const char *other) {
    if (!str) { return false; }
    int res = strcmp(str->str, other);
    return (res == 0);
}

const char *dsbuf_char_ptr(const DSBuffer *str) {
    if (!str) {
        return NULL;
    }

    return str->str;
}

char *dsbuf_to_char_array(const DSBuffer *str) {
    if (!str) {
        return NULL;
    }

    size_t m = (str->len) + 1;
    char* cpy = malloc(m);
    if (!cpy) {
        return NULL;
    }

    memcpy(cpy, str->str, str->len);
    str->str[str->len] = '\0';
    return cpy;
}

unsigned int dsbuf_hash(const DSBuffer *str) {
    if (!str) { return 0; }
    return (unsigned int) hash_fnv1(str->str);
}

int dsbuf_compare(const DSBuffer *left, const DSBuffer *right) {
    if (!left) { return INT_MIN; }
    if (!right) { return INT_MAX; }
    if (left->len < right->len) { return -1; }
    if (left->len > right->len) { return 1; }
    return memcmp(left->str, right->str, left->len);
}

bool dsbuf_utf8_validate(const DSBuffer *buf, size_t *l) {
    if (!buf) { return false; }

    bool count = (l != NULL) ? true : false;
    const char *s = buf->str;
    const char *e = buf->str + buf->len;
    int len = 0;

    for (; s < e; s += len) {
        len = utf8_validate_char(s, e);
        if (len == 0) { return false; }
        if (count) { (*l)++; }
    }
    assert(s == e);

    return true;
}

size_t dsbuf_utf8_len(const DSBuffer *buf) {
    if (!buf) { return 0; }

    const char *s = buf->str;
    const char *e = buf->str + buf->len;
    int len = 0;
    size_t count = 0;

    for (; s < e; s += len) {
        len = utf8_validate_char(s, e);
        if (len == 0) { break; }
        count++;
    }

    return count;
}

/*
 * PRIVATE FUNCTIONS
 */

// Resizes a DSBuffer upwards
static bool dsbuf_resize(DSBuffer *str, size_t size) {
    assert(str);

    if ((size < 1) || (str->cap >= size)) {
        return false;
    }

    char* cache = str->str; // Cache a pointer to the old string
    str->str = malloc(size);
    if (!str->str) {
        str->str = cache;   // Point the old pointer back to the cached value
        return false;
    }

    str->cap = size;
    memcpy(str->str, cache, str->len);
    free(cache);
    return true;
}

/*
 * This function was taken from the charset module on CCAN
 * (http://ccodearchive.net).
 *
 * Copyright (C) 2011 Joseph A. Adams (joeyadams3.14159@gmail.com)
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 * This function implements the syntax given in RFC3629, which is
 * the same as that given in The Unicode Standard, Version 6.0.
 *
 * It has the following properties:
 *
 *  * All codepoints U+0000..U+10FFFF may be encoded,
 *    except for U+D800..U+DFFF, which are reserved
 *    for UTF-16 surrogate pair encoding.
 *  * UTF-8 byte sequences longer than 4 bytes are not permitted,
 *    as they exceed the range of Unicode.
 *  * The sixty-six Unicode "non-characters" are permitted
 *    (namely, U+FDD0..U+FDEF, U+xxFFFE, and U+xxFFFF).
 */
static int utf8_validate_char(const char *s, const char *e) {
    unsigned char c = (unsigned char)*s++;

    if (c <= 0x7F) {        /* 00..7F */
        return 1;
    } else if (c <= 0xC1) { /* 80..C1 */
        /* Disallow overlong 2-byte sequence. */
        return 0;
    } else if (c <= 0xDF) { /* C2..DF */
        /* Make sure the character isn't clipped. */
        if (e - s < 1)
            return 0;

        /* Make sure subsequent byte is in the range 0x80..0xBF. */
        if (((unsigned char)*s++ & 0xC0) != 0x80)
            return 0;

        return 2;
    } else if (c <= 0xEF) { /* E0..EF */
        /* Make sure the character isn't clipped. */
        if (e - s < 2)
            return 0;

        /* Disallow overlong 3-byte sequence. */
        if (c == 0xE0 && (unsigned char)*s < 0xA0)
            return 0;

        /* Disallow U+D800..U+DFFF. */
        if (c == 0xED && (unsigned char)*s > 0x9F)
            return 0;

        /* Make sure subsequent bytes are in the range 0x80..0xBF. */
        if (((unsigned char)*s++ & 0xC0) != 0x80)
            return 0;
        if (((unsigned char)*s++ & 0xC0) != 0x80)
            return 0;

        return 3;
    } else if (c <= 0xF4) { /* F0..F4 */
        /* Make sure the character isn't clipped. */
        if (e - s < 3)
            return 0;

        /* Disallow overlong 4-byte sequence. */
        if (c == 0xF0 && (unsigned char)*s < 0x90)
            return 0;

        /* Disallow codepoints beyond U+10FFFF. */
        if (c == 0xF4 && (unsigned char)*s > 0x8F)
            return 0;

        /* Make sure subsequent bytes are in the range 0x80..0xBF. */
        if (((unsigned char)*s++ & 0xC0) != 0x80)
            return 0;
        if (((unsigned char)*s++ & 0xC0) != 0x80)
            return 0;
        if (((unsigned char)*s++ & 0xC0) != 0x80)
            return 0;

        return 4;
    } else {                /* F5..FF */
        return 0;
    }
}
