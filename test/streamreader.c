/*------------------------------------------------------------------------------
 *    Copyright 2016 Chris Rink
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *----------------------------------------------------------------------------*/

#include <string.h>
#include "streamreader.h"

static const char * const TestString = "This value is going to be the test\n"
                                       "string for the functions of the\n"
                                       "bundled StreamReader library.\n";

void *sr_CreateTempFile(const MunitParameter params[], void *user_data) {
    char *tmpname = munit_malloc(19);
    memcpy(tmpname, "streamreaderXXXXXX", 18);
    mkstemp(tmpname);

    FILE *f = fopen(tmpname, "r+");
    munit_assert_non_null(f);
    munit_assert_cmp_int(fprintf(f, TestString), >, 0);
    munit_assert_cmp_int(fclose(f), ==, 0);
    return tmpname;
}

void sr_CleanUpTempFile(void *file) {
    munit_assert_cmp_int(remove((char *)file), ==, 0);
}

MunitResult sr_TestFileNextChar(const MunitParameter params[], void *file) {
    sr_Reader *stream = sr_NewFile((char *)file);
    munit_assert_non_null(stream);

    const char *cur = &TestString[0];
    while (cur[0] != '\0') {
        int next = sr_NextChar(stream);
        munit_assert_cmp_int(next, ==, cur[0]);
        cur++;
    }

    sr_Destroy(stream);
    return MUNIT_OK;
}

MunitResult sr_TestFileUnread(const MunitParameter params[], void *file) {
    sr_Reader *stream = sr_NewFile((char *)file);
    munit_assert_non_null(stream);

    while (sr_NextChar(stream) != EOF) {
        /* do nothing */
    }

    size_t pos = strlen(TestString) - 1;
    while (1) {
        if (pos == 0) { break; }
        int next = sr_Unread(stream);
        munit_assert_cmp_int(next, ==, TestString[pos]);
        pos--;
    }
    sr_Destroy(stream);
    return MUNIT_OK;
}

MunitResult sr_TestStringNextChar(const MunitParameter params[], void *na) {
    sr_Reader *stream = sr_NewString(TestString);
    munit_assert_non_null(stream);

    const char *cur = &TestString[0];
    while (cur[0] != '\0') {
        int next = sr_NextChar(stream);
        munit_assert_cmp_int(next, ==, cur[0]);
        cur++;
    }

    sr_Destroy(stream);
    return MUNIT_OK;
}

MunitResult sr_TestStringUnread(const MunitParameter params[], void *na) {
    sr_Reader *stream = sr_NewString(TestString);
    munit_assert_non_null(stream);

    while (sr_NextChar(stream) != EOF) {
        /* do nothing */
    }

    size_t pos = strlen(TestString) - 1;
    while (1) {
        if (pos == 0) { break; }
        int next = sr_Unread(stream);
        munit_assert_cmp_int(next, ==, TestString[pos]);
        pos--;
    }
    sr_Destroy(stream);
    return MUNIT_OK;
}
