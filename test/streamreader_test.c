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
#include "streamreader_test.h"

/*
 * TEST DEFINITIONS
 */

static MunitResult sr_TestFileNextChar(const MunitParameter params[], void *file);
static MunitResult sr_TestFileUnread(const MunitParameter params[], void *file);
static MunitResult sr_TestStringNextChar(const MunitParameter params[], void *na);
static MunitResult sr_TestStringUnread(const MunitParameter params[], void *na);
static void *sr_CreateTempFile(const MunitParameter params[], void *user_data);
static void sr_CleanUpTempFile(void *file);

MunitTest streamreader_tests[] = {
    {
        "/FILE-NextChar",
        sr_TestFileNextChar,
        sr_CreateTempFile,
        sr_CleanUpTempFile,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/FILE-Unread",
        sr_TestFileUnread,
        sr_CreateTempFile,
        sr_CleanUpTempFile,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/char*-NextChar",
        sr_TestStringNextChar,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/char*-Unread",
        sr_TestStringUnread,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    { NULL, NULL, NULL, NULL, MUNIT_TEST_OPTION_NONE, NULL }
};

static const char * const TestString = "This value is going to be the test\n"
                                       "string for the functions of the\n"
                                       "bundled StreamReader library.\n";

/*
 * SETUP AND TEARDOWN FUNCTIONS
 */

static void *sr_CreateTempFile(const MunitParameter params[], void *user_data) {
    char *tmpname = munit_malloc(19);
    memcpy(tmpname, "streamreaderXXXXXX", 18);
    mkstemp(tmpname);

    FILE *f = fopen(tmpname, "r+");
    munit_assert_not_null(f);
    munit_assert_int(fprintf(f, TestString), >, 0);
    munit_assert_int(fclose(f), ==, 0);
    return tmpname;
}

static void sr_CleanUpTempFile(void *file) {
    munit_assert_int(remove((char *)file), ==, 0);
    free(file);
}

/*
 * UNIT TEST FUNCTIONS
 */

static MunitResult sr_TestFileNextChar(const MunitParameter params[], void *file) {
    ms_StreamReader *stream = ms_StreamNewFile((char *) file);
    munit_assert_not_null(stream);

    const char *cur = &TestString[0];
    while (cur[0] != '\0') {
        int next = ms_StreamNextChar(stream);
        munit_assert_int(next, ==, cur[0]);
        cur++;
    }

    ms_StreamDestroy(stream);
    return MUNIT_OK;
}

static MunitResult sr_TestFileUnread(const MunitParameter params[], void *file) {
    ms_StreamReader *stream = ms_StreamNewFile((char *) file);
    munit_assert_not_null(stream);

    while (ms_StreamNextChar(stream) != EOF) {
        /* do nothing */
    }

    size_t pos = strlen(TestString) - 1;
    while (1) {
        if (pos == 0) { break; }
        int next = ms_StreamUnread(stream);
        munit_assert_int(next, ==, TestString[pos]);
        pos--;
    }
    ms_StreamDestroy(stream);
    return MUNIT_OK;
}

static MunitResult sr_TestStringNextChar(const MunitParameter params[], void *na) {
    ms_StreamReader *stream = ms_StreamNewString(TestString);
    munit_assert_not_null(stream);

    const char *cur = &TestString[0];
    while (cur[0] != '\0') {
        int next = ms_StreamNextChar(stream);
        munit_assert_int(next, ==, cur[0]);
        cur++;
    }

    ms_StreamDestroy(stream);
    return MUNIT_OK;
}

static MunitResult sr_TestStringUnread(const MunitParameter params[], void *na) {
    ms_StreamReader *stream = ms_StreamNewString(TestString);
    munit_assert_not_null(stream);

    while (ms_StreamNextChar(stream) != EOF) {
        /* do nothing */
    }

    size_t pos = strlen(TestString) - 1;
    while (1) {
        if (pos == 0) { break; }
        int next = ms_StreamUnread(stream);
        munit_assert_int(next, ==, TestString[pos]);
        pos--;
    }
    ms_StreamDestroy(stream);
    return MUNIT_OK;
}
