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

#ifndef MSCRIPT_TEST_STREAMREADER_H
#define MSCRIPT_TEST_STREAMREADER_H

#include "munit/munit.h"
#include "stream/streamreader.h"

void *sr_CreateTempFile(const MunitParameter params[], void *user_data);
void sr_CleanUpTempFile(void *file);
MunitResult sr_TestFileNextChar(const MunitParameter params[], void *file);
MunitResult sr_TestFileUnread(const MunitParameter params[], void *file);
MunitResult sr_TestStringNextChar(const MunitParameter params[], void *na);
MunitResult sr_TestStringUnread(const MunitParameter params[], void *na);

static MunitTest streamreader_tests[] = {
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

#endif //MSCRIPT_TEST_STREAMREADER_H
