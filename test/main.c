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

#include "munit/munit.h"
#include "streamreader.h"
#include "lexer.h"
#include "parser.h"

static MunitSuite suites[] = {
        {
            "/lib/streamreader",
            streamreader_tests,
            NULL,
            1,
            MUNIT_SUITE_OPTION_NONE
        },
        {
            "/lexer",
            lexer_tests,
            NULL,
            1,
            MUNIT_SUITE_OPTION_NONE
        },
        {
            "/parser",
            parser_tests,
            NULL,
            1,
            MUNIT_SUITE_OPTION_NONE
        },
        { NULL, NULL, NULL, 0, MUNIT_SUITE_OPTION_NONE },
};

static const MunitSuite mscript = {
        "/mscript",
        NULL,
        suites,
        1,
        MUNIT_SUITE_OPTION_NONE
};

int main(int argc, char *argv[]) {
    return munit_suite_main(&mscript, NULL, argc, argv);
}
