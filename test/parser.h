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

#ifndef MSCRIPT_TEST_PARSER_H
#define MSCRIPT_TEST_PARSER_H

#include "munit/munit.h"

/*
 * FUNCTION DECLARATIONS
 */

MunitResult prs_TestParseErrors(const MunitParameter params[], void *user_data);
MunitResult prs_TestParseLiterals(const MunitParameter params[], void *user_data);
MunitResult prs_TestParseUnaryExprs(const MunitParameter params[], void *user_data);
MunitResult prs_TestParseBinaryExprs(const MunitParameter params[], void *user_data);
MunitResult prs_TestParseExprPrecedence(const MunitParameter params[], void *user_data);
MunitResult prs_TestParseFunctionCalls(const MunitParameter params[], void *user_data);
MunitResult prs_TestParseDeleteStatement(const MunitParameter params[], void *user_data);
MunitResult prs_TestParseImportStatement(const MunitParameter params[], void *user_data);

/*
 * TEST DEFINITIONS
 */

extern MunitTest parser_tests[];

#endif //MSCRIPT_TEST_PARSER_H
