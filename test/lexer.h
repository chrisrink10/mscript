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

#ifndef MSCRIPT_TEST_LEXER_H
#define MSCRIPT_TEST_LEXER_H

#include "munit/munit.h"

/*
 * FUNCTION DECLARATIONS
 */

MunitResult lex_TestLexIntNumerics(const MunitParameter params[], void *user_data);
MunitResult lex_TestLexFloatNumerics(const MunitParameter params[], void *user_data);
MunitResult lex_TestLexHexNumerics(const MunitParameter params[], void *user_data);
MunitResult lex_TestLexBadNumerics(const MunitParameter params[], void *user_data);
MunitResult lex_TestLexKeywords(const MunitParameter params[], void *user_data);
MunitResult lex_TestLexNonKeywords(const MunitParameter *params, void *user_data);
MunitResult lex_TestLexReservedKeywords(const MunitParameter *params, void *user_data);
MunitResult lex_TestLexGlobals(const MunitParameter *params, void *user_data);
MunitResult lex_TestLexBuiltins(const MunitParameter *params, void *user_data);
MunitResult lex_TestLexInvalidIdentifiers(const MunitParameter *params, void *user_data);
MunitResult lex_TestLexOperators(const MunitParameter *params, void *user_data);
MunitResult lex_TestLexPunctuation(const MunitParameter *params, void *user_data);
MunitResult lex_TestLexNewlines(const MunitParameter *params, void *user_data);
MunitResult lex_TestLexStrings(const MunitParameter params[], void *user_data);
MunitResult lex_TestLexInvalidStrings(const MunitParameter params[], void *user_data);

/*
 * TEST DEFINITIONS
 */

extern MunitTest lexer_tests[];

#endif //MSCRIPT_TEST_LEXER_H
