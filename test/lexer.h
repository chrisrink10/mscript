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
 * TEST DATA AND PARAMETERS
 */

static char* int_num_vals[] = {
        "0", "1", "15", "382932", "791933922",
        NULL
};

static MunitParameterEnum int_num_params[] = {
        { "num", int_num_vals },
        { NULL, NULL }
};

static char* float_num_vals[] = {
        "32223.53", "3.14", "2.7182818284", "1.6180339", "1.414", "2.",
        "2402.", "1332.", "10e4", "1e10", "2.3e8", "8.e4", "7e33",
        NULL
};

static MunitParameterEnum float_num_params[] = {
        { "num", float_num_vals },
        { NULL, NULL }
};

static char* hex_num_vals[] = {
        "0x1", "0X0", "0x32f2", "0xf", "0xbeef", "0XDEAD",
        NULL
};

static MunitParameterEnum hex_num_params[] = {
        { "num", hex_num_vals },
        { NULL, NULL }
};

static char* bad_num_vals[] = {
        "0x",
        NULL
};

static MunitParameterEnum bad_num_params[] = {
        { "num", bad_num_vals },
        { NULL, NULL }
};

static char *non_keyword_vals[] = {
        "IF", "ret", "True", "False", "nil", "function", "delete",
        "i", "k", "next", "CONSTANT",
        NULL
};

static MunitParameterEnum non_keyword_params[] = {
        { "kw", non_keyword_vals },
        { NULL, NULL }
};

static char *reserved_keyword_vals[] = {
        "while", "switch", "goto", "error", "class", "private", "public",
        "protected", "yield", "from", "try", "except", "finally", "do", "and",
        "or", "mut", "const", "async", "await", "repeat", "until",
        NULL
};

static MunitParameterEnum reserved_keyword_params[] = {
        { "kw", reserved_keyword_vals },
        { NULL, NULL }
};

static char* global_vals[] = {
        "^global", "^var", "^_legal_name", "^________", "^g", "^\xF0\x9F\x86\x92",
        NULL
};

static MunitParameterEnum global_params[] = {
        { "global", global_vals },
        { NULL, NULL }
};

static char* builtin_vals[] = {
        "$begin", "$commit", "$rollback", "$order", "$data",
        "$char", "$ord", "$len", "$type", "$open", "$close",
        NULL
};

static MunitParameterEnum builtin_params[] = {
        { "builtin", builtin_vals },
        { NULL, NULL }
};

static char* invalid_identifier_vals[] = {
        "$", "^", "^-some-name", "$%%%%", ";", "?", "`", "#",
        NULL
};

static MunitParameterEnum invalid_identifier_params[] = {
        { "ident", invalid_identifier_vals },
        { NULL, NULL }
};

static char* newline_vals[] = {
        "\r\n", "\n",
        NULL
};

static MunitParameterEnum newline_params[] = {
        { "newline", newline_vals },
        { NULL, NULL }
};

static char* string_vals[] = {
        "\"\"", "''", "\"string\"", "\"string with 'single-quoted' sub\"",
        "\"string with \\\"escaped sub\\\" -- very meta\"",
        "\"string containing keywords: if, else, func\"",
        "\"string containing builtin: $begin, $commit\"",
        "\"string containing global: ^glo, ^people\"",
        NULL
};

static MunitParameterEnum string_params[] = {
        { "str", string_vals },
        { NULL, NULL }
};

static char* invalid_string_vals[] = {
        "\"'", "'\"", "\"\n\"", "\"\r\n\"", "'\n'", "'\r\n'",
        NULL
};

static MunitParameterEnum invalid_string_params[] = {
        { "str", invalid_string_vals },
        { NULL, NULL }
};

/*
 * TEST DEFINITIONS
 */

static MunitTest lexer_tests[] = {
    {
        "/IntNumeric",
        lex_TestLexIntNumerics,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        int_num_params
    },
    {
        "/FloatNumeric",
        lex_TestLexFloatNumerics,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        float_num_params
    },
    {
        "/HexNumeric",
        lex_TestLexHexNumerics,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        hex_num_params
    },
    {
        "/BadNumeric",
        lex_TestLexBadNumerics,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        bad_num_params
    },
    {
        "/Keywords",
        lex_TestLexKeywords,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/NonKeywords",
        lex_TestLexNonKeywords,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        non_keyword_params
    },
    {
        "/ReservedKeywords",
        lex_TestLexReservedKeywords,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        reserved_keyword_params
    },
    {
        "/Globals",
        lex_TestLexGlobals,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        global_params
    },
    {
        "/Builtins",
        lex_TestLexBuiltins,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        builtin_params
    },
    {
        "/InvalidIdentifiers",
        lex_TestLexInvalidIdentifiers,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        invalid_identifier_params
    },
    {
        "/Operators",
        lex_TestLexOperators,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/Punctuation",
        lex_TestLexPunctuation,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/Newlines",
        lex_TestLexNewlines,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        newline_params
    },
    {
        "/Strings",
        lex_TestLexStrings,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        string_params
    },
    {
        "/InvalidStrings",
        lex_TestLexInvalidStrings,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        invalid_string_params
    },
    { NULL, NULL, NULL, NULL, MUNIT_TEST_OPTION_NONE, NULL }
};

#endif //MSCRIPT_TEST_LEXER_H
