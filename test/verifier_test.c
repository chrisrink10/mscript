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

#include "../src/error.h"
#include "../src/parser.h"
#include "../src/verifier.h"
#include "verifier_test.h"

typedef struct {
    const char *val;            /** input code chunk */
    ms_Result expected;         /** expected error result */
} VerifierResultTuple;

/*
 * TEST DEFINITIONS
 */

static MunitResult ver_TestProhibitRedeclaration(const MunitParameter params[], void *user_data);
static MunitResult ver_TestProhibitUndefinedReference(const MunitParameter params[], void *user_data);
static MunitResult ver_TestRequireBreakAndContinueInLoop(const MunitParameter params[], void *user_data);
static MunitResult ver_TestRequireFunctionForReturnStmt(const MunitParameter params[], void *user_data);

MunitTest verifier_tests[] = {
    {
        "/ProhibitRedeclaration",
        ver_TestProhibitRedeclaration,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/ProhibitUndefinedReference",
        ver_TestProhibitUndefinedReference,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/RequireBreakAndContinueInLoop",
        ver_TestRequireBreakAndContinueInLoop,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/RequireFunctionForReturnStmt",
        ver_TestRequireFunctionForReturnStmt,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    { NULL, NULL, NULL, NULL, MUNIT_TEST_OPTION_NONE, NULL }
};

/*
 * FORWARD DECLARATIONS
 */

static MunitResult TestVerifierResultTuple(VerifierResultTuple *tuples, size_t len);

/*
 * TEST CASE FUNCTIONS
 */

static MunitResult ver_TestProhibitRedeclaration(const MunitParameter params[], void *user_data) {
    VerifierResultTuple tuples[] = {
        {
            .val = "func main() {\n"
                   "    var index := 0;\n"
                   "    index += 1;\n"
                   "    var index := 1;\n"
                   "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func main() {\n"
                "    var index, index := 1;\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func main() {\n"
                "    var index, index := 1;\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func inc(index) {\n"
                "    var index := 1;\n"
                "    return index + 1;\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "var index := 0;\n"
                "func inc(index) {\n"
                "    return index + 1;\n"
                "}",
            .expected = MS_RESULT_SUCCESS
        },
        {
            .val = "func inc(index) {\n"
                "    var incrementer := func() {\n"
                "        var index := index + 1;\n"
                "        return index;\n"
                "    };\n"
                "    return incrementer();\n"
                "}",
            .expected = MS_RESULT_SUCCESS
        },
        {
            .val = "func inc() {\n"
                "    var index := 1;\n"
                "    var incrementer := func() {\n"
                "        var index := index + 1;\n"
                "        return index;\n"
                "    };\n"
                "    return incrementer();\n"
                "}",
            .expected = MS_RESULT_SUCCESS
        },
        {
            .val = "var index := 1;\n\n"
                "func inc() {\n"
                "    var incrementer := func() {\n"
                "        var index := index + 1;\n"
                "        return index;\n"
                "    };\n"
                "    return incrementer();\n"
                "}",
            .expected = MS_RESULT_SUCCESS
        },
    };

    size_t len = sizeof(tuples) / sizeof(tuples[0]);
    TestVerifierResultTuple(tuples, len);
    return MUNIT_OK;
}

static MunitResult ver_TestProhibitUndefinedReference(const MunitParameter params[], void *user_data) {
    VerifierResultTuple tuples[] = {
        {
            .val = "func main() {\n"
                "    index += 1;\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func calculate_total() {\n"
                "    var tax_rate := 0.055;\n"
                "    return amount + (amount * tax_rate);\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func calculate_total(amount) {\n"
                "    var tax_rate := 0.055;\n"
                "    return amount + (amount * tax_rate);\n"
                "}",
            .expected = MS_RESULT_SUCCESS
        },
        {
            .val = "func calculate_total(amount) {\n"
                "    var tax_rate := 0.055;\n"
                "    return amount + (amount * tax_rate);\n"
                "}",
            .expected = MS_RESULT_SUCCESS
        },
        {
            .val = "func calculate_total(amount) {\n"
                "    var tax_rate := 0.055;\n"
                "    var calc := func() {\n"
                "        return amount + (amount * tax_rate);"
                "    };\n"
                "    return calc();\n"
                "}",
            .expected = MS_RESULT_SUCCESS
        },
        {
            .val = "func calculate_total(amount) {\n"
                "    var calc := func() {\n"
                "        return amount + (amount * tax_rate);"
                "    };\n"
                "    return calc();\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func calculate_total() {\n"
                "    var tax_rate := 0.055;\n"
                "    var calc := func() {\n"
                "        return amount + (amount * tax_rate);"
                "    };\n"
                "    return calc();\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "var amount := 10;\n\n"
                "func calculate_total() {\n"
                "    var tax_rate := 0.055;\n"
                "    var calc := func() {\n"
                "        return amount + (amount * tax_rate);"
                "    };\n"
                "    return calc();\n"
                "}",
            .expected = MS_RESULT_SUCCESS
        },
    };

    size_t len = sizeof(tuples) / sizeof(tuples[0]);
    TestVerifierResultTuple(tuples, len);
    return MUNIT_OK;
}

static MunitResult ver_TestRequireBreakAndContinueInLoop(const MunitParameter params[], void *user_data) {
    VerifierResultTuple tuples[] = {
        {
            .val = "func main() {\n"
                "    break;\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func main() {\n"
                "    continue;\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func main(x) {\n"
                "    if x {\n"
                "        break;\n"
                "    }\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func main(x) {\n"
                "    if x {\n"
                "        continue;\n"
                "    }\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func main(x) {\n"
                "    if x {\n"
                "        return x;\n"
                "    } else {\n"
                "        break;"
                "    }\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func main(x) {\n"
                "    if x {\n"
                "        return x;\n"
                "    } else {\n"
                "        continue;"
                "    }\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
    };

    size_t len = sizeof(tuples) / sizeof(tuples[0]);
    TestVerifierResultTuple(tuples, len);
    return MUNIT_OK;
}

static MunitResult ver_TestRequireFunctionForReturnStmt(const MunitParameter params[], void *user_data) {
    VerifierResultTuple tuples[] = {
        {
            .val = "var x := 10;\n\n"
                "if x {\n"
                "    return 30;\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func main() {\n"
                "    var x := 10;\n\n"
                "    if x {\n"
                "        return 30;\n"
                "    }\n"
                "}",
            .expected = MS_RESULT_SUCCESS
        },
        {
            .val = "var x := 10;\n\n"
                "if x {\n"
                "    x += 5;\n"
                "} else {\n"
                "    return x;\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func main() {\n"
                "    var x := 10;\n\n"
                "    if x {\n"
                "        x += 5;\n"
                "    } else {\n"
                "        return x;\n"
                "    }\n"
                "}",
            .expected = MS_RESULT_SUCCESS
        },
        {
            .val = "var y;"
                "for y := 1 : 10 {\n"
                "    return y;\n"
                "}",
            .expected = MS_RESULT_ERROR
        },
        {
            .val = "func main() {\n"
                "    var y;"
                "    for y := 1 : 10 {\n"
                "        return y;\n"
                "    }\n"
                "}",
            .expected = MS_RESULT_SUCCESS
        },
    };

    size_t len = sizeof(tuples) / sizeof(tuples[0]);
    TestVerifierResultTuple(tuples, len);
    return MUNIT_OK;
}

/*
 * COMPARISON FUNCTIONS
 */

static MunitResult TestVerifierResultTuple(VerifierResultTuple *tuples, size_t len) {
    ms_Parser *prs = ms_ParserNew();
    munit_assert_non_null(prs);

    for (size_t i = 0; i < len; i++) {
        VerifierResultTuple *tuple = &tuples[i];
        ms_ParserInitString(prs, tuple->val);
        munit_logf(MUNIT_LOG_INFO, "  code='%s'", tuple->val);

        const ms_AST *ast;
        ms_Error *err;
        ms_Result pres = ms_ParserParse(prs, &ast, &err);
        if (err) {
            munit_logf(MUNIT_LOG_INFO, "err = %s", err->msg);
            ms_ErrorDestroy(err);
        }

        munit_assert_cmp_int(pres, !=, MS_RESULT_ERROR);
        munit_assert_non_null(ast);
        munit_assert_null(err);

        ms_Result vres = ms_ParserVerifyAST(ast, &err);
        if (err) {
            munit_logf(MUNIT_LOG_INFO, "err = %s", err->msg);
            ms_ErrorDestroy(err);
        }

        munit_assert_cmp_int(vres, ==, tuple->expected);
    }

    ms_ParserDestroy(prs);
    return MUNIT_OK;
}
