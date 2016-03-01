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

#include "parser.h"
#include "../src/parser.h"
#include "../src/vm.h"

typedef struct ParseResultTuple {
    const char *val;        /** Value of the input token */
    ms_VMByteCode bc;       /** Type of token value should be lexed into */
} ParseResultTuple;

/*
 * FORWARD DECLARATIONS
 */

MunitResult CompareByteCode(ms_VMByteCode *bc1, ms_VMByteCode *bc2);
MunitResult TestParseResultTuple(ParseResultTuple *tuples, size_t len);

/*
 * UNIT TEST FUNCTIONS
 */

#define VM_OPC(opc, arg) ms_VMOpCodeWithArg(opc, arg)
#define VM_FLOAT_LITERAL(v) ((ms_VMValue){ .type = VMVAL_FLOAT, .val = (ms_VMPrimitive){ .f = v } })
#define VM_INT_LITERAL(v) ((ms_VMValue){ .type = VMVAL_INT, .val = (ms_VMPrimitive){ .i = v } })
#define VM_BOOL_LITERAL(v) ((ms_VMValue){ .type = VMVAL_BOOL, .val = (ms_VMPrimitive){ .b = v } })
#define VM_NULL_LITERAL() ((ms_VMValue){ .type = VMVAL_NULL, .val = (ms_VMPrimitive){ .n = MS_VM_NULL_POINTER } })

MunitResult prs_TestParseLiterals(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "0",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(0),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "3",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(3),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "0.0",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_FLOAT_LITERAL(0.0),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "3.14",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_FLOAT_LITERAL(3.14),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "true",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_BOOL_LITERAL(true),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "false",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_BOOL_LITERAL(false),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "null",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_NULL_LITERAL(),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseUnaryExprs(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "-3",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(3)
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_NEGATE, 0),
                },
                .nops = 2, .nvals = 1,
            }
        },
        {
            .val = "--2.72",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_FLOAT_LITERAL(2.72),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_NEGATE, 0),
                    VM_OPC(OPC_NEGATE, 0),
                },
                .nops = 3, .nvals = 1,
            }
        },
        {
            .val = "!true",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_BOOL_LITERAL(true),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_NOT, 0),
                },
                .nops = 2, .nvals = 1,
            }
        },
        {
            .val = "!!false",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_BOOL_LITERAL(false),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_NOT, 0),
                    VM_OPC(OPC_NOT, 0),
                },
                .nops = 3, .nvals = 1,
            }
        },
        {
            .val = "~792",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(792),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_BITWISE_NOT, 0),
                },
                .nops = 2, .nvals = 1,
            }
        },
        {
            .val = "~~42",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(42),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_BITWISE_NOT, 0),
                    VM_OPC(OPC_BITWISE_NOT, 0),
                },
                .nops = 3, .nvals = 1,
            }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseBinaryExprs(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "7 + 3.6",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(7),
                    VM_FLOAT_LITERAL(3.6),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_ADD, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "16 - false",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(16),
                    VM_BOOL_LITERAL(false),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_SUBTRACT, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "2 * 3.14",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(2),
                    VM_FLOAT_LITERAL(3.14),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_MULTIPLY, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "17 / 8.25",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(17),
                    VM_FLOAT_LITERAL(8.25),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_DIVIDE, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "8.888 \\ 6",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_FLOAT_LITERAL(8.888),
                    VM_INT_LITERAL(6),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_IDIVIDE, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "42 % 8",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(42),
                    VM_INT_LITERAL(8),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_MODULO, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "5.25 ** 2",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_FLOAT_LITERAL(5.25),
                    VM_INT_LITERAL(2),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_EXPONENTIATE, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "5 << 2",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(5),
                    VM_INT_LITERAL(2),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_SHIFT_LEFT, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "183822 >> 4",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(183822),
                    VM_INT_LITERAL(4),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_SHIFT_RIGHT, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "13 & 97",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(13),
                    VM_INT_LITERAL(97),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_BITWISE_AND, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "3 | 15",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(3),
                    VM_INT_LITERAL(15),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_BITWISE_OR, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "53 @ 7",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(53),
                    VM_INT_LITERAL(7),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_BITWISE_XOR, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "46.12 <= 73",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_FLOAT_LITERAL(46.12),
                    VM_INT_LITERAL(73),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_LE, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "true < 14",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_BOOL_LITERAL(true),
                    VM_INT_LITERAL(14),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_LT, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "33 != 1988",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(33),
                    VM_INT_LITERAL(1988),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_NOT_EQ, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "71 == 0.33",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(71),
                    VM_FLOAT_LITERAL(0.33),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_EQ, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "81.3 > 90",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_FLOAT_LITERAL(81.3),
                    VM_INT_LITERAL(90),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_GT, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "1000 >= 10000",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(1000),
                    VM_INT_LITERAL(10000),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_GE, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },

        {
            .val = "true && false",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_BOOL_LITERAL(true),
                    VM_BOOL_LITERAL(false),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_AND, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
        {
            .val = "1 || null",
            .bc = {
                .values = (ms_VMValue[]){
                    VM_INT_LITERAL(1),
                    VM_NULL_LITERAL(),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_OR, 0),
                },
                .nops = 3, .nvals = 2,
            }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

/*
 * PRIVATE FUNCTIONS
 */

// Compare byte-code to make sure that two byte-code objects are equal.
MunitResult CompareByteCode(ms_VMByteCode *bc1, ms_VMByteCode *bc2) {
    munit_assert_cmp_int(bc1->nops, ==, bc2->nops);
    munit_assert_cmp_int(bc1->nvals, ==, bc2->nvals);

    for (size_t i = 0; i < bc1->nops; i++) {
        munit_assert_cmp_int(bc1->code[i], ==, bc2->code[i]);
    }

    for (size_t i = 0; i < bc1->nvals; i++) {
        munit_assert_cmp_int(bc1->values[i].type, ==, bc2->values[i].type);
        switch (bc1->values[i].type) {
            case VMVAL_FLOAT:
                munit_assert_cmp_double(bc1->values[i].val.f, ==, bc2->values[i].val.f);
                break;
            case VMVAL_INT:
                munit_assert_cmp_int(bc1->values[i].val.i, ==, bc2->values[i].val.i);
                break;
            case VMVAL_STR:
                munit_assert_true(dsbuf_equals(bc1->values[i].val.s, bc2->values[i].val.s));
                break;
            case VMVAL_BOOL:
                munit_assert(bc1->values[i].val.b == bc2->values[i].val.b);
                break;
            case VMVAL_NULL:
                munit_assert(bc1->values[i].val.n == bc2->values[i].val.n);
                break;
        }
    }

    return MUNIT_OK;
}

// Test a tuple of expected parsing results
MunitResult TestParseResultTuple(ParseResultTuple *tuples, size_t len) {
    ms_Parser *prs = ms_ParserNew();
    munit_assert_non_null(prs);

    for (size_t i = 0; i < len; i++) {
        ParseResultTuple *tuple = &tuples[i];
        ms_ParserInitString(prs, tuple->val);

        ms_VMByteCode *code;    /* freed by the VM */
        const ms_ParseError *err;
        ms_ParseResult pres = ms_ParserParse(prs, &code, &err);

        munit_assert_cmp_int(pres, !=, PARSE_ERROR);
        munit_assert_non_null(code);
        munit_assert_null(err);

        CompareByteCode(code, &tuple->bc);
    }

    ms_ParserDestroy(prs);
    return MUNIT_OK;
}
