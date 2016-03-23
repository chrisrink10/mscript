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
#include "../src/lang.h"
#include "../src/parser.h"
#include "../src/vm.h"
#include "../src/bytecode.h"

typedef struct ParseResultTuple {
    const char *val;        /** Value of the input token */
    ms_AST ast;             /** Abstract syntax tree produced from parsing val */
    ms_VMByteCode bc;       /** Type of token value should be lexed into */
} ParseResultTuple;

/*
 * FORWARD DECLARATIONS
 */

MunitResult CompareAST(const ms_AST *ast1, const ms_AST *ast2);
MunitResult CompareExpressions(const ms_Expr *expr1, const ms_Expr *expr2);
MunitResult CompareExpressionAtoms(ms_ExprAtomType type, const ms_ExprAtom *atom1, const ms_ExprAtom *atom2);
MunitResult CompareByteCode(const ms_VMByteCode *bc1, const ms_VMByteCode *bc2);
MunitResult CompareExpressionList(const ms_ExprList *el1, const ms_ExprList *el2);
MunitResult CompareIdent(const ms_Ident *id1, const ms_Ident *id2);
MunitResult CompareValues(const ms_Value *val1, const ms_Value *val2);
void CleanAST(ms_AST *ast);
void CleanExpression(ms_Expr *expr);
void CleanExpressionAtom(ms_ExprAtomType type, ms_ExprAtom *atom);
void CleanExpressionList(ms_ExprList *list);
void CleanByteCode(ms_VMByteCode *bc);
void CleanParseResultTuple(ParseResultTuple *tuple);
MunitResult TestParseResultTuple(ParseResultTuple *tuples, size_t len);

/*
 * UNIT TEST FUNCTIONS
 */

// "Private" expression composition macros
#define AST(expr) (expr)
#define AST_EXPR(arity, component) ((ms_Expr){ .type = arity, .cmpnt = component })
#define AST_EXPRCOMPONENT(field, memb) ((ms_ExprComponent){ .field = &(memb) })
#define AST_EXPRCOMPONENT_UNARY(atomtype, opname, atomv) AST_EXPRCOMPONENT(u, ((ms_ExprUnary){ .type = atomtype, .op = opname, .atom = atomv }))
#define AST_EXPRCOMPONENT_BINARY(latomv, latomtype, opname, ratomv, ratomtype) AST_EXPRCOMPONENT(b, ((ms_ExprBinary){ .latom = latomv, .ltype = latomtype, .op = opname, .ratom = ratomv, .rtype = ratomtype}))
#define AST_EXPRATOM_EXPR(eatom)  ((ms_ExprAtom){ .expr = &eatom })
#define AST_EXPRATOM_VAL(vatom)   ((ms_ExprAtom){ .val = vatom })
#define AST_EXPRATOM_IDENT(iatom) ((ms_ExprAtom){ .ident = iatom })
#define AST_EXPRATOM_LIST(latom)  ((ms_ExprAtom){ .list = latom })
#define AST_UNARY(atp, uop, v) AST_EXPR(EXPRTYPE_UNARY, AST_EXPRCOMPONENT_UNARY(atp, uop, v))
#define AST_BINARY(latp, lv, op, ratp, rv) AST_EXPR(EXPRTYPE_BINARY, AST_EXPRCOMPONENT_BINARY(lv, latp, op, rv, ratp))

// Expression macros
#define AST_UEXPR_I(uop, v)       AST_UNARY(EXPRATOM_IDENT, uop, AST_EXPRATOM_IDENT(v))
#define AST_UEXPR_V(uop, v)       AST_UNARY(EXPRATOM_VALUE, uop, AST_EXPRATOM_VAL(v))
#define AST_UEXPR_E(uop, v)       AST_UNARY(EXPRATOM_EXPRESSION, uop, AST_EXPRATOM_EXPR(v))
#define AST_BEXPR_VV(lv, bop, rv) AST_BINARY(EXPRATOM_VALUE, AST_EXPRATOM_VAL(lv), bop, EXPRATOM_VALUE, AST_EXPRATOM_VAL(rv))
#define AST_BEXPR_VE(lv, bop, re) AST_BINARY(EXPRATOM_VALUE, AST_EXPRATOM_VAL(lv), bop, EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(re))
#define AST_BEXPR_EV(le, bop, rv) AST_BINARY(EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(le), bop, EXPRATOM_VALUE, AST_EXPRATOM_VAL(rv))
#define AST_BEXPR_EE(le, bop, re) AST_BINARY(EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(le), bop, EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(re))
#define AST_FNCALL_E(e, lst)      AST_BINARY(EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(e), BINARY_CALL, EXPRATOM_EXPRLIST, AST_EXPRATOM_LIST(lst))
#define AST_FNCALL_I(id, lst)     AST_BINARY(EXPRATOM_IDENT, AST_EXPRATOM_IDENT(id), BINARY_CALL, EXPRATOM_EXPRLIST, AST_EXPRATOM_LIST(lst))
#define AST_IDENT(v)         (dsbuf_new_l(v, sizeof(v)-1)) /* subtract one to ignore the terminating NUL */
#define AST_EXPRLIST(l, ...) (dsarray_new_lit((void **)&(((ms_Expr*){ __VA_ARGS__ , })), l, l, NULL, NULL))
#define AST_EMPTY_EXPRLIST() (dsarray_new_cap(1, NULL, NULL))

// VM type and bytecode macros
#define VM_OPC(opc, arg) ms_VMOpCodeWithArg(opc, arg)
#define VM_FLOAT(v) ((ms_Value){ .type = MSVAL_FLOAT, .val = (ms_ValData){ .f = v } })
#define VM_INT(v)   ((ms_Value){ .type = MSVAL_INT,   .val = (ms_ValData){ .i = v } })
#define VM_STR(v)   ((ms_Value){ .type = MSVAL_STR,   .val = (ms_ValData){ .s = dsbuf_new_l(v, sizeof(v)-1) } })
#define VM_BOOL(v)  ((ms_Value){ .type = MSVAL_BOOL,  .val = (ms_ValData){ .b = v } })
#define VM_NULL()   ((ms_Value){ .type = MSVAL_NULL,  .val = (ms_ValData){ .n = MS_VM_NULL_POINTER } })

MunitResult prs_TestParseErrors(const MunitParameter params[], void *user_data) {
    char *exprs[] = {
        "(",
        "(3 ",
        "(3 + ",
        "(3 + 3",
        "3 +",
        "3 + 3)",
        "+ 3",
        "+ 3)",
        "(+ 3",
        "+3",
        "3 ++ 3",
        "3!",
        "3!!",
        "3-",
        "3--",
        "3~",
        "3~~",
        "-3-",
        "~3~",
        "!3!",
        "3 5",
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    ms_Parser *prs = ms_ParserNew();
    munit_assert_non_null(prs);

    for (size_t i = 0; i < len; i++) {
        char *expr = exprs[i];
        ms_ParserInitString(prs, expr);

        ms_VMByteCode *code;
        const ms_ParseError *err;
        ms_ParseResult pres = ms_ParserParse(prs, &code, NULL, &err);

        munit_assert_cmp_int(pres, ==, PARSE_ERROR);
        munit_assert_non_null(err);
        munit_assert_null(code);
    }

    ms_ParserDestroy(prs);

    return MUNIT_OK;
}

MunitResult prs_TestParseLiterals(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "0",
            .ast = AST(AST_UEXPR_V(UNARY_NONE, VM_INT(0))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(0),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "3",
            .ast = AST(AST_UEXPR_V(UNARY_NONE, VM_INT(3))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(3),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "0.0",
            .ast = AST(AST_UEXPR_V(UNARY_NONE, VM_FLOAT(0.0))),
            .bc = {
                .values = (ms_Value[]){
                    VM_FLOAT(0.0),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "3.14",
            .ast = AST(AST_UEXPR_V(UNARY_NONE, VM_FLOAT(3.14))),
            .bc = {
                .values = (ms_Value[]){
                    VM_FLOAT(3.14),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "true",
            .ast = AST(AST_UEXPR_V(UNARY_NONE, VM_BOOL(true))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(true),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "false",
            .ast = AST(AST_UEXPR_V(UNARY_NONE, VM_BOOL(false))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(false),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                },
                .nops = 1, .nvals = 1,
            }
        },
        {
            .val = "null",
            .ast = AST(AST_UEXPR_V(UNARY_NONE, VM_NULL())),
            .bc = {
                .values = (ms_Value[]){
                    VM_NULL(),
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
            .ast = AST(AST_UEXPR_V(UNARY_MINUS, VM_INT(3))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(3)
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
            .ast = AST(AST_UEXPR_E(UNARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_FLOAT(2.72)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_FLOAT(2.72),
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
            .ast = AST(AST_UEXPR_V(UNARY_NOT, VM_BOOL(true))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(true),
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
            .ast = AST(AST_UEXPR_E(UNARY_NOT, AST_UEXPR_V(UNARY_NOT, VM_BOOL(false)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(false),
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
            .ast = AST(AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(792))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(792),
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
            .ast = AST(AST_UEXPR_E(UNARY_BITWISE_NOT, AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(42)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(42),
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
            .ast = AST(AST_BEXPR_VV(VM_INT(7), BINARY_PLUS, VM_FLOAT(3.6))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_FLOAT(3.6),
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
            .val = "3 + ~3",
            .ast = AST(AST_BEXPR_VE(VM_INT(3), BINARY_PLUS, AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(3)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(3),
                    VM_INT(3),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_BITWISE_NOT, 0),
                    VM_OPC(OPC_ADD, 0),
                },
                .nops = 4, .nvals = 2,
            }
        },
        {
            .val = "16 - false",
            .ast = AST(AST_BEXPR_VV(VM_INT(16), BINARY_MINUS, VM_BOOL(false))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(16),
                    VM_BOOL(false),
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
            .val = "3 - -3",
            .ast = AST(AST_BEXPR_VE(VM_INT(3), BINARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_INT(3)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(3),
                    VM_INT(3),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_NEGATE, 0),
                    VM_OPC(OPC_SUBTRACT, 0),
                },
                .nops = 4, .nvals = 2,
            }
        },
        {
            .val = "2 * 3.14",
            .ast = AST(AST_BEXPR_VV(VM_INT(2), BINARY_TIMES, VM_FLOAT(3.14))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(2),
                    VM_FLOAT(3.14),
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
            .ast = AST(AST_BEXPR_VV(VM_INT(17), BINARY_DIVIDE, VM_FLOAT(8.25))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(17),
                    VM_FLOAT(8.25),
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
            .ast = AST(AST_BEXPR_VV(VM_FLOAT(8.888), BINARY_IDIVIDE, VM_INT(6))),
            .bc = {
                .values = (ms_Value[]){
                    VM_FLOAT(8.888),
                    VM_INT(6),
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
            .ast = AST(AST_BEXPR_VV(VM_INT(42), BINARY_MODULO, VM_INT(8))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(42),
                    VM_INT(8),
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
            .ast = AST(AST_BEXPR_VV(VM_FLOAT(5.25), BINARY_EXPONENTIATE, VM_INT(2))),
            .bc = {
                .values = (ms_Value[]){
                    VM_FLOAT(5.25),
                    VM_INT(2),
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
            .ast = AST(AST_BEXPR_VV(VM_INT(5), BINARY_SHIFT_LEFT, VM_INT(2))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(5),
                    VM_INT(2),
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
            .ast = AST(AST_BEXPR_VV(VM_INT(183822), BINARY_SHIFT_RIGHT, VM_INT(4))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(183822),
                    VM_INT(4),
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
            .ast = AST(AST_BEXPR_VV(VM_INT(13), BINARY_BITWISE_AND, VM_INT(97))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(13),
                    VM_INT(97),
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
            .ast = AST(AST_BEXPR_VV(VM_INT(3), BINARY_BITWISE_OR, VM_INT(15))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(3),
                    VM_INT(15),
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
            .ast = AST(AST_BEXPR_VV(VM_INT(53), BINARY_BITWISE_XOR, VM_INT(7))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(53),
                    VM_INT(7),
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
            .ast = AST(AST_BEXPR_VV(VM_FLOAT(46.12), BINARY_LE, VM_INT(73))),
            .bc = {
                .values = (ms_Value[]){
                    VM_FLOAT(46.12),
                    VM_INT(73),
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
            .ast = AST(AST_BEXPR_VV(VM_BOOL(true), BINARY_LT, VM_INT(14))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(true),
                    VM_INT(14),
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
            .ast = AST(AST_BEXPR_VV(VM_INT(33), BINARY_NOT_EQ, VM_INT(1988))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(33),
                    VM_INT(1988),
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
            .ast = AST(AST_BEXPR_VV(VM_INT(71), BINARY_EQ, VM_FLOAT(0.33))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(71),
                    VM_FLOAT(0.33),
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
            .ast = AST(AST_BEXPR_VV(VM_FLOAT(81.3), BINARY_GT, VM_INT(90))),
            .bc = {
                .values = (ms_Value[]){
                    VM_FLOAT(81.3),
                    VM_INT(90),
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
            .ast = AST(AST_BEXPR_VV(VM_INT(1000), BINARY_GE, VM_INT(10000))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(1000),
                    VM_INT(10000),
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
            .ast = AST(AST_BEXPR_VV(VM_BOOL(true), BINARY_AND, VM_BOOL(false))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(true),
                    VM_BOOL(false),
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
            .val = "true && !true",
            .ast = AST(AST_BEXPR_VE(VM_BOOL(true), BINARY_AND, AST_UEXPR_V(UNARY_NOT, VM_BOOL(true)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(true),
                    VM_BOOL(true),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_NOT, 0),
                    VM_OPC(OPC_AND, 0),
                },
                .nops = 4, .nvals = 2,
            }
        },
        {
            .val = "1 || null",
            .ast = AST(AST_BEXPR_VV(VM_INT(1), BINARY_OR, VM_NULL())),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(1),
                    VM_NULL(),
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

MunitResult prs_TestParseExprPrecedence(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "false || false && true",
            .ast = AST(AST_BEXPR_VE(VM_BOOL(false), BINARY_OR, AST_BEXPR_VV(VM_BOOL(false), BINARY_AND, VM_BOOL(true)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(false),
                    VM_BOOL(false),
                    VM_BOOL(true),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_AND, 0),
                    VM_OPC(OPC_OR, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "(false || false) && true",
            .ast = AST(AST_BEXPR_EV(AST_BEXPR_VV(VM_BOOL(false), BINARY_OR, VM_BOOL(false)), BINARY_AND, VM_BOOL(true))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(false),
                    VM_BOOL(false),
                    VM_BOOL(true),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_OR, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_AND, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "(false || !false) && true",
            .ast = AST(AST_BEXPR_EV(AST_BEXPR_VE(VM_BOOL(false), BINARY_OR, AST_UEXPR_V(UNARY_NOT, VM_BOOL(false))), BINARY_AND, VM_BOOL(true))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(false),
                    VM_BOOL(false),
                    VM_BOOL(true),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_NOT, 0),
                    VM_OPC(OPC_OR, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_AND, 0),
                },
                .nops = 6, .nvals = 3,
            }
        },
        {
            .val = "false == false != true",
            .ast = AST(AST_BEXPR_EV(AST_BEXPR_VV(VM_BOOL(false), BINARY_EQ, VM_BOOL(false)), BINARY_NOT_EQ, VM_BOOL(true))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(false),
                    VM_BOOL(false),
                    VM_BOOL(true),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_EQ, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_NOT_EQ, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "false == (false != true)",
            .ast = AST(AST_BEXPR_VE(VM_BOOL(false), BINARY_EQ, AST_BEXPR_VV(VM_BOOL(false), BINARY_NOT_EQ, VM_BOOL(true)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_BOOL(false),
                    VM_BOOL(false),
                    VM_BOOL(true),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_NOT_EQ, 0),
                    VM_OPC(OPC_EQ, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 > 3.6 < 8",
            .ast = AST(AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_GT, VM_FLOAT(3.6)), BINARY_LT, VM_INT(8))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_FLOAT(3.6),
                    VM_INT(8),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_GT, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_LT, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 > (3.6 < 8)",
            .ast = AST(AST_BEXPR_VE(VM_INT(7), BINARY_GT, AST_BEXPR_VV(VM_FLOAT(3.6), BINARY_GT, VM_INT(8)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_FLOAT(3.6),
                    VM_INT(8),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_LT, 0),
                    VM_OPC(OPC_GT, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 > 3.6 >= 8",
            .ast = AST(AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_GT, VM_FLOAT(3.6)), BINARY_GE, VM_INT(8))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_FLOAT(3.6),
                    VM_INT(8),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_GT, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_GE, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 | 2 & 15",
            .ast = AST(AST_BEXPR_VE(VM_INT(7), BINARY_BITWISE_OR, AST_BEXPR_VV(VM_INT(2), BINARY_BITWISE_AND, VM_INT(15)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(2),
                    VM_INT(15),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_BITWISE_AND, 0),
                    VM_OPC(OPC_BITWISE_OR, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 @ 2 & 15",
            .ast = AST(AST_BEXPR_VE(VM_INT(7), BINARY_BITWISE_XOR, AST_BEXPR_VV(VM_INT(2), BINARY_BITWISE_AND, VM_INT(15)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(2),
                    VM_INT(15),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_BITWISE_AND, 0),
                    VM_OPC(OPC_BITWISE_XOR, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 @ 2 | 15",
            .ast = AST(AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_XOR, VM_INT(2)), BINARY_BITWISE_OR, VM_INT(15))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(2),
                    VM_INT(15),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_BITWISE_XOR, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_BITWISE_OR, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 @ (2 | 15)",
            .ast = AST(AST_BEXPR_VE(VM_INT(7), BINARY_BITWISE_XOR, AST_BEXPR_VV(VM_INT(2), BINARY_BITWISE_OR, VM_INT(15)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(2),
                    VM_INT(15),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_BITWISE_OR, 0),
                    VM_OPC(OPC_BITWISE_XOR, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "(7 | 2) & 15",
            .ast = AST(AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_OR, VM_INT(2)), BINARY_BITWISE_AND, VM_INT(15))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(2),
                    VM_INT(15),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_BITWISE_OR, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_BITWISE_AND, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "(7 | 2) & ~15",
            .ast = AST(AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_OR, VM_INT(2)), BINARY_BITWISE_AND, AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(15)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(2),
                    VM_INT(15),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_BITWISE_OR, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_BITWISE_NOT, 0),
                    VM_OPC(OPC_BITWISE_AND, 0),
                },
                .nops = 6, .nvals = 3,
            }
        },
        {
            .val = "~(7 | 2) & 15",
            .ast = AST(AST_BEXPR_EV(AST_UEXPR_E(UNARY_BITWISE_NOT, AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_OR, VM_INT(2))), BINARY_BITWISE_AND, VM_INT(15))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(2),
                    VM_INT(15),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_BITWISE_OR, 0),
                    VM_OPC(OPC_BITWISE_NOT, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_BITWISE_AND, 0),
                },
                .nops = 6, .nvals = 3,
            }
        },
        {
            .val = "15 | 1 << 2",
            .ast = AST(AST_BEXPR_VE(VM_INT(15), BINARY_BITWISE_OR, AST_BEXPR_VV(VM_INT(1), BINARY_SHIFT_LEFT, VM_INT(2)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(15),
                    VM_INT(1),
                    VM_INT(2),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_SHIFT_LEFT, 0),
                    VM_OPC(OPC_BITWISE_OR, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "(15 | 1) << 2",
            .ast = AST(AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(15), BINARY_BITWISE_OR, VM_INT(1)), BINARY_SHIFT_LEFT, VM_INT(2))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(15),
                    VM_INT(1),
                    VM_INT(2),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_BITWISE_OR, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_SHIFT_LEFT, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 + 3.6 - 8",
            .ast = AST(AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_PLUS, VM_FLOAT(3.6)), BINARY_MINUS, VM_INT(8))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_FLOAT(3.6),
                    VM_INT(8),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_ADD, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_SUBTRACT, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 + (3.6 - 8)",
            .ast = AST(AST_BEXPR_VE(VM_INT(7), BINARY_PLUS, AST_BEXPR_VV(VM_FLOAT(3.6), BINARY_MINUS, VM_INT(8)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_FLOAT(3.6),
                    VM_INT(8),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_SUBTRACT, 0),
                    VM_OPC(OPC_ADD, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 + (3.6 - -8)",
            .ast = AST(AST_BEXPR_VE(VM_INT(7), BINARY_PLUS, AST_BEXPR_VE(VM_FLOAT(3.6), BINARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_INT(8))))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_FLOAT(3.6),
                    VM_INT(8),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_NEGATE, 0),
                    VM_OPC(OPC_SUBTRACT, 0),
                    VM_OPC(OPC_ADD, 0),
                },
                .nops = 6, .nvals = 3,
            }
        },
        {
            .val = "(7 + 3.6) - -8",
            .ast = AST(AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_PLUS, VM_FLOAT(3.6)), BINARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_INT(8)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_FLOAT(3.6),
                    VM_INT(8),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_ADD, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_NEGATE, 0),
                    VM_OPC(OPC_SUBTRACT, 0),
                },
                .nops = 6, .nvals = 3,
            }
        },
        {
            .val = "7 + 3.6 * -8",
            .ast = AST(AST_BEXPR_VE(VM_INT(7), BINARY_PLUS, AST_BEXPR_VE(VM_FLOAT(3.6), BINARY_TIMES, AST_UEXPR_V(UNARY_MINUS, VM_INT(8))))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_FLOAT(3.6),
                    VM_INT(8),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_NEGATE, 0),
                    VM_OPC(OPC_MULTIPLY, 0),
                    VM_OPC(OPC_ADD, 0),
                },
                .nops = 6, .nvals = 3,
            }
        },
        {
            .val = "7 / 3.6 * -8",
            .ast = AST(AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_DIVIDE, VM_FLOAT(3.6)), BINARY_TIMES, AST_UEXPR_V(UNARY_MINUS, VM_INT(8)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_FLOAT(3.6),
                    VM_INT(8),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_DIVIDE, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_NEGATE, 0),
                    VM_OPC(OPC_MULTIPLY, 0),
                },
                .nops = 6, .nvals = 3,
            }
        },
        {
            .val = "7 \\ 3.6 % -8",
            .ast = AST(AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_IDIVIDE, VM_FLOAT(3.6)), BINARY_MODULO, AST_UEXPR_V(UNARY_MINUS, VM_INT(8)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_FLOAT(3.6),
                    VM_INT(8),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_IDIVIDE, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_NEGATE, 0),
                    VM_OPC(OPC_MODULO, 0),
                },
                .nops = 6, .nvals = 3,
            }
        },
        {
            .val = "7 ** 4 ** 2",
            .ast = AST(AST_BEXPR_VE(VM_INT(7), BINARY_EXPONENTIATE, AST_BEXPR_VV(VM_INT(4), BINARY_EXPONENTIATE, VM_INT(2)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(4),
                    VM_INT(2),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_EXPONENTIATE, 0),
                    VM_OPC(OPC_EXPONENTIATE, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "(7 ** 4) ** 2",
            .ast = AST(AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_EXPONENTIATE, VM_INT(4)), BINARY_EXPONENTIATE, VM_INT(2))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(4),
                    VM_INT(2),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_EXPONENTIATE, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_EXPONENTIATE, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 ** 4 * 2",
            .ast = AST(AST_BEXPR_VE(VM_INT(7), BINARY_EXPONENTIATE, AST_BEXPR_VV(VM_INT(4), BINARY_TIMES, VM_INT(2)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(4),
                    VM_INT(2),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_MULTIPLY, 0),
                    VM_OPC(OPC_EXPONENTIATE, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 ** 4 + 2",
            .ast = AST(AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_EXPONENTIATE, VM_INT(4)), BINARY_PLUS, VM_INT(2))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(4),
                    VM_INT(2),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_EXPONENTIATE, 0),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_ADD, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
        {
            .val = "7 ** (4 + 2)",
            .ast = AST(AST_BEXPR_VE(VM_INT(7), BINARY_EXPONENTIATE, AST_BEXPR_VV(VM_INT(4), BINARY_PLUS, VM_INT(2)))),
            .bc = {
                .values = (ms_Value[]){
                    VM_INT(7),
                    VM_INT(4),
                    VM_INT(2),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_PUSH, 1),
                    VM_OPC(OPC_PUSH, 2),
                    VM_OPC(OPC_ADD, 0),
                    VM_OPC(OPC_EXPONENTIATE, 0),
                },
                .nops = 5, .nvals = 3,
            }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseFunctionCalls(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "$len()",
            .ast = AST(AST_FNCALL_I(AST_IDENT("$len"), AST_EMPTY_EXPRLIST())),
            .bc = {
                .values = NULL,
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_LOAD_NAME, 0),
                    VM_OPC(OPC_CALL, 0),
                },
                .idents = ((ms_Ident*[]){
                    AST_IDENT("$len"),
                }),
                .nops = 2, .nvals = 0, .nidents = 1
            }
        },
        {
            .val = "foo()",
            .ast = AST(AST_FNCALL_I(AST_IDENT("foo"), AST_EMPTY_EXPRLIST())),
            .bc = {
                .values = NULL,
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_LOAD_NAME, 0),
                    VM_OPC(OPC_CALL, 0),
                },
                .idents = ((ms_Ident*[]){
                    AST_IDENT("foo"),
                }),
                .nops = 2, .nvals = 0, .nidents = 1
            }
        },
        {
            .val = "$len(\"string\")",
            .ast = AST(AST_FNCALL_I(AST_IDENT("$len"), AST_EXPRLIST(1, &AST_UEXPR_V(UNARY_NONE, VM_STR("\"string\""))))),
            .bc = {
                .values = (ms_Value[]){
                    VM_STR("\"string\""),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_LOAD_NAME, 0),
                    VM_OPC(OPC_CALL, 0),
                },
                .idents = ((ms_Ident*[]){
                    AST_IDENT("$len"),
                }),
                .nops = 3, .nvals = 1, .nidents = 1
            }
        },
        {
            .val = "foo(\"string\")",
            .ast = AST(AST_FNCALL_I(AST_IDENT("foo"), AST_EXPRLIST(1, &AST_UEXPR_V(UNARY_NONE, VM_STR("\"string\""))))),
            .bc = {
                .values = (ms_Value[]){
                    VM_STR("\"string\""),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_LOAD_NAME, 0),
                    VM_OPC(OPC_CALL, 0),
                },
                .idents = ((ms_Ident*[]){
                    AST_IDENT("foo"),
                }),
                .nops = 3, .nvals = 1, .nidents = 1
            }
        },
        {
            .val = "foo()()",
            .ast = AST(AST_FNCALL_E(AST_FNCALL_I(AST_IDENT("foo"), AST_EMPTY_EXPRLIST()), AST_EMPTY_EXPRLIST())),
            .bc = {
                .values = NULL,
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_LOAD_NAME, 0),
                    VM_OPC(OPC_CALL, 0),
                    VM_OPC(OPC_CALL, 0),
                },
                .idents = ((ms_Ident*[]){
                    AST_IDENT("foo"),
                }),
                .nops = 3, .nvals = 0, .nidents = 1
            }
        },
        {
            .val = "bar(\"baz\")()",
            .ast = AST(AST_FNCALL_E(AST_FNCALL_I(AST_IDENT("bar"), AST_EXPRLIST(1, &AST_UEXPR_V(UNARY_NONE, VM_STR("\"baz\"")))), AST_EMPTY_EXPRLIST())),
            .bc = {
                .values = (ms_Value[]){
                    VM_STR("\"baz\""),
                },
                .code = (ms_VMOpCode[]){
                    VM_OPC(OPC_PUSH, 0),
                    VM_OPC(OPC_LOAD_NAME, 0),
                    VM_OPC(OPC_CALL, 0),
                    VM_OPC(OPC_CALL, 0),
                },
                .idents = ((ms_Ident*[]){
                    AST_IDENT("bar"),
                }),
                .nops = 4, .nvals = 1, .nidents = 1
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

MunitResult CompareAST(const ms_AST *ast1, const ms_AST *ast2) {
    munit_assert_non_null(ast1);
    munit_assert_non_null(ast2);
    return CompareExpressions(ast1, ast2);
}

MunitResult CompareExpressions(const ms_Expr *expr1, const ms_Expr *expr2) {
    munit_assert_non_null(expr1);
    munit_assert_non_null(expr2);

    munit_assert_cmp_int(expr1->type, ==, expr2->type);
    switch(expr1->type) {
        case EXPRTYPE_UNARY:
            munit_assert_cmp_int(expr1->cmpnt.u->type, ==, expr2->cmpnt.u->type);
            munit_assert_cmp_int(expr1->cmpnt.u->op, ==, expr1->cmpnt.u->op);
            CompareExpressionAtoms(expr1->cmpnt.u->type, &expr1->cmpnt.u->atom, &expr2->cmpnt.u->atom);
            break;
        case EXPRTYPE_BINARY:
            munit_assert_cmp_int(expr1->cmpnt.b->ltype, ==, expr2->cmpnt.b->ltype);
            CompareExpressionAtoms(expr1->cmpnt.b->ltype, &expr1->cmpnt.b->latom, &expr2->cmpnt.b->latom);
            munit_assert_cmp_int(expr1->cmpnt.b->op, ==, expr1->cmpnt.b->op);
            munit_assert_cmp_int(expr1->cmpnt.b->rtype, ==, expr2->cmpnt.b->rtype);
            CompareExpressionAtoms(expr1->cmpnt.b->rtype, &expr1->cmpnt.b->ratom, &expr2->cmpnt.b->ratom);
            break;
    }

    return MUNIT_OK;
}

MunitResult CompareExpressionAtoms(ms_ExprAtomType type, const ms_ExprAtom *atom1, const ms_ExprAtom *atom2) {
    munit_assert_non_null(atom1);
    munit_assert_non_null(atom2);

    switch(type) {
        case EXPRATOM_EXPRESSION:
            CompareExpressions(atom1->expr, atom2->expr);
            break;
        case EXPRATOM_VALUE:
            CompareValues(&atom1->val, &atom2->val);
            break;
        case EXPRATOM_IDENT:
            CompareIdent(atom1->ident, atom2->ident);
            break;
        case EXPRATOM_EXPRLIST:
            CompareExpressionList(atom1->list, atom2->list);
            break;
        case EXPRATOM_EMPTY:
            munit_assert(false);
            break;
    }

    return MUNIT_OK;
}

MunitResult CompareByteCode(const ms_VMByteCode *bc1, const ms_VMByteCode *bc2) {
    munit_assert_non_null(bc1);
    munit_assert_non_null(bc2);
    munit_assert_cmp_int(bc1->nops, ==, bc2->nops);
    munit_assert_cmp_int(bc1->nvals, ==, bc2->nvals);

    for (size_t i = 0; i < bc1->nops; i++) {
        munit_assert_cmp_int(bc1->code[i], ==, bc2->code[i]);
    }

    for (size_t i = 0; i < bc1->nvals; i++) {
        CompareValues(&bc1->values[i], &bc2->values[i]);
    }

    for (size_t i = 0; i < bc1->nidents; i++) {
        CompareIdent(bc1->idents[i], bc2->idents[i]);
    }

    return MUNIT_OK;
}

MunitResult CompareExpressionList(const ms_ExprList *el1, const ms_ExprList *el2) {
    munit_assert_non_null(el1);
    munit_assert_non_null(el2);

    munit_assert_cmp_int(dsarray_len(el1), ==, dsarray_len(el2));
    size_t len = dsarray_len(el1);
    for (size_t i = 0; i < len; i++) {
        const ms_Expr *expr1 = dsarray_get(el1, i);
        const ms_Expr *expr2 = dsarray_get(el2, i);
        CompareExpressions(expr1, expr2);
    }

    return MUNIT_OK;
}

MunitResult CompareIdent(const ms_Ident *id1, const ms_Ident *id2) {
    munit_assert_non_null(id1);
    munit_assert_non_null(id1);

    const char *s1 = dsbuf_char_ptr(id1);
    const char *s2 = dsbuf_char_ptr(id2);
    munit_logf(MUNIT_LOG_INFO, "  ident1='%s'", s1);
    munit_logf(MUNIT_LOG_INFO, "  ident2='%s'", s2);
    munit_assert_string_equal(s1, s2);

    return MUNIT_OK;
}

MunitResult CompareValues(const ms_Value *val1, const ms_Value *val2) {
    munit_assert_non_null(val1);
    munit_assert_non_null(val2);

    munit_assert_cmp_int(val1->type, ==, val2->type);
    switch (val1->type) {
        case MSVAL_FLOAT:
            munit_assert_cmp_double(val1->val.f, ==, val2->val.f);
            break;
        case MSVAL_INT:
            munit_assert_cmp_int(val1->val.i, ==, val2->val.i);
            break;
        case MSVAL_STR:
            munit_assert_true(dsbuf_equals(val1->val.s, val2->val.s));
            break;
        case MSVAL_BOOL:
            munit_assert(val1->val.b == val2->val.b);
            break;
        case MSVAL_NULL:
            munit_assert(val1->val.n == val2->val.n);
            break;
    }

    return MUNIT_OK;
}

void CleanAST(ms_AST *ast) {
    CleanExpression(ast);
}

void CleanExpression(ms_Expr *expr) {
    if (!expr) { return; }

    switch(expr->type) {
        case EXPRTYPE_UNARY:
            CleanExpressionAtom(expr->cmpnt.u->type, &expr->cmpnt.u->atom);
            return;
        case EXPRTYPE_BINARY:
            CleanExpressionAtom(expr->cmpnt.b->ltype, &expr->cmpnt.b->latom);
            CleanExpressionAtom(expr->cmpnt.b->rtype, &expr->cmpnt.b->ratom);
            return;
    }
}

void CleanExpressionAtom(ms_ExprAtomType type, ms_ExprAtom *atom) {
    switch(type) {
        case EXPRATOM_EXPRESSION:
            CleanExpression(atom->expr);
            break;
        case EXPRATOM_VALUE:
            if (atom->val.type == MSVAL_STR) {
                dsbuf_destroy(atom->val.val.s);
                atom->val.val.s = NULL;
            }
            break;
        case EXPRATOM_IDENT:
            dsbuf_destroy(atom->ident);
            atom->ident = NULL;
            break;
        case EXPRATOM_EXPRLIST:
            CleanExpressionList(atom->list);
            atom->list = NULL;
            break;
        case EXPRATOM_EMPTY:
            break;
    }
}

void CleanExpressionList(ms_ExprList *el) {
    if (!el) { return; }

    size_t len = dsarray_len(el);
    for (size_t i = 0; i < len; i++) {
        ms_Expr *expr = dsarray_get(el, i);
        CleanExpression(expr);
    }
    dsarray_destroy(el);
}

void CleanByteCode(ms_VMByteCode *bc) {
    for (size_t i = 0; i < bc->nvals; i++) {
        if (bc->values[i].type == MSVAL_STR) {
            dsbuf_destroy(bc->values[i].val.s);
            bc->values[i].val.s = NULL;
        }
    }

    for (size_t i = 0; i < bc->nidents; i++) {
        dsbuf_destroy(bc->idents[i]);
        bc->idents[i] = NULL;
    }
}

void CleanParseResultTuple(ParseResultTuple *tuple) {
    CleanAST(&tuple->ast);
    CleanByteCode(&tuple->bc);
}

MunitResult TestParseResultTuple(ParseResultTuple *tuples, size_t len) {
    ms_Parser *prs = ms_ParserNew();
    munit_assert_non_null(prs);

    for (size_t i = 0; i < len; i++) {
        ParseResultTuple *tuple = &tuples[i];
        ms_ParserInitString(prs, tuple->val);
        munit_logf(MUNIT_LOG_INFO, "  code='%s'", tuple->val);

        const ms_AST *ast;
        ms_VMByteCode *code;
        const ms_ParseError *err;
        ms_ParseResult pres = ms_ParserParse(prs, &code, &ast, &err);

        munit_assert_cmp_int(pres, !=, PARSE_ERROR);
        munit_assert_non_null(code);
        munit_assert_non_null(ast);
        munit_assert_null(err);

        CompareByteCode(code, &tuple->bc);
        CompareAST(ast, &tuple->ast);

        ms_VMByteCodeDestroy(code);
        CleanParseResultTuple(tuple);
    }

    ms_ParserDestroy(prs);
    return MUNIT_OK;
}
