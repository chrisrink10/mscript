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

typedef union {
    ms_AST *module;
    ms_Stmt stmt;
    ms_Expr expr;
} ASTComponent;

typedef enum {
    ASTCMPNT_MODULE,
    ASTCMPNT_STMT,
    ASTCMPNT_EXPR
} ASTComponentType;

typedef struct ParseResultTuple {
    const char *val;        /** Value of the input token */
    ASTComponent cmpnt;     /** Abstract syntax tree produced from parsing val */
    ASTComponentType type;  /** Type of AST component */
    ms_VMByteCode bc;       /** Type of token value should be lexed into */
} ParseResultTuple;

/*
 * TEST DEFINITIONS
 */

static char* bad_code[] = {
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
    "true.",
    "name.",
    "name.second.",
    "break \"string\"",
    "continue 3.14",
    "del",
    "del for",
    "for var := 10 { }",
    "for var 10 := true { }",
    "for var x := { }",
    "for var x := 1 { }",
    "for var x := 1 : { }",
    "for var x := 1 : 10 : { }",
    "for 10 := true { }",
    "for := true { }",
    "for x := { }",
    "for x := 1 { }",
    "for x := 1 : { }",
    "for x := 1 : 10 : { }",
    "for x += 1 { }",
    "for i in { }",
    "for { }",
    "for true",
    "for true {",
    "if { }",
    "if true {",
    "if true { } else {",
    "if true { } else if { }",
    "else { }",
    "import",
    "import 10",
    "import Sys :",
    "import Sys : true",
    "merge",
    "merge 10",
    "merge x :=",
    "merge var := y",
    "return )",
    "return func",
    "func { }",
    "var := 10",
    "var name :=",
    "10 :=",
    "10 := 12",
    NULL
};

static MunitParameterEnum parse_error_params[] = {
    { "code", bad_code },
    { NULL, NULL }
};

MunitTest parser_tests[] = {
    {
        "/ParseErrors",
        prs_TestParseErrors,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        parse_error_params
    },
    {
        "/Literals",
        prs_TestParseLiterals,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/UnaryExpressions",
        prs_TestParseUnaryExprs,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/BinaryExpressions",
        prs_TestParseBinaryExprs,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/OperatorPrecedence",
        prs_TestParseExprPrecedence,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/FunctionCalls",
        prs_TestParseFunctionCalls,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/DeleteStatement",
        prs_TestParseDeleteStatement,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/ForIncrStatements",
        prs_TestParseForIncStatements,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/ForIterStatements",
        prs_TestParseForIterStatements,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/ForExprStatements",
        prs_TestParseForExprStatements,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/IfStatement",
        prs_TestParseIfStatements,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/ImportStatement",
        prs_TestParseImportStatement,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/MergeStatement",
        prs_TestParseMergeStatement,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/ReturnStatement",
        prs_TestParseReturnStatement,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/FuncDeclaration",
        prs_TestParseFuncDeclaration,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/Declaration",
        prs_TestParseDeclaration,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/Assignment",
        prs_TestParseAssignment,
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

static MunitResult CompareASTToComponent(const ms_AST *ast, ASTComponentType type, ASTComponent *cmpnt);
static MunitResult CompareAST(const ms_AST *ast1, const ms_AST *ast2);
static MunitResult CompareStatements(const ms_Stmt *stmt1, const ms_Stmt *stmt2);
static MunitResult CompareArgumentList(const ms_ArgList *al1, const ms_ArgList *al2);
static MunitResult CompareBlocks(const ms_StmtBlock *blk1, const ms_StmtBlock *blk2);
static MunitResult CompareForStatement(const ms_StmtFor *for1, const ms_StmtFor *for2);
static MunitResult CompareIfElseStatement(const ms_StmtIfElse *elif1, const ms_StmtIfElse *elif2);
static MunitResult CompareDeclarations(const ms_StmtDeclaration *decl1, const ms_StmtDeclaration *decl2);
static MunitResult CompareExpressions(const ms_Expr *expr1, const ms_Expr *expr2);
static MunitResult CompareExpressionAtoms(ms_ExprAtomType type, const ms_ExprAtom *atom1, const ms_ExprAtom *atom2);
static MunitResult CompareByteCode(const ms_VMByteCode *bc1, const ms_VMByteCode *bc2);
static MunitResult CompareExpressionList(const ms_ExprList *el1, const ms_ExprList *el2);
static MunitResult CompareIdent(const ms_Ident *id1, const ms_Ident *id2);
static MunitResult CompareValues(const ms_Value *val1, const ms_Value *val2);
static MunitResult CompareFunctions(const ms_ValFunc *fn1, const ms_ValFunc *fn2);

static void CleanAST(ms_AST *ast);
static void CleanStatement(ms_Stmt *stmt);
static void CleanBlock(ms_StmtBlock *block);
static void CleanForStatement(ms_StmtFor *forstmt);
static void CleanIfElseStatement(ms_StmtIfElse *elif);
static void CleanDeclaration(ms_StmtDeclaration *decl);
static void CleanExpression(ms_Expr *expr);
static void CleanExpressionAtom(ms_ExprAtomType type, ms_ExprAtom *atom);
static void CleanFunction(ms_ValFunc *func);
static void CleanExpressionList(ms_ExprList *list);
static void CleanByteCode(ms_VMByteCode *bc);
static void CleanParseResultTuple(ParseResultTuple *tuple);
static MunitResult TestParseResultTuple(ParseResultTuple *tuples, size_t len);

/*
 * AST COMPOSITION MACROS
 *
 * Writing struct literals for each statement and expression struct was
 * prohibitively difficult for definiting enough test cases, so I created
 * macros that could compose each element at a higher level and save me
 * a tremendous amount of time in writing (and hopefully improve readability).
 * Sadly, the complexity and nesting of these macros causes my IDE (CLion)
 * to drag on this file -_-
 */

// "Private" expression composition macros
#define AST(expr) (expr)
#define AST_EXPR(arity, component)  ((ms_Expr){ .type = arity, .cmpnt = component })
#define AST_EXPRCOMPONENT(field, memb) \
                                    ((ms_ExprComponent){ .field = &(memb) })
#define AST_EXPRCOMPONENT_UNARY(atomtype, opname, atomv) \
                                    AST_EXPRCOMPONENT(u, ((ms_ExprUnary){ .type = atomtype, .op = opname, .atom = atomv }))
#define AST_EXPRCOMPONENT_BINARY(latomv, latomtype, opname, ratomv, ratomtype) \
                                    AST_EXPRCOMPONENT(b, ((ms_ExprBinary){ .latom = latomv, .ltype = latomtype, .op = opname, .ratom = ratomv, .rtype = ratomtype}))
#define AST_EXPRATOM_EXPR(eatom)    ((ms_ExprAtom){ .expr = &eatom })
#define AST_EXPRATOM_VAL(vatom)     ((ms_ExprAtom){ .val = vatom })
#define AST_EXPRATOM_IDENT(iatom)   ((ms_ExprAtom){ .ident = iatom })
#define AST_EXPRATOM_LIST(latom)    ((ms_ExprAtom){ .list = latom })
#define AST_UNARY(atp, uop, v)      AST_EXPR(EXPRTYPE_UNARY, AST_EXPRCOMPONENT_UNARY(atp, uop, v))
#define AST_BINARY(latp, lv, op, ratp, rv) \
                                    AST_EXPR(EXPRTYPE_BINARY, AST_EXPRCOMPONENT_BINARY(lv, latp, op, rv, ratp))

// Expression macros
#define AST_UEXPR_I(uop, v)         AST_UNARY(EXPRATOM_IDENT, uop, AST_EXPRATOM_IDENT(v))
#define AST_UEXPR_V(uop, v)         AST_UNARY(EXPRATOM_VALUE, uop, AST_EXPRATOM_VAL(v))
#define AST_UEXPR_E(uop, v)         AST_UNARY(EXPRATOM_EXPRESSION, uop, AST_EXPRATOM_EXPR(v))
#define AST_BEXPR_VV(lv, bop, rv)   AST_BINARY(EXPRATOM_VALUE, AST_EXPRATOM_VAL(lv), bop, EXPRATOM_VALUE, AST_EXPRATOM_VAL(rv))
#define AST_BEXPR_VE(lv, bop, re)   AST_BINARY(EXPRATOM_VALUE, AST_EXPRATOM_VAL(lv), bop, EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(re))
#define AST_BEXPR_VI(lv, bop, ri)   AST_BINARY(EXPRATOM_VALUE, AST_EXPRATOM_VAL(lv), bop, EXPRATOM_IDENT, AST_EXPRATOM_IDENT(ri))
#define AST_BEXPR_EV(le, bop, rv)   AST_BINARY(EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(le), bop, EXPRATOM_VALUE, AST_EXPRATOM_VAL(rv))
#define AST_BEXPR_EE(le, bop, re)   AST_BINARY(EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(le), bop, EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(re))
#define AST_BEXPR_EI(le, bop, ri)   AST_BINARY(EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(le), bop, EXPRATOM_IDENT, AST_EXPRATOM_IDENT(ri))
#define AST_BEXPR_II(li, bop, ri)   AST_BINARY(EXPRATOM_IDENT, AST_EXPRATOM_IDENT(li), bop, EXPRATOM_IDENT, AST_EXPRATOM_IDENT(ri))
#define AST_BEXPR_IE(li, bop, re)   AST_BINARY(EXPRATOM_IDENT, AST_EXPRATOM_IDENT(li), bop, EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(re))
#define AST_BEXPR_IV(li, bop, rv)   AST_BINARY(EXPRATOM_IDENT, AST_EXPRATOM_IDENT(li), bop, EXPRATOM_VALUE, AST_EXPRATOM_VAL(rv))
#define AST_FNCALL_E(e, lst)        AST_BINARY(EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(e), BINARY_CALL, EXPRATOM_EXPRLIST, AST_EXPRATOM_LIST(lst))
#define AST_FNCALL_I(id, lst)       AST_BINARY(EXPRATOM_IDENT, AST_EXPRATOM_IDENT(id), BINARY_CALL, EXPRATOM_EXPRLIST, AST_EXPRATOM_LIST(lst))
#define AST_IDENT(v)                (dsbuf_new_l(v, sizeof(v)-1)) /* subtract one to ignore the terminating NUL */
#define AST_EXPRLIST(l, ...)        (dsarray_new_lit((void **)(((ms_Expr*[]){ __VA_ARGS__ , })), l, l, NULL, NULL))
#define AST_EMPTY_EXPRLIST()        (dsarray_new_cap(1, NULL, NULL))

// "Private" statement composition macros
#define AST_STMT(tp, component)         ((ms_Stmt){ .type = tp, .cmpnt = component })
#define AST_STMTCOMPONENT(field, memb)  ((ms_StmtComponent){ .field = &(memb) })
#define AST_STMT_FOR(fortp, c, f, blk)  ((ms_StmtFor){ .type = fortp, .clause = ((ms_StmtForClause){ .f = &(c) }), .block = blk })
#define AST_STMT_FOR_INC(id, start, stop, incr, decl) \
                                        ((ms_StmtForIncrement){ .ident = &(id), .init = &(start), .end = &(stop), .step = &(incr), .declare = decl })
#define AST_STMT_FOR_ITER(id, e, d)     ((ms_StmtForIterator){ .ident = &(id), .iter = &(e), .declare = d })
#define AST_STMT_FOR_EXPR(e)            ((ms_StmtForExpr){ .expr = &(e) })
#define AST_STMT_IF(e, b, elseif)       ((ms_StmtIf){ .expr = &(e), .block = b, .elif = elseif })
#define AST_STMT_ELSE(b)                ((ms_StmtElse){ .block = b })
#define AST_STMT_ELIF(tp, m, c)         &((ms_StmtIfElse){ .type = tp, .clause = { .m = &(c) } })

// Statement macros
#define AST_BREAK()                 AST_STMT(STMTTYPE_BREAK, AST_STMTCOMPONENT(brk, NULL))
#define AST_CONTINUE()              AST_STMT(STMTTYPE_CONTINUE, AST_STMTCOMPONENT(cont, NULL))
#define AST_DEL(e)                  AST_STMT(STMTTYPE_DELETE, AST_STMTCOMPONENT(del, ((ms_StmtDelete){ .expr = &(e) })))
#define AST_FOR_INC(id, start, stop, incr, decl, b) \
                                    AST_STMT(STMTTYPE_FOR, AST_STMTCOMPONENT(forstmt, AST_STMT_FOR(FORSTMT_INCREMENT, AST_STMT_FOR_INC(id, start, stop, incr, decl), inc, b)))
#define AST_FOR_INC_1(id, start, stop, decl, b) \
                                    AST_STMT(STMTTYPE_FOR, AST_STMTCOMPONENT(forstmt, AST_STMT_FOR(FORSTMT_INCREMENT, AST_STMT_FOR_INC(id, start, stop, AST_UEXPR_V(UNARY_NONE, VM_INT(1)), decl), inc, b)))
#define AST_FOR_ITER(id, e, d, b)   AST_STMT(STMTTYPE_FOR, AST_STMTCOMPONENT(forstmt, AST_STMT_FOR(FORSTMT_ITERATOR, AST_STMT_FOR_ITER(id, e, d), iter, b)))
#define AST_FOR_EXPR(e, b)          AST_STMT(STMTTYPE_FOR, AST_STMTCOMPONENT(forstmt, AST_STMT_FOR(FORSTMT_EXPR, AST_STMT_FOR_EXPR(e), expr, b)))
#define AST_IF_ELIF(e, b, elseif)   AST_STMT(STMTTYPE_IF, AST_STMTCOMPONENT(ifstmt, AST_STMT_IF(e, b, elseif)))
#define AST_ELIF_IF(e, b, elseif)   AST_STMT_ELIF(IFELSE_IF, ifstmt, AST_STMT_IF(e, b, elseif))
#define AST_ELIF_ELSE(b)            AST_STMT_ELIF(IFELSE_ELSE, elstmt, AST_STMT_ELSE(b))
#define AST_IMPORT(e, a)            AST_STMT(STMTTYPE_IMPORT, AST_STMTCOMPONENT(import, ((ms_StmtImport){ .ident = &(e), .alias = a })))
#define AST_MERGE(l, r)             AST_STMT(STMTTYPE_MERGE, AST_STMTCOMPONENT(merge, ((ms_StmtMerge){ .left = &(l), .right = &(r) })))
#define AST_RETURN(e)               AST_STMT(STMTTYPE_RETURN, AST_STMTCOMPONENT(ret, ((ms_StmtReturn){ .expr = &(e) })))
#define AST_ASSIGN(i, e)            AST_STMT(STMTTYPE_ASSIGNMENT, AST_STMTCOMPONENT(assign, ((ms_StmtAssignment){ .ident = &(i), .expr = &(e) })))
#define AST_DECL_CMPNT(i, n)        ((ms_StmtDeclaration){ .ident = i, .expr = NULL, .next = n })
#define AST_DECL_CMPNT_V(i, e, n)   ((ms_StmtDeclaration){ .ident = i, .expr = &(e), .next = n })
#define AST_DECLARE(i, n)           AST_STMT(STMTTYPE_DECLARATION, AST_STMTCOMPONENT(decl, AST_DECL_CMPNT(i, n)))
#define AST_DECLARE_V(i, e, n)      AST_STMT(STMTTYPE_DECLARATION, AST_STMTCOMPONENT(decl, AST_DECL_CMPNT_V(i, e, n)))
#define AST_EXPR_STMT(e)            AST_STMT(STMTTYPE_EXPR, AST_STMTCOMPONENT(expr, ((ms_StmtExpression){ .expr = &(e) }))
#define AST_STMT_BLOCK(l, ...)      (dsarray_new_lit((void **)(((ms_Stmt*[]){ __VA_ARGS__ , })), l, l, NULL, NULL))
#define AST_EMPTY_STMT_BLOCK()      (dsarray_new_cap(1, NULL, NULL))

// VM type and bytecode macros
#define VM_OPC(opc, arg)            ms_VMOpCodeWithArg(opc, arg)
#define VM_FLOAT(v)                 ((ms_Value){ .type = MSVAL_FLOAT, .val = (ms_ValData){ .f = v } })
#define VM_INT(v)                   ((ms_Value){ .type = MSVAL_INT,   .val = (ms_ValData){ .i = v } })
#define VM_STR(v)                   ((ms_Value){ .type = MSVAL_STR,   .val = (ms_ValData){ .s = dsbuf_new_l(v, sizeof(v)-1) } })
#define VM_BOOL(v)                  ((ms_Value){ .type = MSVAL_BOOL,  .val = (ms_ValData){ .b = v } })
#define VM_NULL()                   ((ms_Value){ .type = MSVAL_NULL,  .val = (ms_ValData){ .n = MS_VM_NULL_POINTER } })
#define VM_FUNC(id, arglist, b)     ((ms_Value){ .type = MSVAL_FUNC,  .val = (ms_ValData){ .fn = &((ms_ValFunc){ .ident = id, .args = (arglist), .block = b }) } })
#define AST_ARGLIST(l, ...)         (dsarray_new_lit((void **)(((ms_Ident*[]){ __VA_ARGS__ , })), l, l, NULL, NULL))

/*
 * TEST CASE FUNCTIONS
 *
 * The functions below (mostly) contain test cases of abstract syntax tree
 * elements and expected bytecode. Each tests a sort of fuzzy subset of
 * language grammar elements.
 */

MunitResult prs_TestParseErrors(const MunitParameter params[], void *user_data) {
    const char *code = munit_parameters_get(params, "code");
    ms_Parser *prs = ms_ParserNew();
    munit_assert_non_null(prs);

    ms_ParserInitString(prs, code);
    ms_VMByteCode *bc;
    const ms_ParseError *err;
    ms_ParseResult pres = ms_ParserParse(prs, &bc, NULL, &err);

    munit_assert_cmp_int(pres, ==, PARSE_ERROR);
    munit_assert_non_null(err);
    munit_assert_null(bc);

    ms_ParserDestroy(prs);
    return MUNIT_OK;
}

MunitResult prs_TestParseLiterals(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "0",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_INT(0)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_INT(3)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_FLOAT(0.0)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_FLOAT(3.14)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_BOOL(true)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_BOOL(false)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_NULL()),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_MINUS, VM_INT(3)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_E(UNARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_FLOAT(2.72))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NOT, VM_BOOL(true)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_E(UNARY_NOT, AST_UEXPR_V(UNARY_NOT, VM_BOOL(false))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(792)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_E(UNARY_BITWISE_NOT, AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(42))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(7), BINARY_PLUS, VM_FLOAT(3.6)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(3), BINARY_PLUS, AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(3))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(16), BINARY_MINUS, VM_BOOL(false)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(3), BINARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_INT(3))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(2), BINARY_TIMES, VM_FLOAT(3.14)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(17), BINARY_DIVIDE, VM_FLOAT(8.25)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_FLOAT(8.888), BINARY_IDIVIDE, VM_INT(6)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(42), BINARY_MODULO, VM_INT(8)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_FLOAT(5.25), BINARY_EXPONENTIATE, VM_INT(2)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(5), BINARY_SHIFT_LEFT, VM_INT(2)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(183822), BINARY_SHIFT_RIGHT, VM_INT(4)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(13), BINARY_BITWISE_AND, VM_INT(97)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(3), BINARY_BITWISE_OR, VM_INT(15)),
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
            .val = "53 ^ 7",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(53), BINARY_BITWISE_XOR, VM_INT(7)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_FLOAT(46.12), BINARY_LE, VM_INT(73)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_BOOL(true), BINARY_LT, VM_INT(14)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(33), BINARY_NOT_EQ, VM_INT(1988)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(71), BINARY_EQ, VM_FLOAT(0.33)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_FLOAT(81.3), BINARY_GT, VM_INT(90)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(1000), BINARY_GE, VM_INT(10000)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_BOOL(true), BINARY_AND, VM_BOOL(false)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_BOOL(true), BINARY_AND, AST_UEXPR_V(UNARY_NOT, VM_BOOL(true))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(1), BINARY_OR, VM_NULL()),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_BOOL(false), BINARY_OR, AST_BEXPR_VV(VM_BOOL(false), BINARY_AND, VM_BOOL(true))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_BOOL(false), BINARY_OR, VM_BOOL(false)), BINARY_AND, VM_BOOL(true)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VE(VM_BOOL(false), BINARY_OR, AST_UEXPR_V(UNARY_NOT, VM_BOOL(false))), BINARY_AND, VM_BOOL(true)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_BOOL(false), BINARY_EQ, VM_BOOL(false)), BINARY_NOT_EQ, VM_BOOL(true)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_BOOL(false), BINARY_EQ, AST_BEXPR_VV(VM_BOOL(false), BINARY_NOT_EQ, VM_BOOL(true))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_GT, VM_FLOAT(3.6)), BINARY_LT, VM_INT(8)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_GT, AST_BEXPR_VV(VM_FLOAT(3.6), BINARY_GT, VM_INT(8))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_GT, VM_FLOAT(3.6)), BINARY_GE, VM_INT(8)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_BITWISE_OR, AST_BEXPR_VV(VM_INT(2), BINARY_BITWISE_AND, VM_INT(15))),
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
            .val = "7 ^ 2 & 15",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_BITWISE_XOR, AST_BEXPR_VV(VM_INT(2), BINARY_BITWISE_AND, VM_INT(15))),
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
            .val = "7 ^ 2 | 15",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_XOR, VM_INT(2)), BINARY_BITWISE_OR, VM_INT(15)),
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
            .val = "7 ^ (2 | 15)",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_BITWISE_XOR, AST_BEXPR_VV(VM_INT(2), BINARY_BITWISE_OR, VM_INT(15))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_OR, VM_INT(2)), BINARY_BITWISE_AND, VM_INT(15)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_OR, VM_INT(2)), BINARY_BITWISE_AND, AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(15))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_UEXPR_E(UNARY_BITWISE_NOT, AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_OR, VM_INT(2))), BINARY_BITWISE_AND, VM_INT(15)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(15), BINARY_BITWISE_OR, AST_BEXPR_VV(VM_INT(1), BINARY_SHIFT_LEFT, VM_INT(2))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(15), BINARY_BITWISE_OR, VM_INT(1)), BINARY_SHIFT_LEFT, VM_INT(2)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_PLUS, VM_FLOAT(3.6)), BINARY_MINUS, VM_INT(8)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_PLUS, AST_BEXPR_VV(VM_FLOAT(3.6), BINARY_MINUS, VM_INT(8))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_PLUS, AST_BEXPR_VE(VM_FLOAT(3.6), BINARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_INT(8)))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_PLUS, VM_FLOAT(3.6)), BINARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_INT(8))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_PLUS, AST_BEXPR_VE(VM_FLOAT(3.6), BINARY_TIMES, AST_UEXPR_V(UNARY_MINUS, VM_INT(8)))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_DIVIDE, VM_FLOAT(3.6)), BINARY_TIMES, AST_UEXPR_V(UNARY_MINUS, VM_INT(8))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_IDIVIDE, VM_FLOAT(3.6)), BINARY_MODULO, AST_UEXPR_V(UNARY_MINUS, VM_INT(8))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_EXPONENTIATE, AST_BEXPR_VV(VM_INT(4), BINARY_EXPONENTIATE, VM_INT(2))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_EXPONENTIATE, VM_INT(4)), BINARY_EXPONENTIATE, VM_INT(2)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_EXPONENTIATE, AST_BEXPR_VV(VM_INT(4), BINARY_TIMES, VM_INT(2))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_EXPONENTIATE, VM_INT(4)), BINARY_PLUS, VM_INT(2)),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_EXPONENTIATE, AST_BEXPR_VV(VM_INT(4), BINARY_PLUS, VM_INT(2))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_I(AST_IDENT("$len"), AST_EMPTY_EXPRLIST()),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_I(AST_IDENT("foo"), AST_EMPTY_EXPRLIST()),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_I(AST_IDENT("$len"), AST_EXPRLIST(1, &AST_UEXPR_V(UNARY_NONE, VM_STR("\"string\"")))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_I(AST_IDENT("foo"), AST_EXPRLIST(1, &AST_UEXPR_V(UNARY_NONE, VM_STR("\"string\"")))),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_E(AST_FNCALL_I(AST_IDENT("foo"), AST_EMPTY_EXPRLIST()), AST_EMPTY_EXPRLIST()),
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
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_E(AST_FNCALL_I(AST_IDENT("bar"), AST_EXPRLIST(1, &AST_UEXPR_V(UNARY_NONE, VM_STR("\"baz\"")))), AST_EMPTY_EXPRLIST()),
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

MunitResult prs_TestParseDeleteStatement(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "del @global",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DEL(AST_UEXPR_I(UNARY_NONE, AST_IDENT("@global"))),
            .bc = { 0 }
        },
        {
            .val = "del local",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DEL(AST_UEXPR_I(UNARY_NONE, AST_IDENT("local")))
        },
        {
            .val = "del name.second.third",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DEL(AST_BEXPR_EI(AST_BEXPR_II(AST_IDENT("name"), BINARY_GETATTR, AST_IDENT("second")), BINARY_GETATTR, AST_IDENT("third")))
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseForIncStatements(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "for var i := 0 : lim { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_INC_1(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("i")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(0)),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("lim")),
                true,
                AST_EMPTY_STMT_BLOCK()),
            .bc = { 0 }
        },
        {
            .val = "for var i := 0 : lim : 2 { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_INC(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("i")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(0)),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("lim")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(2)),
                true,
                AST_EMPTY_STMT_BLOCK()),
            .bc = { 0 }
        },
        {
            .val = "for i := 0 : lim { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_INC_1(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("i")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(0)),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("lim")),
                false,
                AST_EMPTY_STMT_BLOCK()),
            .bc = { 0 }
        },
        {
            .val = "for i := 0 : lim : 2 { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_INC(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("i")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(0)),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("lim")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(2)),
                false,
                AST_EMPTY_STMT_BLOCK()),
            .bc = { 0 }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseForIterStatements(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "for var i in range { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_ITER(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("i")),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("range")),
                true,
                AST_EMPTY_STMT_BLOCK()),
            .bc = { 0 }
        },
        {
            .val = "for i in range { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_ITER(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("i")),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("range")),
                false,
                AST_EMPTY_STMT_BLOCK()),
            .bc = { 0 }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseForExprStatements(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "for cond { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_EXPR(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT("cond")),
                AST_EMPTY_STMT_BLOCK()),
            .bc = { 0 }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseIfStatements(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "if cost >= money { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IF_ELIF(AST_BEXPR_II(AST_IDENT("cost"), BINARY_GE, AST_IDENT("money")), AST_EMPTY_STMT_BLOCK(), NULL),
            .bc = { 0 }
        },
        {
            .val = "if cost >= money { \n"
                   "    // do nothing\n"
                   "} else { \n"
                   "    // also do nothing\n"
                   "}",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IF_ELIF(
                AST_BEXPR_II(AST_IDENT("cost"), BINARY_GE, AST_IDENT("money")),
                AST_EMPTY_STMT_BLOCK(),
                AST_ELIF_ELSE(
                    AST_EMPTY_STMT_BLOCK()
                )
            ),
            .bc = { 0 }
        },
        {
            .val = "if cost >= money { \n"
                   "    // freak out\n"
                   "} else if cost == money { \n"
                   "    // sigh a breath of relief\n"
                   "} else { \n"
                   "    // all good\n"
                   "}",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IF_ELIF(
                AST_BEXPR_II(AST_IDENT("cost"), BINARY_GE, AST_IDENT("money")),
                AST_EMPTY_STMT_BLOCK(),
                AST_ELIF_IF(
                    AST_BEXPR_II(AST_IDENT("cost"), BINARY_EQ, AST_IDENT("money")),
                    AST_EMPTY_STMT_BLOCK(),
                    AST_ELIF_ELSE(
                        AST_EMPTY_STMT_BLOCK()
                    )
                )
            ),
            .bc = { 0 }
        },
        {
            .val = "if pct >= 0.9 { \n"
                   "    return \"A\"\n"
                   "} else if pct >= 0.8 { \n"
                   "    return \"B\"\n"
                   "} else if pct >= 0.7 { \n"
                   "    return \"C\"\n"
                   "} else {\n"
                   "    return \"F\"\n"
                   "}",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IF_ELIF(
                AST_BEXPR_IV(AST_IDENT("pct"), BINARY_GE, VM_FLOAT(0.9)),
                AST_STMT_BLOCK(1, &AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_STR("\"A\"")))),
                AST_ELIF_IF(
                    AST_BEXPR_IV(AST_IDENT("pct"), BINARY_GE, VM_FLOAT(0.8)),
                    AST_STMT_BLOCK(1, &AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_STR("\"B\"")))),
                    AST_ELIF_IF(
                        AST_BEXPR_IV(AST_IDENT("pct"), BINARY_GE, VM_FLOAT(0.7)),
                        AST_STMT_BLOCK(1, &AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_STR("\"C\"")))),
                        AST_ELIF_ELSE(
                            AST_STMT_BLOCK(1, &AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_STR("\"F\""))))
                        )
                    )
                )
            ),
            .bc = { 0 }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseImportStatement(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "import Sys",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IMPORT(AST_UEXPR_I(UNARY_NONE, AST_IDENT("Sys")), NULL),
            .bc = { 0 }
        },
        {
            .val = "import Http.Server",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IMPORT(AST_BEXPR_II(AST_IDENT("Http"), BINARY_GETATTR, AST_IDENT("Server")), NULL),
            .bc = { 0 }
        },
        {
            .val = "import Http.Server : srv",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IMPORT(AST_BEXPR_II(AST_IDENT("Http"), BINARY_GETATTR, AST_IDENT("Server")), AST_IDENT("srv")),
            .bc = { 0 }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseMergeStatement(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "merge name := other",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_MERGE(AST_UEXPR_I(UNARY_NONE, AST_IDENT("name")), AST_UEXPR_I(UNARY_NONE, AST_IDENT("other"))),
            .bc = { 0 }
        },
        {
            .val = "merge name.second := other",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_MERGE(AST_BEXPR_II(AST_IDENT("name"), BINARY_GETATTR, AST_IDENT("second")), AST_UEXPR_I(UNARY_NONE, AST_IDENT("other"))),
            .bc = { 0 }
        },
        {
            .val = "merge @global := other",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_MERGE(AST_UEXPR_I(UNARY_NONE, AST_IDENT("@global")), AST_UEXPR_I(UNARY_NONE, AST_IDENT("other"))),
            .bc = { 0 }
        },
        {
            .val = "merge @global := \"some string\"",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_MERGE(AST_UEXPR_I(UNARY_NONE, AST_IDENT("@global")), AST_UEXPR_V(UNARY_NONE, VM_STR("\"some string\""))),
            .bc = { 0 }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseReturnStatement(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "return",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_NULL())),
            .bc = { 0 }
        },
        {
            .val = "return null",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_NULL())),
            .bc = { 0 }
        },
        {
            .val = "return 1",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_INT(1))),
            .bc = { 0 }
        },
        {
            .val = "return \"a string\"",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_STR("\"a string\""))),
            .bc = { 0 }
        },
        {
            .val = "return name",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_UEXPR_I(UNARY_NONE, AST_IDENT("name"))),
            .bc = { 0 }
        },
        {
            .val = "return name.second",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_BEXPR_II(AST_IDENT("name"), BINARY_GETATTR, AST_IDENT("second"))),
            .bc = { 0 }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseFuncDeclaration(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "func Sum(a, b) { return a + b }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE_V(
                AST_IDENT("Sum"),
                AST_UEXPR_V(
                    UNARY_NONE,
                    VM_FUNC(
                        AST_IDENT("Sum"),
                        AST_ARGLIST(2, AST_IDENT("a"), AST_IDENT("b")),
                        AST_STMT_BLOCK(
                            1,
                            &AST_RETURN(AST_BEXPR_II(AST_IDENT("a"), BINARY_PLUS, AST_IDENT("b")))
                        )
                    )
                ),
                NULL
            ),
            .bc = { 0 }
        },
        {
            .val = "var Sum := func (a, b) { return a + b }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE_V(
                AST_IDENT("Sum"),
                AST_UEXPR_V(
                    UNARY_NONE,
                    VM_FUNC(
                        NULL,
                        AST_ARGLIST(2, AST_IDENT("a"), AST_IDENT("b")),
                        AST_STMT_BLOCK(
                            1,
                            &AST_RETURN(AST_BEXPR_II(AST_IDENT("a"), BINARY_PLUS, AST_IDENT("b")))
                        )
                    )
                ),
                NULL
            ),
            .bc = { 0 }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseDeclaration(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "var name",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE(AST_IDENT("name"), NULL),
            .bc = { 0 }
        },
        {
            .val = "var name := \"Gladys\"",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE_V(AST_IDENT("name"), AST_UEXPR_V(UNARY_NONE, VM_STR("\"Gladys\"")), NULL),
            .bc = { 0 }
        },
        {
            .val = "var name, second",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE(AST_IDENT("name"), &AST_DECL_CMPNT(AST_IDENT("second"), NULL)),
            .bc = { 0 }
        },
        {
            .val = "var name, second, third",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE(AST_IDENT("name"), &AST_DECL_CMPNT(AST_IDENT("second"), &AST_DECL_CMPNT(AST_IDENT("third"), NULL))),
            .bc = { 0 }
        },
        {
            .val = "var name, second := \"J\", third",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE(AST_IDENT("name"), &AST_DECL_CMPNT_V(AST_IDENT("second"), AST_UEXPR_V(UNARY_NONE, VM_STR("\"J\"")), &AST_DECL_CMPNT(AST_IDENT("third"), NULL))),
            .bc = { 0 }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

MunitResult prs_TestParseAssignment(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "name := 10",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN(AST_UEXPR_I(UNARY_NONE, AST_IDENT("name")), AST_UEXPR_V(UNARY_NONE, VM_INT(10))),
            .bc = { 0 }
        },
        {
            .val = "name.second := 10",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN(AST_BEXPR_II(AST_IDENT("name"), BINARY_GETATTR, AST_IDENT("second")), AST_UEXPR_V(UNARY_NONE, VM_INT(10))),
            .bc = { 0 }
        },
        {
            .val = "name.second += 10",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN(AST_BEXPR_II(AST_IDENT("name"), BINARY_GETATTR, AST_IDENT("second")), AST_BEXPR_EV(AST_BEXPR_II(AST_IDENT("name"), BINARY_GETATTR, AST_IDENT("second")), BINARY_PLUS, VM_INT(10))),
            .bc = { 0 }
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

/*
 * COMPARISON FUNCTIONS
 *
 * The functions below compare the abstract syntax tree components given
 * as test cases against the AST returned from the parser. The parse test
 * case tuple allows callers to specify different AST granularity (expression,
 * statement, or full module), so comparisons always need to start with
 * `CompareASTToComponent` since it will select the more granular element
 * from the full AST returned by the parser. Each other comparison function
 * will be called recursively as needed when processing the AST. Each
 * comparison function will bubble errors back up as they call various
 * assert functions from the unit test library.
 */

static MunitResult CompareASTToComponent(const ms_AST *ast, ASTComponentType type, ASTComponent *cmpnt) {
    munit_assert_non_null(ast);

    switch(type) {
        case ASTCMPNT_MODULE:
            CompareAST(ast, cmpnt->module);
            break;
        case ASTCMPNT_STMT: {
            munit_assert_cmp_int(dsarray_len(ast), ==, 1);
            const ms_Stmt *stmt = dsarray_get(ast, 0);
            CompareStatements(stmt, &cmpnt->stmt);
            break;
        }
        case ASTCMPNT_EXPR: {
            munit_assert_cmp_int(dsarray_len(ast), ==, 1);
            const ms_Stmt *stmt = dsarray_get(ast, 0);
            munit_assert_cmp_int(stmt->type, ==, STMTTYPE_EXPRESSION);
            const ms_Expr *expr = stmt->cmpnt.expr;
            CompareExpressions(expr, &cmpnt->expr);
            break;
        }
    }

    return MUNIT_OK;
}

static MunitResult CompareAST(const ms_AST *ast1, const ms_AST *ast2) {
    munit_assert_non_null(ast1);
    munit_assert_non_null(ast2);

    munit_assert_cmp_int(dsarray_len(ast1), ==, dsarray_len(ast2));
    size_t len = dsarray_len(ast1);
    for (size_t i = 0; i < len; i++) {
        const ms_Stmt *stmt1 = dsarray_get(ast1, i);
        const ms_Stmt *stmt2 = dsarray_get(ast2, i);
        CompareStatements(stmt1, stmt2);
    }

    return MUNIT_OK;
}

static MunitResult CompareStatements(const ms_Stmt *stmt1, const ms_Stmt *stmt2) {
    munit_assert_non_null(stmt1);
    munit_assert_non_null(stmt2);

    munit_assert_cmp_int(stmt1->type, ==, stmt2->type);
    switch(stmt1->type) {
        case STMTTYPE_EMPTY:        // Fall through
        case STMTTYPE_BREAK:        // Fall through
        case STMTTYPE_CONTINUE:
            break;
        case STMTTYPE_DELETE:
            CompareExpressions(stmt1->cmpnt.del->expr, stmt2->cmpnt.del->expr);
            break;
        case STMTTYPE_FOR:
            CompareForStatement(stmt1->cmpnt.forstmt, stmt2->cmpnt.forstmt);
            break;
        case STMTTYPE_IF:
            CompareExpressions(stmt1->cmpnt.ifstmt->expr, stmt2->cmpnt.ifstmt->expr);
            CompareBlocks(stmt1->cmpnt.ifstmt->block, stmt2->cmpnt.ifstmt->block);
            if ((stmt1->cmpnt.ifstmt->elif) || (stmt1->cmpnt.ifstmt->elif)) {
                CompareIfElseStatement(stmt1->cmpnt.ifstmt->elif, stmt2->cmpnt.ifstmt->elif);
            }
            break;
        case STMTTYPE_IMPORT:
            CompareExpressions(stmt1->cmpnt.import->ident, stmt2->cmpnt.import->ident);
            if ((stmt1->cmpnt.import->alias) || (stmt2->cmpnt.import->alias)) {
                CompareIdent(stmt1->cmpnt.import->alias, stmt2->cmpnt.import->alias);
            }
            break;
        case STMTTYPE_MERGE:
            CompareExpressions(stmt1->cmpnt.merge->left, stmt2->cmpnt.merge->left);
            CompareExpressions(stmt1->cmpnt.merge->right, stmt2->cmpnt.merge->right);
            break;
        case STMTTYPE_RETURN:
            CompareExpressions(stmt1->cmpnt.ret->expr, stmt2->cmpnt.ret->expr);
            break;
        case STMTTYPE_DECLARATION:
            CompareDeclarations(stmt1->cmpnt.decl, stmt2->cmpnt.decl);
            break;
        case STMTTYPE_ASSIGNMENT:
            CompareExpressions(stmt1->cmpnt.assign->ident, stmt2->cmpnt.assign->ident);
            CompareExpressions(stmt1->cmpnt.assign->expr, stmt2->cmpnt.assign->expr);
            break;
        case STMTTYPE_EXPRESSION:
            CompareExpressions(stmt1->cmpnt.expr, stmt2->cmpnt.expr);
            break;
    }

    return MUNIT_OK;
}

static MunitResult CompareArgumentList(const ms_ArgList *al1, const ms_ArgList *al2) {
    munit_assert_non_null(al1);
    munit_assert_non_null(al2);

    munit_assert_cmp_int(dsarray_len(al1), ==, dsarray_len(al2));
    size_t len = dsarray_len(al1);
    for (size_t i = 0; i < len; i++) {
        const ms_Ident *ident1 = dsarray_get(al1, i);
        const ms_Ident *ident2 = dsarray_get(al2, i);
        CompareIdent(ident1, ident2);
    }

    return MUNIT_OK;
}

static MunitResult CompareBlocks(const ms_StmtBlock *blk1, const ms_StmtBlock *blk2) {
    munit_assert_non_null(blk1);
    munit_assert_non_null(blk2);

    munit_assert_cmp_int(dsarray_len(blk1), ==, dsarray_len(blk2));
    size_t len = dsarray_len(blk1);
    for (size_t i = 0; i < len; i++) {
        const ms_Stmt *stmt1 = dsarray_get(blk1, i);
        const ms_Stmt *stmt2 = dsarray_get(blk2, i);
        CompareStatements(stmt1, stmt2);
    }

    return MUNIT_OK;
}

static MunitResult CompareForStatement(const ms_StmtFor *for1, const ms_StmtFor *for2) {
    munit_assert_non_null(for1);
    munit_assert_non_null(for2);

    munit_assert_cmp_int(for1->type, ==, for2->type);
    switch (for1->type) {
        case FORSTMT_ITERATOR:
            munit_assert(for1->clause.iter->declare == for2->clause.iter->declare);
            CompareExpressions(for1->clause.iter->ident, for2->clause.iter->ident);
            CompareExpressions(for1->clause.iter->iter, for2->clause.iter->iter);
            break;
        case FORSTMT_INCREMENT:
            munit_assert(for1->clause.inc->declare == for2->clause.inc->declare);
            CompareExpressions(for1->clause.inc->ident, for2->clause.inc->ident);
            CompareExpressions(for1->clause.inc->init, for2->clause.inc->init);
            CompareExpressions(for1->clause.inc->end, for2->clause.inc->end);
            CompareExpressions(for1->clause.inc->step, for2->clause.inc->step);
            break;
        case FORSTMT_EXPR:
            CompareExpressions(for1->clause.expr->expr, for2->clause.expr->expr);
            break;
    }

    CompareBlocks(for1->block, for2->block);

    return MUNIT_OK;
}

static MunitResult CompareIfElseStatement(const ms_StmtIfElse *elif1, const ms_StmtIfElse *elif2) {
    munit_assert_non_null(elif1);
    munit_assert_non_null(elif2);

    munit_assert_cmp_int(elif1->type, ==, elif2->type);
    switch (elif1->type) {
        case IFELSE_IF:
            CompareExpressions(elif1->clause.ifstmt->expr, elif2->clause.ifstmt->expr);
            CompareBlocks(elif1->clause.ifstmt->block, elif2->clause.ifstmt->block);
            if ((elif1->clause.ifstmt->elif) || (elif2->clause.ifstmt->elif)) {
                CompareIfElseStatement(elif1->clause.ifstmt->elif, elif2->clause.ifstmt->elif);
            }
            break;
        case IFELSE_ELSE:
            CompareBlocks(elif1->clause.elstmt->block, elif2->clause.elstmt->block);
            break;
    }

    return MUNIT_OK;
}

static MunitResult CompareDeclarations(const ms_StmtDeclaration *decl1, const ms_StmtDeclaration *decl2) {
    munit_assert_non_null(decl1);
    munit_assert_non_null(decl2);

    CompareIdent(decl1->ident, decl2->ident);
    if ((decl1->expr) || (decl2->expr)) {
        CompareExpressions(decl1->expr, decl2->expr);
    }
    if ((decl1->next) || (decl2->next)) {
        CompareDeclarations(decl1->next, decl2->next);
    }

    return MUNIT_OK;
}

static MunitResult CompareExpressions(const ms_Expr *expr1, const ms_Expr *expr2) {
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

static MunitResult CompareExpressionAtoms(ms_ExprAtomType type, const ms_ExprAtom *atom1, const ms_ExprAtom *atom2) {
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

static MunitResult CompareByteCode(const ms_VMByteCode *bc1, const ms_VMByteCode *bc2) {
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

static MunitResult CompareExpressionList(const ms_ExprList *el1, const ms_ExprList *el2) {
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

static MunitResult CompareIdent(const ms_Ident *id1, const ms_Ident *id2) {
    munit_assert_non_null(id1);
    munit_assert_non_null(id1);

    const char *s1 = dsbuf_char_ptr(id1);
    const char *s2 = dsbuf_char_ptr(id2);
    munit_logf(MUNIT_LOG_INFO, "  ident1='%s'", s1);
    munit_logf(MUNIT_LOG_INFO, "  ident2='%s'", s2);
    munit_assert_string_equal(s1, s2);

    return MUNIT_OK;
}

static MunitResult CompareValues(const ms_Value *val1, const ms_Value *val2) {
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
        case MSVAL_FUNC:
            CompareFunctions(val1->val.fn, val2->val.fn);
            break;
    }

    return MUNIT_OK;
}

static MunitResult CompareFunctions(const ms_ValFunc *fn1, const ms_ValFunc *fn2) {
    munit_assert_non_null(fn1);
    munit_assert_non_null(fn2);

    if ((fn1->ident) || (fn2->ident)) {
        CompareIdent(fn1->ident, fn2->ident);
    }
    CompareArgumentList(fn1->args, fn2->args);
    CompareBlocks(fn1->block, fn2->block);

    return MUNIT_OK;
}

static MunitResult TestParseResultTuple(ParseResultTuple *tuples, size_t len) {
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
        if (err) { munit_logf(MUNIT_LOG_INFO, "err = %s", err->msg); }

        munit_assert_cmp_int(pres, !=, PARSE_ERROR);
        //munit_assert_non_null(code);
        munit_assert_non_null(ast);
        munit_assert_null(err);

        //CompareByteCode(code, &tuple->bc);
        CompareASTToComponent(ast, tuple->type, &tuple->cmpnt);

        ms_VMByteCodeDestroy(code);
        CleanParseResultTuple(tuple);
    }

    ms_ParserDestroy(prs);
    return MUNIT_OK;
}

/*
 * CLEAN UP FUNCTIONS
 *
 * Most of the structures allocated for testing that the parser forms
 * proper ASTs from the input text are stack allocated. However, because
 * a few of the structures inside the actual AST are heap-allocated opaque
 * types (such as the DSArray array list and DSBuf string buffer), it is
 * necessary to have some cleanup functions to make sure we don't leak
 * massive amounts of memory while the tests are running.
 */

static void CleanAST(ms_AST *ast) {
    if (!ast) { return; }

    size_t len = dsarray_len(ast);
    for (size_t i = 0; i < len; i++) {
        ms_Stmt *stmt = dsarray_get(ast, i);
        CleanStatement(stmt);
    }

    dsarray_destroy(ast);
}

static void CleanStatement(ms_Stmt *stmt) {
    if (!stmt) { return; }

    switch(stmt->type) {
        case STMTTYPE_EMPTY:        // Fall through
        case STMTTYPE_BREAK:        // Fall through
        case STMTTYPE_CONTINUE:
            break;
        case STMTTYPE_DELETE:
            CleanExpression(stmt->cmpnt.del->expr);
            break;
        case STMTTYPE_FOR:
            CleanForStatement(stmt->cmpnt.forstmt);
            break;
        case STMTTYPE_IF:
            CleanExpression(stmt->cmpnt.ifstmt->expr);
            CleanBlock(stmt->cmpnt.ifstmt->block);
            CleanIfElseStatement(stmt->cmpnt.ifstmt->elif);
            break;
        case STMTTYPE_IMPORT:
            CleanExpression(stmt->cmpnt.import->ident);
            dsbuf_destroy(stmt->cmpnt.import->alias);
            break;
        case STMTTYPE_MERGE:
            CleanExpression(stmt->cmpnt.merge->left);
            CleanExpression(stmt->cmpnt.merge->right);
            break;
        case STMTTYPE_RETURN:
            CleanExpression(stmt->cmpnt.ret->expr);
            break;
        case STMTTYPE_DECLARATION:
            CleanDeclaration(stmt->cmpnt.decl);
            break;
        case STMTTYPE_ASSIGNMENT:
            CleanExpression(stmt->cmpnt.assign->ident);
            CleanExpression(stmt->cmpnt.assign->expr);
            break;
        case STMTTYPE_EXPRESSION:
            CleanExpression(stmt->cmpnt.expr);
            break;
    }
}

static void CleanBlock(ms_StmtBlock *block) {
    if (!block) { return; }

    size_t len = dsarray_len(block);
    for (size_t i = 0; i < len; i++) {
        ms_Stmt *stmt = dsarray_get(block, i);
        CleanStatement(stmt);
    }

    dsarray_destroy(block);
}

static void CleanForStatement(ms_StmtFor *forstmt) {
    if (!forstmt) { return; }

    switch (forstmt->type) {
        case FORSTMT_ITERATOR:
            CleanExpression(forstmt->clause.iter->ident);
            CleanExpression(forstmt->clause.iter->iter);
            break;
        case FORSTMT_INCREMENT:
            CleanExpression(forstmt->clause.inc->ident);
            CleanExpression(forstmt->clause.inc->init);
            CleanExpression(forstmt->clause.inc->end);
            CleanExpression(forstmt->clause.inc->step);
            break;
        case FORSTMT_EXPR:
            CleanExpression(forstmt->clause.expr->expr);
            break;
    }

    CleanBlock(forstmt->block);
    forstmt->block = NULL;
}

static void CleanIfElseStatement(ms_StmtIfElse *elif) {
    if (!elif) { return; }

    switch (elif->type) {
        case IFELSE_IF:
            CleanExpression(elif->clause.ifstmt->expr);
            CleanBlock(elif->clause.ifstmt->block);
            CleanIfElseStatement(elif->clause.ifstmt->elif);
            break;
        case IFELSE_ELSE:
            CleanBlock(elif->clause.elstmt->block);
            break;
    }
}

static void CleanDeclaration(ms_StmtDeclaration *decl) {
    if (!decl) { return; }
    dsbuf_destroy(decl->ident);
    CleanExpression(decl->expr);
    CleanDeclaration(decl->next);
}

static void CleanExpression(ms_Expr *expr) {
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

static void CleanExpressionAtom(ms_ExprAtomType type, ms_ExprAtom *atom) {
    switch(type) {
        case EXPRATOM_EXPRESSION:
            CleanExpression(atom->expr);
            atom->expr = NULL;
            break;
        case EXPRATOM_VALUE:
            if (atom->val.type == MSVAL_STR) {
                dsbuf_destroy(atom->val.val.s);
                atom->val.val.s = NULL;
            } else if (atom->val.type == MSVAL_FUNC) {
                CleanFunction(atom->val.val.fn);
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

static void CleanFunction(ms_ValFunc *func) {
    if (!func) { return; }

    dsbuf_destroy(func->ident);
    CleanBlock(func->block);

    size_t len = dsarray_len(func->args);
    for (size_t i = 0; i < len; i++) {
        ms_Ident *ident = dsarray_get(func->args, i);
        dsbuf_destroy(ident);
    }
    dsarray_destroy(func->args);
}

static void CleanExpressionList(ms_ExprList *el) {
    if (!el) { return; }

    size_t len = dsarray_len(el);
    for (size_t i = 0; i < len; i++) {
        ms_Expr *expr = dsarray_get(el, i);
        CleanExpression(expr);
    }
    dsarray_destroy(el);
}

static void CleanByteCode(ms_VMByteCode *bc) {
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

static void CleanParseResultTuple(ParseResultTuple *tuple) {
    if (!tuple) { return; }

    switch (tuple->type) {
        case ASTCMPNT_MODULE:
            CleanAST(tuple->cmpnt.module);
            tuple->cmpnt.module = NULL;
            break;
        case ASTCMPNT_STMT:
            CleanStatement(&tuple->cmpnt.stmt);
            break;
        case ASTCMPNT_EXPR:
            CleanExpression(&tuple->cmpnt.expr);
            break;
    }

    CleanByteCode(&tuple->bc);
}
