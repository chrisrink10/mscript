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

#include "parser_test.h"
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
} ParseResultTuple;

/*
 * TEST DEFINITIONS
 */

static MunitResult prs_TestParseErrors(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseLiterals(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseArrayLiterals(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseObjectLiterals(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseUnaryExprs(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseBinaryExprs(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseConditionalExprs(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseExprPrecedence(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseFunctionCalls(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseQualifiedIdents(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseDeleteStatement(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseForIncStatements(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseForIterStatements(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseForExprStatements(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseIfStatements(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseImportStatement(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseReturnStatement(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseFuncDeclaration(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseDeclaration(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseAssignment(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseMultipleAssignment(const MunitParameter params[], void *user_data);
static MunitResult prs_TestParseCompoundAssignment(const MunitParameter params[], void *user_data);

static char* bad_code[] = {
    "(;",
    "(3 ;",
    "(3 + ;",
    "(3 + 3;",
    "3 +;",
    "3 + 3);",
    "+ 3;",
    "+ 3);",
    "(+ 3;",
    "+3;",
    "3 ++ 3;",
    "3!;",
    "3!!;",
    "3-;",
    "3--;",
    "3~;",
    "3~~;",
    "-3-;",
    "~3~;",
    "!3!;",
    "3 5;",
    "true.;",
    "name.;",
    "name.second.;",
    "[;",
    "[,;",
    "[,];",
    "{;",
    "{,;",
    "{,};",
    "{\"key\",};",
    "{\"key\":,};",
    "{\"key\"};",
    "{\"key\":};",
    "select( ;",
    "select(name ;",
    "select(name);",
    "select(name:);",
    "select(name: value,);",
    "break \"string\";",
    "continue 3.14;",
    "del;",
    "del for;",
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
    "import;",
    "import 10;",
    "import Sys :;",
    "import Sys : true;",
    "return );",
    "return func;",
    "func { }",
    "var := 10;",
    "var := 10",
    "var = 10;",
    "var = 10",
    "var name = 10;",
    "var name = 10",
    "var name :=;",
    "var name :="
    "name :=;",
    "name :=",
    "name =;",
    "name =",
    "x +=;",
    "x +=",
    "x, y := 10;",
    "x, y := 10",
    "x, y := ;",
    "x, y :=",
    "x, y =;",
    "x, y =",
    "10 :=;",
    "10 := 12;",
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
        "/ArrayLiterals",
        prs_TestParseArrayLiterals,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/ObjectLiterals",
        prs_TestParseObjectLiterals,
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
        "/ConditionalExpressions",
        prs_TestParseConditionalExprs,
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
        "/QualifiedIdents",
        prs_TestParseQualifiedIdents,
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
    {
        "/MultipleAssignment",
        prs_TestParseMultipleAssignment,
        NULL,
        NULL,
        MUNIT_TEST_OPTION_NONE,
        NULL
    },
    {
        "/CompoundAssignment",
        prs_TestParseCompoundAssignment,
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
static MunitResult CompareAssignment(const ms_StmtAssignment *assign1, const ms_StmtAssignment *assign2);
static MunitResult CompareExpressions(const ms_Expr *expr1, const ms_Expr *expr2);
static MunitResult CompareExpressionAtoms(ms_ExprAtomType type, const ms_ExprAtom *atom1, const ms_ExprAtom *atom2);
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
static void CleanAssignment(ms_StmtAssignment *assign);
static void CleanExpression(ms_Expr *expr);
static void CleanExpressionAtom(ms_ExprAtomType type, ms_ExprAtom *atom);
static void CleanValue(ms_Value *val);
static void CleanArray(ms_ValArray *arr);
static void CleanObject(ms_ValObject *obj);
static void CleanFunction(ms_ValFunc *func);
static void CleanExpressionList(ms_ExprList *list);
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
#define AST_EXPRCOMPONENT_COND(catom, ctype, tatom, ttype, fatom, ftype) \
                                    AST_EXPRCOMPONENT(c, ((ms_ExprConditional){ .cond = catom, .condtype = ctype, .iftrue = tatom, .truetype = ttype, .iffalse = fatom, .falsetype = ftype}))
#define AST_EXPRATOM_EXPR(eatom)    ((ms_ExprAtom){ .expr = &eatom })
#define AST_EXPRATOM_VAL(vatom)     ((ms_ExprAtom){ .val = vatom })
#define AST_EXPRATOM_IDENT(iatom)   ((ms_ExprAtom){ .ident = iatom })
#define AST_EXPRATOM_LIST(latom)    ((ms_ExprAtom){ .list = latom })
#define AST_UNARY(atp, uop, v)      AST_EXPR(EXPRTYPE_UNARY, AST_EXPRCOMPONENT_UNARY(atp, uop, v))
#define AST_BINARY(latp, lv, op, ratp, rv) \
                                    AST_EXPR(EXPRTYPE_BINARY, AST_EXPRCOMPONENT_BINARY(lv, latp, op, rv, ratp))
#define AST_COND(ca, ctp, ta, ttp, fa, ftp) \
                                    AST_EXPR(EXPRTYPE_CONDITIONAL, AST_EXPRCOMPONENT_COND(ca, ctp, ta, ttp, fa, ftp))
#define AST_IDENT_NAME(v)           (dsbuf_new_l(v, sizeof(v)-1))

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
#define AST_CEXPR_VVV(v1, v2, v3)   AST_COND(AST_EXPRATOM_VAL(v1), EXPRATOM_VALUE, AST_EXPRATOM_VAL(v2), EXPRATOM_VALUE, AST_EXPRATOM_VAL(v3), EXPRATOM_VALUE)
#define AST_CEXPR_EEE(e1, e2, e3)   AST_COND(AST_EXPRATOM_EXPR(e1), EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(e2), EXPRATOM_EXPRESSION, AST_EXPRATOM_EXPR(e3), EXPRATOM_EXPRESSION)
#define AST_CEXPR_III(i1, i2, i3)   AST_COND(AST_EXPRATOM_IDENT(i1), EXPRATOM_IDENT, AST_EXPRATOM_IDENT(i2), EXPRATOM_IDENT, AST_EXPRATOM_IDENT(i3), EXPRATOM_IDENT)
#define AST_CEXPR_IVV(i1, v2, v3)   AST_COND(AST_EXPRATOM_IDENT(i1), EXPRATOM_IDENT, AST_EXPRATOM_VAL(v2), EXPRATOM_VALUE, AST_EXPRATOM_VAL(v3), EXPRATOM_VALUE)
#define AST_CEXPR_IIV(i1, i2, v3)   AST_COND(AST_EXPRATOM_IDENT(i1), EXPRATOM_IDENT, AST_EXPRATOM_IDENT(i2), EXPRATOM_IDENT, AST_EXPRATOM_VAL(v3), EXPRATOM_VALUE)
#define AST_CEXPR_EVV(e1, v2, v3)   AST_COND(AST_EXPRATOM_EXPR(e1), EXPRATOM_EXPRESSION, AST_EXPRATOM_VAL(v2), EXPRATOM_VALUE, AST_EXPRATOM_VAL(v3), EXPRATOM_VALUE)
#define AST_CEXPR_IVE(i1, v2, e3)   AST_COND(AST_EXPRATOM_IDENT(i1), EXPRATOM_IDENT, AST_EXPRATOM_VAL(v2), EXPRATOM_VALUE, AST_EXPRATOM_EXPR(e3), EXPRATOM_EXPRESSION)
#define AST_CEXPR_EVE(e1, v2, e3)   AST_COND(AST_EXPRATOM_EXPR(e1), EXPRATOM_EXPRESSION, AST_EXPRATOM_VAL(v2), EXPRATOM_VALUE, AST_EXPRATOM_EXPR(e3), EXPRATOM_EXPRESSION)
#define AST_IDENT(tp, v)            &((ms_Ident){ .type = tp, .name = AST_IDENT_NAME(v) }) /* subtract one to ignore the terminating NUL */
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
#define AST_RETURN(e)               AST_STMT(STMTTYPE_RETURN, AST_STMTCOMPONENT(ret, ((ms_StmtReturn){ .expr = &(e) })))
#define AST_ASSIGN_T(i, n)          ((ms_StmtAssignTarget){ .target = &(i), .next = &(n) })
#define AST_ASSIGN_T_SNG(i)         ((ms_StmtAssignTarget){ .target = &(i), .next = NULL })
#define AST_ASSIGN_E(e, n)          ((ms_StmtAssignExpr){ .expr = &(e), .next = &(n) })
#define AST_ASSIGN_E_SNG(e)         ((ms_StmtAssignExpr){ .expr = &(e), .next = NULL })
#define AST_ASSIGN(i, e)            AST_STMT(STMTTYPE_ASSIGNMENT, AST_STMTCOMPONENT(assign, ((ms_StmtAssignment){ .ident = &(i), .expr = &(e) })))
#define AST_ASSIGN_SNG(i, e)        AST_STMT(STMTTYPE_ASSIGNMENT, AST_STMTCOMPONENT(assign, ((ms_StmtAssignment){ .ident = &((ms_StmtAssignTarget){ .target = &(i), .next = NULL }), .expr = &((ms_StmtAssignExpr){ .expr = &(e), .next = NULL }) })))
#define AST_DECL_CMPNT(i, n)        ((ms_StmtDeclaration){ .ident = i, .expr = NULL, .next = n })
#define AST_DECL_CMPNT_V(i, e, n)   ((ms_StmtDeclaration){ .ident = i, .expr = &(e), .next = n })
#define AST_DECLARE(i, n)           AST_STMT(STMTTYPE_DECLARATION, AST_STMTCOMPONENT(decl, AST_DECL_CMPNT(i, n)))
#define AST_DECLARE_V(i, e, n)      AST_STMT(STMTTYPE_DECLARATION, AST_STMTCOMPONENT(decl, AST_DECL_CMPNT_V(i, e, n)))
#define AST_EXPR_STMT(e)            AST_STMT(STMTTYPE_EXPR, AST_STMTCOMPONENT(expr, ((ms_StmtExpression){ .expr = &(e) }))
#define AST_STMT_BLOCK(l, ...)      (dsarray_new_lit((void **)(((ms_Stmt*[]){ __VA_ARGS__ , })), l, l, NULL, NULL))
#define AST_EMPTY_STMT_BLOCK()      (dsarray_new_cap(1, NULL, NULL))

// VM type and bytecode macros
#define VM_OPC(opc, arg)            ms_VMOpCodeWithArg(opc, arg)
#define VM_FLOAT(v)                 ((ms_Value){ .type = MSVAL_FLOAT,   .val = (ms_ValData){ .f = v } })
#define VM_INT(v)                   ((ms_Value){ .type = MSVAL_INT,     .val = (ms_ValData){ .i = v } })
#define VM_STR(v)                   ((ms_Value){ .type = MSVAL_STR,     .val = (ms_ValData){ .s = dsbuf_new_l(v, sizeof(v)-1) } })
#define VM_BOOL(v)                  ((ms_Value){ .type = MSVAL_BOOL,    .val = (ms_ValData){ .b = v } })
#define VM_NULL()                   ((ms_Value){ .type = MSVAL_NULL,    .val = (ms_ValData){ .n = MS_VM_NULL_POINTER } })
#define VM_ARR(l, ...)              ((ms_Value){ .type = MSVAL_ARRAY,   .val = (ms_ValData){ .a = (dsarray_new_lit((void **)(((ms_Expr*[]){ __VA_ARGS__ , })), l, l, NULL, NULL)) } })
#define VM_EMPTY_ARR()              ((ms_Value){ .type = MSVAL_ARRAY,   .val = (ms_ValData){ .a = (dsarray_new_cap(1, NULL, NULL)) } })
#define VM_OBJ_TUPLE(k, v)          &((ms_ValObjectTuple){ .key = &(k), .val = &(v) })
#define VM_OBJ(l, ...)              ((ms_Value){ .type = MSVAL_OBJECT,  .val = (ms_ValData){ .o = (dsarray_new_lit((void **)(((ms_ValObjectTuple*[]){ __VA_ARGS__ , })), l, l, NULL, NULL)) } })
#define VM_EMPTY_OBJ()              ((ms_Value){ .type = MSVAL_OBJECT,  .val = (ms_ValData){ .o = (dsarray_new_cap(1, NULL, NULL)) } })
#define VM_FUNC(id, arglist, b)     ((ms_Value){ .type = MSVAL_FUNC,    .val = (ms_ValData){ .fn = &((ms_ValFunc){ .ident = id, .args = (arglist), .block = b }) } })
#define AST_ARGLIST(l, ...)         (dsarray_new_lit((void **)(((ms_Ident*[]){ __VA_ARGS__ , })), l, l, NULL, NULL))

/*
 * TEST CASE FUNCTIONS
 *
 * The functions below (mostly) contain test cases of abstract syntax tree
 * elements and expected bytecode. Each tests a sort of fuzzy subset of
 * language grammar elements.
 */

static MunitResult prs_TestParseErrors(const MunitParameter params[], void *user_data) {
    const char *code = munit_parameters_get(params, "code");
    ms_Parser *prs = ms_ParserNew();
    munit_assert_not_null(prs);

    munit_assert(ms_ParserInitString(prs, code));
    const ms_AST *ast;
    ms_Error *err;
    ms_Result pres = ms_ParserParse(prs, &ast, &err);

    munit_assert_int(pres, ==, MS_RESULT_ERROR);
    munit_assert_not_null(err);

    ms_ErrorDestroy(err);
    ms_ParserDestroy(prs);
    return MUNIT_OK;
}

static MunitResult prs_TestParseLiterals(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "0;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_INT(0)),
        },
        {
            .val = "3;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_INT(3)),
        },
        {
            .val = "0.0;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_FLOAT(0.0)),
        },
        {
            .val = "3.14;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_FLOAT(3.14)),
        },
        {
            .val = "true;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_BOOL(true)),
        },
        {
            .val = "false;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_BOOL(false)),
        },
        {
            .val = "null;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_NULL()),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseArrayLiterals(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "[];",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_EMPTY_ARR()),
        },
        {
            .val = "[1];",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_ARR(1, &AST_UEXPR_V(UNARY_NONE, VM_INT(1)))),
        },
        {
            .val = "[1,];",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_ARR(1, &AST_UEXPR_V(UNARY_NONE, VM_INT(1)))),
        },
        {
            .val = "[1 + 3];",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_ARR(1, &AST_BEXPR_VV(VM_INT(1), BINARY_PLUS, VM_INT(3)))),
        },
        {
            .val = "[1 + 3,];",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_ARR(1, &AST_BEXPR_VV(VM_INT(1), BINARY_PLUS, VM_INT(3)))),
        },
        {
            .val = "[1 + 3, \"shifty\"];",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_ARR(2, &AST_BEXPR_VV(VM_INT(1), BINARY_PLUS, VM_INT(3)), &AST_UEXPR_V(UNARY_NONE, VM_STR("shifty")))),
        },
        {
            .val = "[1 + 3, \"shifty\",];",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_ARR(2, &AST_BEXPR_VV(VM_INT(1), BINARY_PLUS, VM_INT(3)), &AST_UEXPR_V(UNARY_NONE, VM_STR("shifty")))),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseObjectLiterals(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "{};",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_EMPTY_OBJ()),
        },
        {
            .val = "{\"three\": 3};",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_OBJ(1, VM_OBJ_TUPLE(AST_UEXPR_V(UNARY_NONE, VM_STR("three")), AST_UEXPR_V(UNARY_NONE, VM_INT(3))))),
        },
        {
            .val = "{\"three\": 3,};",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_OBJ(1, VM_OBJ_TUPLE(AST_UEXPR_V(UNARY_NONE, VM_STR("three")), AST_UEXPR_V(UNARY_NONE, VM_INT(3))))),
        },
        {
            .val = "{\"three plus one\": 3 + 1};",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_OBJ(1, VM_OBJ_TUPLE(AST_UEXPR_V(UNARY_NONE, VM_STR("three plus one")), AST_BEXPR_VV(VM_INT(3), BINARY_PLUS, VM_INT(1))))),
        },
        {
            .val = "{\"three plus one\": 3 + 1,};",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE, VM_OBJ(1, VM_OBJ_TUPLE(AST_UEXPR_V(UNARY_NONE, VM_STR("three plus one")), AST_BEXPR_VV(VM_INT(3), BINARY_PLUS, VM_INT(1))))),
        },
        {
            .val = "{\"three plus one\": 3 + 1, 12 + 8: \"corduroy\"};",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE,
                                      VM_OBJ(2,
                                             VM_OBJ_TUPLE(AST_UEXPR_V(UNARY_NONE, VM_STR("three plus one")), AST_BEXPR_VV(VM_INT(3), BINARY_PLUS, VM_INT(1))),
                                             VM_OBJ_TUPLE(AST_BEXPR_VV(VM_INT(12), BINARY_PLUS, VM_INT(8)), AST_UEXPR_V(UNARY_NONE, VM_STR("corduroy"))))),
        },
        {
            .val = "{\"three plus one\": 3 + 1, 12 + 8: \"corduroy\",};",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NONE,
                                      VM_OBJ(2,
                                             VM_OBJ_TUPLE(AST_UEXPR_V(UNARY_NONE, VM_STR("three plus one")), AST_BEXPR_VV(VM_INT(3), BINARY_PLUS, VM_INT(1))),
                                             VM_OBJ_TUPLE(AST_BEXPR_VV(VM_INT(12), BINARY_PLUS, VM_INT(8)), AST_UEXPR_V(UNARY_NONE, VM_STR("corduroy"))))),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseUnaryExprs(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "-3;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_MINUS, VM_INT(3)),
        },
        {
            .val = "--2.72;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_E(UNARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_FLOAT(2.72))),
        },
        {
            .val = "!true;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_NOT, VM_BOOL(true)),
        },
        {
            .val = "!!false;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_E(UNARY_NOT, AST_UEXPR_V(UNARY_NOT, VM_BOOL(false))),
        },
        {
            .val = "~792;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(792)),
        },
        {
            .val = "~~42;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_E(UNARY_BITWISE_NOT, AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(42))),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseBinaryExprs(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "7 + 3.6;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(7), BINARY_PLUS, VM_FLOAT(3.6)),
        },
        {
            .val = "3 + ~3;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(3), BINARY_PLUS, AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(3))),
        },
        {
            .val = "16 - false;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(16), BINARY_MINUS, VM_BOOL(false)),
        },
        {
            .val = "3 - -3;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(3), BINARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_INT(3))),
        },
        {
            .val = "2 * 3.14;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(2), BINARY_TIMES, VM_FLOAT(3.14)),
        },
        {
            .val = "17 / 8.25;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(17), BINARY_DIVIDE, VM_FLOAT(8.25)),
        },
        {
            .val = "8.888 \\ 6;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_FLOAT(8.888), BINARY_IDIVIDE, VM_INT(6)),
        },
        {
            .val = "42 % 8;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(42), BINARY_MODULO, VM_INT(8)),
        },
        {
            .val = "5.25 ** 2;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_FLOAT(5.25), BINARY_EXPONENTIATE, VM_INT(2)),
        },
        {
            .val = "5 << 2;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(5), BINARY_SHIFT_LEFT, VM_INT(2)),
        },
        {
            .val = "183822 >> 4;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(183822), BINARY_SHIFT_RIGHT, VM_INT(4)),
        },
        {
            .val = "13 & 97;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(13), BINARY_BITWISE_AND, VM_INT(97)),
        },
        {
            .val = "3 | 15;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(3), BINARY_BITWISE_OR, VM_INT(15)),
        },
        {
            .val = "53 ^ 7;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(53), BINARY_BITWISE_XOR, VM_INT(7)),
        },
        {
            .val = "46.12 <= 73;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_FLOAT(46.12), BINARY_LE, VM_INT(73)),
        },
        {
            .val = "true < 14;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_BOOL(true), BINARY_LT, VM_INT(14)),
        },
        {
            .val = "33 != 1988;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(33), BINARY_NOT_EQ, VM_INT(1988)),
        },
        {
            .val = "71 == 0.33;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(71), BINARY_EQ, VM_FLOAT(0.33)),
        },
        {
            .val = "81.3 > 90;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_FLOAT(81.3), BINARY_GT, VM_INT(90)),
        },
        {
            .val = "1000 >= 10000;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(1000), BINARY_GE, VM_INT(10000)),
        },

        {
            .val = "true && false;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_BOOL(true), BINARY_AND, VM_BOOL(false)),
        },
        {
            .val = "true && !true;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_BOOL(true), BINARY_AND, AST_UEXPR_V(UNARY_NOT, VM_BOOL(true))),
        },
        {
            .val = "1 || null;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VV(VM_INT(1), BINARY_OR, VM_NULL()),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseConditionalExprs(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "(use_degrees) ? 360 : 2.0 * pi;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_CEXPR_IVE(AST_IDENT(IDENT_NAME, "use_degrees"), VM_INT(360), AST_BEXPR_VI(VM_FLOAT(2.0), BINARY_TIMES, AST_IDENT(IDENT_NAME, "pi"))),
        },
        {
            .val = "select(has_value: value);",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_CEXPR_IIV(AST_IDENT(IDENT_NAME, "has_value"), AST_IDENT(IDENT_NAME, "value"), VM_NULL()),
        },
        {
            .val = "select(has_value: value, 10);",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_CEXPR_IIV(AST_IDENT(IDENT_NAME, "has_value"), AST_IDENT(IDENT_NAME, "value"), VM_INT(10)),
        },
        {
            .val = "select(i >= 10: 10, i >= 5: 5, 0);",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_CEXPR_EVE(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "i"), BINARY_GE, VM_INT(10)), VM_INT(10), AST_CEXPR_EVV(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "i"), BINARY_GE, VM_INT(5)), VM_INT(5), VM_INT(0))),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseExprPrecedence(const MunitParameter params[], void *user_data) {

    ParseResultTuple exprs[] = {
        {
            .val = "false || false && true;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_BOOL(false), BINARY_OR, AST_BEXPR_VV(VM_BOOL(false), BINARY_AND, VM_BOOL(true))),
        },
        {
            .val = "(false || false) && true;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_BOOL(false), BINARY_OR, VM_BOOL(false)), BINARY_AND, VM_BOOL(true)),
        },
        {
            .val = "(false || !false) && true;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VE(VM_BOOL(false), BINARY_OR, AST_UEXPR_V(UNARY_NOT, VM_BOOL(false))), BINARY_AND, VM_BOOL(true)),
        },
        {
            .val = "false == false != true;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_BOOL(false), BINARY_EQ, VM_BOOL(false)), BINARY_NOT_EQ, VM_BOOL(true)),
        },
        {
            .val = "false == (false != true);",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_BOOL(false), BINARY_EQ, AST_BEXPR_VV(VM_BOOL(false), BINARY_NOT_EQ, VM_BOOL(true))),
        },
        {
            .val = "7 > 3.6 < 8;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_GT, VM_FLOAT(3.6)), BINARY_LT, VM_INT(8)),
        },
        {
            .val = "7 > (3.6 < 8);",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_GT, AST_BEXPR_VV(VM_FLOAT(3.6), BINARY_GT, VM_INT(8))),
        },
        {
            .val = "7 > 3.6 >= 8;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_GT, VM_FLOAT(3.6)), BINARY_GE, VM_INT(8)),
        },
        {
            .val = "7 | 2 & 15;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_BITWISE_OR, AST_BEXPR_VV(VM_INT(2), BINARY_BITWISE_AND, VM_INT(15))),
        },
        {
            .val = "7 ^ 2 & 15;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_BITWISE_XOR, AST_BEXPR_VV(VM_INT(2), BINARY_BITWISE_AND, VM_INT(15))),
        },
        {
            .val = "7 ^ 2 | 15;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_XOR, VM_INT(2)), BINARY_BITWISE_OR, VM_INT(15)),
        },
        {
            .val = "7 ^ (2 | 15);",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_BITWISE_XOR, AST_BEXPR_VV(VM_INT(2), BINARY_BITWISE_OR, VM_INT(15))),
        },
        {
            .val = "(7 | 2) & 15;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_OR, VM_INT(2)), BINARY_BITWISE_AND, VM_INT(15)),
        },
        {
            .val = "(7 | 2) & ~15;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_OR, VM_INT(2)), BINARY_BITWISE_AND, AST_UEXPR_V(UNARY_BITWISE_NOT, VM_INT(15))),
        },
        {
            .val = "~(7 | 2) & 15;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_UEXPR_E(UNARY_BITWISE_NOT, AST_BEXPR_VV(VM_INT(7), BINARY_BITWISE_OR, VM_INT(2))), BINARY_BITWISE_AND, VM_INT(15)),
        },
        {
            .val = "15 | 1 << 2;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(15), BINARY_BITWISE_OR, AST_BEXPR_VV(VM_INT(1), BINARY_SHIFT_LEFT, VM_INT(2))),
        },
        {
            .val = "(15 | 1) << 2;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(15), BINARY_BITWISE_OR, VM_INT(1)), BINARY_SHIFT_LEFT, VM_INT(2)),
        },
        {
            .val = "7 + 3.6 - 8;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_PLUS, VM_FLOAT(3.6)), BINARY_MINUS, VM_INT(8)),
        },
        {
            .val = "7 + (3.6 - 8);",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_PLUS, AST_BEXPR_VV(VM_FLOAT(3.6), BINARY_MINUS, VM_INT(8))),
        },
        {
            .val = "7 + (3.6 - -8);",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_PLUS, AST_BEXPR_VE(VM_FLOAT(3.6), BINARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_INT(8)))),
        },
        {
            .val = "(7 + 3.6) - -8;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_PLUS, VM_FLOAT(3.6)), BINARY_MINUS, AST_UEXPR_V(UNARY_MINUS, VM_INT(8))),
        },
        {
            .val = "7 + 3.6 * -8;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_PLUS, AST_BEXPR_VE(VM_FLOAT(3.6), BINARY_TIMES, AST_UEXPR_V(UNARY_MINUS, VM_INT(8)))),
        },
        {
            .val = "7 / 3.6 * -8;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_DIVIDE, VM_FLOAT(3.6)), BINARY_TIMES, AST_UEXPR_V(UNARY_MINUS, VM_INT(8))),
        },
        {
            .val = "7 \\ 3.6 % -8;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EE(AST_BEXPR_VV(VM_INT(7), BINARY_IDIVIDE, VM_FLOAT(3.6)), BINARY_MODULO, AST_UEXPR_V(UNARY_MINUS, VM_INT(8))),
        },
        {
            .val = "7 ** 4 ** 2;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_EXPONENTIATE, AST_BEXPR_VV(VM_INT(4), BINARY_EXPONENTIATE, VM_INT(2))),
        },
        {
            .val = "(7 ** 4) ** 2;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_EXPONENTIATE, VM_INT(4)), BINARY_EXPONENTIATE, VM_INT(2)),
        },
        {
            .val = "7 ** 4 * 2;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_EXPONENTIATE, AST_BEXPR_VV(VM_INT(4), BINARY_TIMES, VM_INT(2))),
        },
        {
            .val = "7 ** 4 + 2;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_VV(VM_INT(7), BINARY_EXPONENTIATE, VM_INT(4)), BINARY_PLUS, VM_INT(2)),
        },
        {
            .val = "7 ** (4 + 2);",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_VE(VM_INT(7), BINARY_EXPONENTIATE, AST_BEXPR_VV(VM_INT(4), BINARY_PLUS, VM_INT(2))),
        },
    };
    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseFunctionCalls(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "$len();",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_I(AST_IDENT(IDENT_BUILTIN, "$len"), AST_EMPTY_EXPRLIST()),
        },
        {
            .val = "foo();",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_I(AST_IDENT(IDENT_NAME, "foo"), AST_EMPTY_EXPRLIST()),
        },
        {
            .val = "$len(\"string\");",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_I(AST_IDENT(IDENT_BUILTIN, "$len"), AST_EXPRLIST(1, &AST_UEXPR_V(UNARY_NONE, VM_STR("string")))),
        },
        {
            .val = "foo(\"string\");",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_I(AST_IDENT(IDENT_NAME, "foo"), AST_EXPRLIST(1, &AST_UEXPR_V(UNARY_NONE, VM_STR("string")))),
        },
        {
            .val = "foo()();",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_E(AST_FNCALL_I(AST_IDENT(IDENT_NAME, "foo"), AST_EMPTY_EXPRLIST()), AST_EMPTY_EXPRLIST()),
        },
        {
            .val = "bar(\"baz\")();",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_FNCALL_E(AST_FNCALL_I(AST_IDENT(IDENT_NAME, "bar"), AST_EXPRLIST(1, &AST_UEXPR_V(UNARY_NONE, VM_STR("baz")))), AST_EMPTY_EXPRLIST()),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseQualifiedIdents(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "name;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")),
        },
        {
            .val = "name.first;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("first")),
        },
        {
            .val = "name?.first;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_SAFEGETATTR, VM_STR("first")),
        },
        {
            .val = "name[\"first\"];",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("first")),
        },
        {
            .val = "name?[\"first\"];",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_SAFEGETATTR, VM_STR("first")),
        },
        {
            .val = "name?[\"first\", \"second\"];",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_SAFEGETATTR, VM_STR("first")), BINARY_SAFEGETATTR, VM_STR("second")),
        },
        {
            .val = "name.first.second.third;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_EV(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("first")), BINARY_GETATTR, VM_STR("second")), BINARY_GETATTR, VM_STR("third")),
        },
        {
            .val = "name?.first?.second?.third;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_EV(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_SAFEGETATTR, VM_STR("first")), BINARY_SAFEGETATTR, VM_STR("second")), BINARY_SAFEGETATTR, VM_STR("third")),
        },
        {
            .val = "name[\"first\", \"second\"]?.third;",
            .type = ASTCMPNT_EXPR,
            .cmpnt.expr = AST_BEXPR_EV(AST_BEXPR_EV(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("first")), BINARY_GETATTR, VM_STR("second")), BINARY_SAFEGETATTR, VM_STR("third")),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseDeleteStatement(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "del @global;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DEL(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_GLOBAL, "@global"))),
        },
        {
            .val = "del local;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DEL(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "local")))
        },
        {
            .val = "del name.second.third;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DEL(AST_BEXPR_EV(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("second")), BINARY_GETATTR, VM_STR("third")))
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseForIncStatements(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "for var i := 0 : lim { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_INC_1(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "i")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(0)),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "lim")),
                true,
                AST_EMPTY_STMT_BLOCK()),
        },
        {
            .val = "for var i := 0 : lim : 2 { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_INC(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "i")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(0)),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "lim")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(2)),
                true,
                AST_EMPTY_STMT_BLOCK()),
        },
        {
            .val = "for i := 0 : lim { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_INC_1(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "i")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(0)),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "lim")),
                false,
                AST_EMPTY_STMT_BLOCK()),
        },
        {
            .val = "for i := 0 : lim : 2 { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_INC(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "i")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(0)),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "lim")),
                AST_UEXPR_V(UNARY_NONE, VM_INT(2)),
                false,
                AST_EMPTY_STMT_BLOCK()),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseForIterStatements(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "for var i in range { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_ITER(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "i")),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "range")),
                true,
                AST_EMPTY_STMT_BLOCK()),
        },
        {
            .val = "for i in range { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_ITER(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "i")),
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "range")),
                false,
                AST_EMPTY_STMT_BLOCK()),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseForExprStatements(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "for cond { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_EXPR(
                AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "cond")),
                AST_EMPTY_STMT_BLOCK()),
        },
        {
            .val = "for true { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_EXPR(
                AST_UEXPR_V(UNARY_NONE, VM_BOOL(true)),
                AST_EMPTY_STMT_BLOCK()),
        },
        {
            .val = "for 1 { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_EXPR(
                AST_UEXPR_V(UNARY_NONE, VM_INT(1)),
                AST_EMPTY_STMT_BLOCK()),
        },
        {
            .val = "for arr.drained() { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_FOR_EXPR(
                AST_FNCALL_E(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "arr"), BINARY_GETATTR, VM_STR("drained")), AST_EMPTY_EXPRLIST()),
                AST_EMPTY_STMT_BLOCK()),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseIfStatements(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "if cost >= money { }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IF_ELIF(AST_BEXPR_II(AST_IDENT(IDENT_NAME, "cost"), BINARY_GE, AST_IDENT(IDENT_NAME, "money")), AST_EMPTY_STMT_BLOCK(), NULL),
        },
        {
            .val = "if cost >= money { \n"
                   "    // do nothing\n"
                   "} else { \n"
                   "    // also do nothing\n"
                   "}",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IF_ELIF(
                AST_BEXPR_II(AST_IDENT(IDENT_NAME, "cost"), BINARY_GE, AST_IDENT(IDENT_NAME, "money")),
                AST_EMPTY_STMT_BLOCK(),
                AST_ELIF_ELSE(
                    AST_EMPTY_STMT_BLOCK()
                )
            ),
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
                AST_BEXPR_II(AST_IDENT(IDENT_NAME, "cost"), BINARY_GE, AST_IDENT(IDENT_NAME, "money")),
                AST_EMPTY_STMT_BLOCK(),
                AST_ELIF_IF(
                    AST_BEXPR_II(AST_IDENT(IDENT_NAME, "cost"), BINARY_EQ, AST_IDENT(IDENT_NAME, "money")),
                    AST_EMPTY_STMT_BLOCK(),
                    AST_ELIF_ELSE(
                        AST_EMPTY_STMT_BLOCK()
                    )
                )
            ),
        },
        {
            .val = "if pct >= 0.9 {\n"
                   "    return \"A\";\n"
                   "} else if pct >= 0.8 { \n"
                   "    return \"B\";\n"
                   "} else if pct >= 0.7 { \n"
                   "    return \"C\";\n"
                   "} else {\n"
                   "    return \"F\";\n"
                   "}",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IF_ELIF(
                AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "pct"), BINARY_GE, VM_FLOAT(0.9)),
                AST_STMT_BLOCK(1, &AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_STR("A")))),
                AST_ELIF_IF(
                    AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "pct"), BINARY_GE, VM_FLOAT(0.8)),
                    AST_STMT_BLOCK(1, &AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_STR("B")))),
                    AST_ELIF_IF(
                        AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "pct"), BINARY_GE, VM_FLOAT(0.7)),
                        AST_STMT_BLOCK(1, &AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_STR("C")))),
                        AST_ELIF_ELSE(
                            AST_STMT_BLOCK(1, &AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_STR("F"))))
                        )
                    )
                )
            ),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseImportStatement(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "import Sys;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IMPORT(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "Sys")), NULL),
        },
        {
            .val = "import Http.Server;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IMPORT(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "Http"), BINARY_GETATTR, VM_STR("Server")), NULL),
        },
        {
            .val = "import Http.Server : srv;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_IMPORT(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "Http"), BINARY_GETATTR, VM_STR("Server")), AST_IDENT(IDENT_NAME, "srv")),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseReturnStatement(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "return;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_NULL())),
        },
        {
            .val = "return null;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_NULL())),
        },
        {
            .val = "return 1;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_INT(1))),
        },
        {
            .val = "return \"a string\";",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_UEXPR_V(UNARY_NONE, VM_STR("a string"))),
        },
        {
            .val = "return name;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name"))),
        },
        {
            .val = "return name.second;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_RETURN(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("second"))),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseFuncDeclaration(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "func Sum(a, b) { return a + b; }",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE_V(
                AST_IDENT(IDENT_NAME, "Sum"),
                AST_UEXPR_V(
                    UNARY_NONE,
                    VM_FUNC(
                        AST_IDENT(IDENT_NAME, "Sum"),
                        AST_ARGLIST(2, AST_IDENT(IDENT_NAME, "a"), AST_IDENT(IDENT_NAME, "b")),
                        AST_STMT_BLOCK(
                            1,
                            &AST_RETURN(AST_BEXPR_II(AST_IDENT(IDENT_NAME, "a"), BINARY_PLUS, AST_IDENT(IDENT_NAME, "b")))
                        )
                    )
                ),
                NULL
            ),
        },
        {
            .val = "var Sum := func (a, b) { return a + b; };",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE_V(
                AST_IDENT(IDENT_NAME, "Sum"),
                AST_UEXPR_V(
                    UNARY_NONE,
                    VM_FUNC(
                        NULL,
                        AST_ARGLIST(2, AST_IDENT(IDENT_NAME, "a"), AST_IDENT(IDENT_NAME, "b")),
                        AST_STMT_BLOCK(
                            1,
                            &AST_RETURN(AST_BEXPR_II(AST_IDENT(IDENT_NAME, "a"), BINARY_PLUS, AST_IDENT(IDENT_NAME, "b")))
                        )
                    )
                ),
                NULL
            ),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseDeclaration(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "var name;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE(AST_IDENT(IDENT_NAME, "name"), NULL),
        },
        {
            .val = "var name := \"Gladys\";",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE_V(AST_IDENT(IDENT_NAME, "name"), AST_UEXPR_V(UNARY_NONE, VM_STR("Gladys")), NULL),
        },
        {
            .val = "var name, second;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE(AST_IDENT(IDENT_NAME, "name"), &AST_DECL_CMPNT(AST_IDENT(IDENT_NAME, "second"), NULL)),
        },
        {
            .val = "var name, second, third;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE(AST_IDENT(IDENT_NAME, "name"), &AST_DECL_CMPNT(AST_IDENT(IDENT_NAME, "second"), &AST_DECL_CMPNT(AST_IDENT(IDENT_NAME, "third"), NULL))),
        },
        {
            .val = "var name, second := \"J\", third;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_DECLARE(AST_IDENT(IDENT_NAME, "name"), &AST_DECL_CMPNT_V(AST_IDENT(IDENT_NAME, "second"), AST_UEXPR_V(UNARY_NONE, VM_STR("J")), &AST_DECL_CMPNT(AST_IDENT(IDENT_NAME, "third"), NULL))),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseAssignment(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "name := 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_UEXPR_V(UNARY_NONE, VM_INT(10))),
        },
        {
            .val = "name.second := 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("second")), AST_UEXPR_V(UNARY_NONE, VM_INT(10))),
        },
        {
            .val = "name.second.third := 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_BEXPR_EV(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("second")), BINARY_GETATTR, VM_STR("third")), AST_UEXPR_V(UNARY_NONE, VM_INT(10))),
        },
        {
            .val = "name[\"second\"] := 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("second")), AST_UEXPR_V(UNARY_NONE, VM_INT(10))),
        },
        {
            .val = "name[\"second\", \"third\"] := 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_BEXPR_EV(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("second")), BINARY_GETATTR, VM_STR("third")), AST_UEXPR_V(UNARY_NONE, VM_INT(10))),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseMultipleAssignment(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "x, y := 0, 1;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN(
                AST_ASSIGN_T(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "x")), AST_ASSIGN_T_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "y")))),
                AST_ASSIGN_E(AST_UEXPR_V(UNARY_NONE, VM_INT(0)), AST_ASSIGN_E_SNG(AST_UEXPR_V(UNARY_NONE, VM_INT(1))))
            ),
        },
        {
            .val = "x, y, z := 0, 1, 0;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN(
                AST_ASSIGN_T(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "x")), AST_ASSIGN_T(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "y")), AST_ASSIGN_T_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "z"))))),
                AST_ASSIGN_E(AST_UEXPR_V(UNARY_NONE, VM_INT(0)), AST_ASSIGN_E(AST_UEXPR_V(UNARY_NONE, VM_INT(1)), AST_ASSIGN_E_SNG(AST_UEXPR_V(UNARY_NONE, VM_INT(0)))))
            ),
        },
        {
            .val = "x, y := y, x;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN(
                AST_ASSIGN_T(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "x")), AST_ASSIGN_T_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "y")))),
                AST_ASSIGN_E(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "y")), AST_ASSIGN_E_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "x"))))
            ),
        },
        {
            .val = "x.attr, y.attr := y.attr, 1;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN(
                AST_ASSIGN_T(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "x"), BINARY_GETATTR, VM_STR("attr")), AST_ASSIGN_T_SNG(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "y"), BINARY_GETATTR, VM_STR("attr")))),
                AST_ASSIGN_E(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "x"), BINARY_GETATTR, VM_STR("attr")), AST_ASSIGN_E_SNG(AST_UEXPR_V(UNARY_NONE, VM_INT(1))))
            ),
        },
    };

    size_t len = sizeof(exprs) / sizeof(exprs[0]);
    TestParseResultTuple(exprs, len);
    return MUNIT_OK;
}

static MunitResult prs_TestParseCompoundAssignment(const MunitParameter params[], void *user_data) {
    ParseResultTuple exprs[] = {
        {
            .val = "name.second += 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("second")), AST_BEXPR_EV(AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_GETATTR, VM_STR("second")), BINARY_PLUS, VM_INT(10))),
        },
        {
            .val = "name += 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_PLUS, VM_INT(10))),
        },
        {
            .val = "name -= 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_MINUS, VM_INT(10))),
        },
        {
            .val = "name *= 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_TIMES, VM_INT(10))),
        },
        {
            .val = "name /= 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_DIVIDE, VM_INT(10))),
        },
        {
            .val = "name \\= 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_IDIVIDE, VM_INT(10))),
        },
        {
            .val = "name %= 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_MODULO, VM_INT(10))),
        },
        {
            .val = "name &= 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_BITWISE_AND, VM_INT(10))),
        },
        {
            .val = "name |= 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_BITWISE_OR, VM_INT(10))),
        },
        {
            .val = "name ^= 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_BITWISE_XOR, VM_INT(10))),
        },
        {
            .val = "name <<= 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_SHIFT_LEFT, VM_INT(10))),
        },
        {
            .val = "name >>= 10;",
            .type = ASTCMPNT_STMT,
            .cmpnt.stmt = AST_ASSIGN_SNG(AST_UEXPR_I(UNARY_NONE, AST_IDENT(IDENT_NAME, "name")), AST_BEXPR_IV(AST_IDENT(IDENT_NAME, "name"), BINARY_SHIFT_RIGHT, VM_INT(10))),
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
    munit_assert_not_null(ast);

    switch(type) {
        case ASTCMPNT_MODULE:
            CompareAST(ast, cmpnt->module);
            break;
        case ASTCMPNT_STMT: {
            munit_assert_int(dsarray_len(ast), ==, 1);
            const ms_Stmt *stmt = dsarray_get(ast, 0);
            CompareStatements(stmt, &cmpnt->stmt);
            break;
        }
        case ASTCMPNT_EXPR: {
            munit_assert_int(dsarray_len(ast), ==, 1);
            const ms_Stmt *stmt = dsarray_get(ast, 0);
            munit_assert_int(stmt->type, ==, STMTTYPE_EXPRESSION);
            const ms_Expr *expr = stmt->cmpnt.expr;
            CompareExpressions(expr, &cmpnt->expr);
            break;
        }
    }

    return MUNIT_OK;
}

static MunitResult CompareAST(const ms_AST *ast1, const ms_AST *ast2) {
    munit_assert_not_null(ast1);
    munit_assert_not_null(ast2);

    munit_assert_int(dsarray_len(ast1), ==, dsarray_len(ast2));
    size_t len = dsarray_len(ast1);
    for (size_t i = 0; i < len; i++) {
        const ms_Stmt *stmt1 = dsarray_get(ast1, i);
        const ms_Stmt *stmt2 = dsarray_get(ast2, i);
        CompareStatements(stmt1, stmt2);
    }

    return MUNIT_OK;
}

static MunitResult CompareStatements(const ms_Stmt *stmt1, const ms_Stmt *stmt2) {
    munit_assert_not_null(stmt1);
    munit_assert_not_null(stmt2);

    munit_assert_int(stmt1->type, ==, stmt2->type);
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
        case STMTTYPE_RETURN:
            CompareExpressions(stmt1->cmpnt.ret->expr, stmt2->cmpnt.ret->expr);
            break;
        case STMTTYPE_DECLARATION:
            CompareDeclarations(stmt1->cmpnt.decl, stmt2->cmpnt.decl);
            break;
        case STMTTYPE_ASSIGNMENT:
            CompareAssignment(stmt1->cmpnt.assign, stmt2->cmpnt.assign);
            break;
        case STMTTYPE_EXPRESSION:
            CompareExpressions(stmt1->cmpnt.expr, stmt2->cmpnt.expr);
            break;
    }

    return MUNIT_OK;
}

static MunitResult CompareArgumentList(const ms_ArgList *al1, const ms_ArgList *al2) {
    munit_assert_not_null(al1);
    munit_assert_not_null(al2);

    munit_assert_int(dsarray_len(al1), ==, dsarray_len(al2));
    size_t len = dsarray_len(al1);
    for (size_t i = 0; i < len; i++) {
        const ms_Ident *ident1 = dsarray_get(al1, i);
        const ms_Ident *ident2 = dsarray_get(al2, i);
        CompareIdent(ident1, ident2);
    }

    return MUNIT_OK;
}

static MunitResult CompareBlocks(const ms_StmtBlock *blk1, const ms_StmtBlock *blk2) {
    munit_assert_not_null(blk1);
    munit_assert_not_null(blk2);

    munit_assert_int(dsarray_len(blk1), ==, dsarray_len(blk2));
    size_t len = dsarray_len(blk1);
    for (size_t i = 0; i < len; i++) {
        const ms_Stmt *stmt1 = dsarray_get(blk1, i);
        const ms_Stmt *stmt2 = dsarray_get(blk2, i);
        CompareStatements(stmt1, stmt2);
    }

    return MUNIT_OK;
}

static MunitResult CompareForStatement(const ms_StmtFor *for1, const ms_StmtFor *for2) {
    munit_assert_not_null(for1);
    munit_assert_not_null(for2);

    munit_assert_int(for1->type, ==, for2->type);
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
    munit_assert_not_null(elif1);
    munit_assert_not_null(elif2);

    munit_assert_int(elif1->type, ==, elif2->type);
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
    munit_assert_not_null(decl1);
    munit_assert_not_null(decl2);

    CompareIdent(decl1->ident, decl2->ident);
    if ((decl1->expr) || (decl2->expr)) {
        CompareExpressions(decl1->expr, decl2->expr);
    }
    if ((decl1->next) || (decl2->next)) {
        CompareDeclarations(decl1->next, decl2->next);
    }

    return MUNIT_OK;
}

static MunitResult CompareAssignment(const ms_StmtAssignment *assign1, const ms_StmtAssignment *assign2) {
    munit_assert_not_null(assign1);
    munit_assert_not_null(assign2);

    ms_StmtAssignTarget *ident1 = assign1->ident;
    ms_StmtAssignTarget *ident2 = assign2->ident;
    while ((ident1) && (ident2)) {
        CompareExpressions(ident1->target, ident2->target);
        ident1 = ident1->next;
        ident2 = ident2->next;
    }
    munit_assert_null(ident1);
    munit_assert_null(ident2);

    ms_StmtAssignExpr *expr1 = assign1->expr;
    ms_StmtAssignExpr *expr2 = assign1->expr;
    while ((expr1) && (expr2)) {
        CompareExpressions(expr1->expr, expr2->expr);
        expr1 = expr1->next;
        expr2 = expr2->next;
    }
    munit_assert_null(expr1);
    munit_assert_null(expr2);

    return MUNIT_OK;
}

static MunitResult CompareExpressions(const ms_Expr *expr1, const ms_Expr *expr2) {
    munit_assert_not_null(expr1);
    munit_assert_not_null(expr2);

    munit_assert_int(expr1->type, ==, expr2->type);
    switch(expr1->type) {
        case EXPRTYPE_UNARY:
            munit_assert_int(expr1->cmpnt.u->type, ==, expr2->cmpnt.u->type);
            munit_assert_int(expr1->cmpnt.u->op, ==, expr1->cmpnt.u->op);
            CompareExpressionAtoms(expr1->cmpnt.u->type, &expr1->cmpnt.u->atom, &expr2->cmpnt.u->atom);
            break;
        case EXPRTYPE_BINARY:
            munit_assert_int(expr1->cmpnt.b->ltype, ==, expr2->cmpnt.b->ltype);
            CompareExpressionAtoms(expr1->cmpnt.b->ltype, &expr1->cmpnt.b->latom, &expr2->cmpnt.b->latom);
            munit_assert_int(expr1->cmpnt.b->op, ==, expr1->cmpnt.b->op);
            munit_assert_int(expr1->cmpnt.b->rtype, ==, expr2->cmpnt.b->rtype);
            CompareExpressionAtoms(expr1->cmpnt.b->rtype, &expr1->cmpnt.b->ratom, &expr2->cmpnt.b->ratom);
            break;
        case EXPRTYPE_CONDITIONAL:
            munit_assert_int(expr1->cmpnt.c->condtype, ==, expr2->cmpnt.c->condtype);
            CompareExpressionAtoms(expr1->cmpnt.c->condtype, &expr1->cmpnt.c->cond, &expr2->cmpnt.c->cond);
            munit_assert_int(expr1->cmpnt.c->truetype, ==, expr2->cmpnt.c->truetype);
            CompareExpressionAtoms(expr1->cmpnt.c->truetype, &expr1->cmpnt.c->iftrue, &expr2->cmpnt.c->iftrue);
            munit_assert_int(expr1->cmpnt.c->falsetype, ==, expr2->cmpnt.c->falsetype);
            CompareExpressionAtoms(expr1->cmpnt.c->falsetype, &expr1->cmpnt.c->iffalse, &expr2->cmpnt.c->iffalse);
            break;

    }

    return MUNIT_OK;
}

static MunitResult CompareExpressionAtoms(ms_ExprAtomType type, const ms_ExprAtom *atom1, const ms_ExprAtom *atom2) {
    munit_assert_not_null(atom1);
    munit_assert_not_null(atom2);

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

static MunitResult CompareExpressionList(const ms_ExprList *el1, const ms_ExprList *el2) {
    munit_assert_not_null(el1);
    munit_assert_not_null(el2);

    munit_assert_int(dsarray_len(el1), ==, dsarray_len(el2));
    size_t len = dsarray_len(el1);
    for (size_t i = 0; i < len; i++) {
        const ms_Expr *expr1 = dsarray_get(el1, i);
        const ms_Expr *expr2 = dsarray_get(el2, i);
        CompareExpressions(expr1, expr2);
    }

    return MUNIT_OK;
}

static MunitResult CompareIdent(const ms_Ident *id1, const ms_Ident *id2) {
    munit_assert_not_null(id1);
    munit_assert_not_null(id1);

    munit_assert_int(id1->type, ==, id2->type);
    const char *s1 = dsbuf_char_ptr(id1->name);
    const char *s2 = dsbuf_char_ptr(id2->name);
    munit_logf(MUNIT_LOG_INFO, "  ident1='%s'", s1);
    munit_logf(MUNIT_LOG_INFO, "  ident2='%s'", s2);
    munit_assert_string_equal(s1, s2);

    return MUNIT_OK;
}

static MunitResult CompareValues(const ms_Value *val1, const ms_Value *val2) {
    munit_assert_not_null(val1);
    munit_assert_not_null(val2);

    munit_assert_int(val1->type, ==, val2->type);
    switch (val1->type) {
        case MSVAL_FLOAT:
            munit_assert_double(val1->val.f, ==, val2->val.f);
            break;
        case MSVAL_INT:
            munit_assert_int(val1->val.i, ==, val2->val.i);
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
        case MSVAL_ARRAY: {
            size_t len1 = dsarray_len(val1->val.a);
            size_t len2 = dsarray_len(val2->val.a);
            munit_assert_size(len1, ==, len2);
            for(size_t i = 0; i < len1; i++) {
                ms_Expr *expr1 = dsarray_get(val1->val.a, i);
                ms_Expr *expr2 = dsarray_get(val2->val.a, i);
                CompareExpressions(expr1, expr2);
            }
            break;
        }
        case MSVAL_OBJECT: {
            size_t len1 = dsarray_len(val1->val.o);
            size_t len2 = dsarray_len(val2->val.o);
            munit_assert_size(len1, ==, len2);
            for(size_t i = 0; i < len1; i++) {
                ms_ValObjectTuple *tuple1 = dsarray_get(val1->val.o, i);
                ms_ValObjectTuple *tuple2 = dsarray_get(val2->val.o, i);
                munit_assert_not_null(tuple1);
                munit_assert_not_null(tuple2);
                CompareExpressions(tuple1->key, tuple2->key);
                CompareExpressions(tuple1->val, tuple2->val);
            }
            break;
        }
        case MSVAL_FUNC:
            CompareFunctions(val1->val.fn, val2->val.fn);
            break;
    }

    return MUNIT_OK;
}

static MunitResult CompareFunctions(const ms_ValFunc *fn1, const ms_ValFunc *fn2) {
    munit_assert_not_null(fn1);
    munit_assert_not_null(fn2);

    if ((fn1->ident) || (fn2->ident)) {
        CompareIdent(fn1->ident, fn2->ident);
    }
    CompareArgumentList(fn1->args, fn2->args);
    CompareBlocks(fn1->block, fn2->block);

    return MUNIT_OK;
}

static MunitResult TestParseResultTuple(ParseResultTuple *tuples, size_t len) {
    ms_Parser *prs = ms_ParserNew();
    munit_assert_not_null(prs);

    for (size_t i = 0; i < len; i++) {
        ParseResultTuple *tuple = &tuples[i];
        ms_ParserInitString(prs, tuple->val);
        munit_logf(MUNIT_LOG_INFO, "  code='%s'", tuple->val);

        const ms_AST *ast;
        ms_Error *err;
        ms_Result pres = ms_ParserParse(prs, &ast, &err);
        if (err) {
            munit_logf(MUNIT_LOG_INFO, "err = %s", err->msg);
        }

        munit_assert_int(pres, !=, MS_RESULT_ERROR);
        munit_assert_not_null(ast);
        munit_assert_null(err);

        CompareASTToComponent(ast, tuple->type, &tuple->cmpnt);
        CleanParseResultTuple(tuple);
        ms_ErrorDestroy(err);
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
            if (stmt->cmpnt.import->alias) {
                dsbuf_destroy(stmt->cmpnt.import->alias->name);
                stmt->cmpnt.import->alias->name = NULL;
            }
            break;
        case STMTTYPE_RETURN:
            CleanExpression(stmt->cmpnt.ret->expr);
            break;
        case STMTTYPE_DECLARATION:
            CleanDeclaration(stmt->cmpnt.decl);
            break;
        case STMTTYPE_ASSIGNMENT:
            CleanAssignment(stmt->cmpnt.assign);
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
    dsbuf_destroy(decl->ident->name);
    decl->ident->name = NULL;
    CleanExpression(decl->expr);
    CleanDeclaration(decl->next);
}

static void CleanAssignment(ms_StmtAssignment *assign) {
    if (!assign) { return; }

    ms_StmtAssignTarget *ident = assign->ident;
    while (ident) {
        CleanExpression(ident->target);
        ident = ident->next;
    }

    ms_StmtAssignExpr *expr = assign->expr;
    while (expr) {
        CleanExpression(expr->expr);
        expr = expr->next;
    }
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
        case EXPRTYPE_CONDITIONAL:
            CleanExpressionAtom(expr->cmpnt.c->condtype, &expr->cmpnt.c->cond);
            CleanExpressionAtom(expr->cmpnt.c->truetype, &expr->cmpnt.c->iftrue);
            CleanExpressionAtom(expr->cmpnt.c->falsetype, &expr->cmpnt.c->iffalse);
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
            CleanValue(&atom->val);
            break;
        case EXPRATOM_IDENT:
            dsbuf_destroy(atom->ident->name);
            atom->ident->name = NULL;
            break;
        case EXPRATOM_EXPRLIST:
            CleanExpressionList(atom->list);
            atom->list = NULL;
            break;
        case EXPRATOM_EMPTY:
            break;
    }
}

static void CleanValue(ms_Value *val) {
    switch(val->type) {
        case MSVAL_FLOAT:   /* fall through */
        case MSVAL_INT:     /* fall through */
        case MSVAL_BOOL:    /* fall through */
        case MSVAL_NULL:
            break;
        case MSVAL_STR:
            dsbuf_destroy(val->val.s);
            val->val.s = NULL;
            break;
        case MSVAL_ARRAY:
            CleanArray(val->val.a);
            val->val.a = NULL;
            break;
        case MSVAL_OBJECT:
            CleanObject(val->val.o);
            val->val.o = NULL;
            break;
        case MSVAL_FUNC:
            CleanFunction(val->val.fn);
            /* no need to NULL out the function struct since it is
             * statically allocated */
            break;
    }
}

static void CleanArray(ms_ValArray *arr) {
    if (!arr) { return; }

    size_t len = dsarray_len(arr);
    for (size_t i = 0; i < len; i++) {
        ms_Expr *expr = dsarray_get(arr, i);
        CleanExpression(expr);
    }
    dsarray_destroy(arr);
}

static void CleanObject(ms_ValObject *obj) {
    if (!obj) { return; }

    size_t len = dsarray_len(obj);
    for (size_t i = 0; i < len; i++) {
        ms_ValObjectTuple *tuple = dsarray_get(obj, i);
        CleanExpression(tuple->key);
        CleanExpression(tuple->val);
    }
    dsarray_destroy(obj);
}

static void CleanFunction(ms_ValFunc *func) {
    if (!func) { return; }

    if (func->ident) {
        dsbuf_destroy(func->ident->name);
        func->ident->name = NULL;
    }

    CleanBlock(func->block);

    size_t len = dsarray_len(func->args);
    for (size_t i = 0; i < len; i++) {
        ms_Ident *ident = dsarray_get(func->args, i);
        dsbuf_destroy(ident->name);
        ident->name = NULL;
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
}
