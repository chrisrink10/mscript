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

#ifndef MSCRIPT_LANG_H
#define MSCRIPT_LANG_H

#include "libds/array.h"
#include "libds/buffer.h"
#include "lexer.h"

typedef struct ms_Expr ms_Expr;

/*
 * MISCELLANEOUS LANGUAGE COMPONENTS
 */

typedef DSBuffer ms_Ident;
typedef DSArray ms_ExprList;

/*
 * VALUE LANGUAGE COMPONENTS
 */

typedef double ms_ValFloat;
typedef long long ms_ValInt;
typedef DSBuffer ms_ValStr;
typedef bool ms_ValBool;
typedef const void ms_ValNull;

typedef enum {
    MSVAL_FLOAT,
    MSVAL_INT,
    MSVAL_STR,
    MSVAL_BOOL,
    MSVAL_NULL,
} ms_ValDataType;

typedef union {
    ms_ValFloat f;
    ms_ValInt i;
    ms_ValStr *s;
    ms_ValBool b;
    ms_ValNull *n;
} ms_ValData;

typedef struct {
    ms_ValDataType type;
    ms_ValData val;
} ms_Value;

/*
 * EXPRESSION LANGUAGE COMPONENTS
 */

typedef union {
    ms_Expr *expr;
    ms_Value val;
    ms_Ident *ident;
    ms_ExprList *list;
} ms_ExprAtom;

typedef enum {
    EXPRATOM_EMPTY,
    EXPRATOM_EXPRESSION,
    EXPRATOM_VALUE,
    EXPRATOM_IDENT,
    EXPRATOM_EXPRLIST,
} ms_ExprAtomType;

typedef enum {
    UNARY_NONE,
    UNARY_MINUS,
    UNARY_NOT,
    UNARY_BITWISE_NOT,
} ms_ExprUnaryOp;

typedef struct {
    ms_ExprAtom atom;
    ms_ExprAtomType type;
    ms_ExprUnaryOp op;
} ms_ExprUnary;

typedef enum {
    BINARY_EMPTY,
    BINARY_PLUS,
    BINARY_MINUS,
    BINARY_TIMES,
    BINARY_DIVIDE,
    BINARY_IDIVIDE,
    BINARY_MODULO,
    BINARY_EXPONENTIATE,
    BINARY_SHIFT_LEFT,
    BINARY_SHIFT_RIGHT,
    BINARY_BITWISE_AND,
    BINARY_BITWISE_XOR,
    BINARY_BITWISE_OR,
    BINARY_LE,
    BINARY_LT,
    BINARY_GE,
    BINARY_GT,
    BINARY_EQ,
    BINARY_NOT_EQ,
    BINARY_AND,
    BINARY_OR,
    BINARY_CALL,
    BINARY_GETATTR,
} ms_ExprBinaryOp;

typedef struct {
    ms_ExprAtom latom;
    ms_ExprAtomType ltype;
    ms_ExprBinaryOp op;
    ms_ExprAtom ratom;
    ms_ExprAtomType rtype;
} ms_ExprBinary;

typedef union {
    ms_ExprBinary *b;
    ms_ExprUnary *u;
} ms_ExprComponent;

typedef enum {
    EXPRTYPE_BINARY,
    EXPRTYPE_UNARY
} ms_ExprType;

struct ms_Expr {
    ms_ExprComponent cmpnt;
    ms_ExprType type;
};

/* Enumeration used to indicate which part of an expression to flatten
 * into the outer/containing expression. */
typedef enum {
    EXPRLOC_UNARY,
    EXPRLOC_LEFT,
    EXPRLOC_RIGHT,
} ms_ExprLocation;

/*
 * STATEMENT LANGUAGE COMPONENTS
 */

typedef DSArray ms_StmtBlock;

typedef struct ms_StmtBreak ms_StmtBreak;           /* dummy types required to be declared */
typedef struct ms_StmtContinue ms_StmtContinue;     /* as pointers (which will have to be NULL) */
typedef struct ms_StmtIf ms_StmtIf;
typedef struct ms_StmtElse ms_StmtElse;

typedef union {
    ms_StmtIf *ifstmt;
    ms_StmtElse *elstmt;
} ms_StmtIfElseClause;

typedef enum {
    IFELSE_IF,
    IFELSE_ELSE,
} ms_StmtIfElseType;

typedef struct {
    ms_StmtIfElseClause clause;
    ms_StmtIfElseType type;
} ms_StmtIfElse;

struct ms_StmtIf {
    ms_Expr *expr;
    ms_StmtBlock *block;
    ms_StmtIfElse *elif;
};

struct ms_StmtElse {
    ms_StmtBlock *block;
};

typedef struct {
    ms_Expr *expr;
} ms_StmtReturn;

typedef struct {
    ms_Expr *ident;
    ms_Expr *expr;
} ms_StmtAssignment;

typedef struct {
    ms_Ident *ident;
    ms_Expr *expr;
} ms_StmtDeclaration;

typedef enum {
    STMTTYPE_EMPTY,
    STMTTYPE_BREAK,
    STMTTYPE_CONTINUE,
    STMTTYPE_IF,
    STMTTYPE_RETURN,
    STMTTYPE_ASSIGNMENT,
    STMTTYPE_DECLARATION,
    STMTTYPE_EXPRESSION,
} ms_StmtType;

typedef union {
    ms_StmtBreak *brk;
    ms_StmtContinue *cont;
    ms_StmtIf *ifstmt;
    ms_StmtReturn *ret;
    ms_StmtAssignment *assign;
    ms_StmtDeclaration *decl;
    ms_Expr *expr;
} ms_StmtComponent;

typedef struct {
    ms_StmtComponent cmpnt;
    ms_StmtType type;
} ms_Stmt;

/*
 * ABSTRACT SYNTAX TREE ROOT
 */

typedef ms_Stmt ms_AST;

/**
* @brief Create a new @c ms_Expr object.
*/
ms_Expr *ms_ExprNew(ms_ExprType type);

/**
* @brief Create a new @c ms_Expr object with a primitive value.
*/
ms_Expr *ms_ExprNewWithVal(ms_ValDataType type, ms_ValData v);

/**
* @brief Create a new @c ms_Expr object for an identifier.
*/
ms_Expr *ms_ExprNewWithIdent(const char *name, size_t len);

/**
* @brief Create a new @c ms_Expr object for containing a list of expressions.
*/
ms_Expr *ms_ExprNewWithList(ms_ExprList *list);

/**
* @brief Create a new unary @c ms_Expr object containing a floating point
* number from a string.
*/
ms_Expr *ms_ExprFloatFromString(const char *str);

/**
* @brief Create a new unary @c ms_Expr object containing an integer
* number from a string.
*/
ms_Expr *ms_ExprIntFromString(const char *str);

/**
* @brief Flatten two expressions such that the expression tree does not
* become too deep too quickly.
*
* This function WILL FREE @c inner if it is no longer needed (i.e. if the
* expression is flattened). Be careful to NULL out any remaining pointers
* you have to @c inner after calling this function.
*
* @param outer the outer/containing @c ms_Expr object
* @param inner the inner/contained @c ms_Expr object; this memory may be
*        freed if the inner expression is superfluous
* @param loc the location to flatten the inner expression in the outer
*        expression
* @returns the outer expression
*/
ms_Expr *ms_ExprFlatten(ms_Expr *outer, ms_Expr *inner, ms_ExprLocation loc);

/**
* @brief Destroy the given @c ms_Expr and any nested expressions.
*/
void ms_ExprDestroy(ms_Expr *expr);

/**
* @brief Destroy the given @c ms_Stmt and nested AST elements.
*/
void ms_StmtDestroy(ms_Stmt *stmt);

/*
* @brief Placeholder for real AST destroy function.
*/
#define ms_ASTDestroy(ast) ms_StmtDestroy(ast)

#endif //MSCRIPT_LANG_H
