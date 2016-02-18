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
#include "lexer.h"

/**
* @brief Operator associativity enumeration
*/
typedef enum ms_ExprOpAssociativity {
    ASSOC_LEFT,
    ASSOC_RIGHT
} ms_ExprOpAssociativity;

/**
* @brief Tuple structure representing the type and associativity of
* mscript operators
*/
typedef struct ms_ExprOpPrecedence {
    ms_TokenType type;
    int precedence;
    ms_ExprOpAssociativity assoc;
} ms_ExprOpPrecedence;

/**
* @brief Expression syntax tree object
*/
typedef struct ms_Expr ms_Expr;

/**
* @brief Expression atom union
*/
typedef union {
    ms_Expr *expr;
    double num;
} ms_ExprAtom;

/**
* @brief Type of expression atom
*/
typedef enum {
    EXPRATOM_EMPTY,
    EXPRATOM_EXPRESSION,
    EXPRATOM_NUMBER,
} ms_ExprAtomType;

/**
* @brief Type of operation for this binary expression
*/
typedef enum {
    UNARY_NONE,
    UNARY_PLUS,
    UNARY_MINUS,
} ms_ExprUnaryOp;

/*
* @brief Unary expression object
*/
typedef struct {
    ms_ExprAtom expr;
    ms_ExprAtomType type;
    ms_ExprUnaryOp op;
} ms_ExprUnary;

/**
* @brief Type of operation for this binary expression
*/
typedef enum {
    BINARY_EMPTY,
    BINARY_PLUS,
    BINARY_MINUS,
    BINARY_TIMES,
    BINARY_DIVIDE,
    BINARY_IDIVIDE,
    BINARY_MODULO,
    BINARY_EXPONENTIATE,
} ms_ExprBinaryOp;

/**
* @brief Binary expression object
*/
typedef struct {
    ms_ExprAtom left;
    ms_ExprAtomType ltype;
    ms_ExprBinaryOp op;
    ms_ExprAtom right;
    ms_ExprAtomType rtype;
} ms_ExprBinary;

/*
* @brief Union of binary or unary expression
*/
typedef union {
    ms_ExprBinary *b;
    ms_ExprUnary *u;
} ms_ExprComponent;

/*
* @brief Type of expression (binary or unary)
*/
typedef enum {
    EXPRTYPE_BINARY,
    EXPRTYPE_UNARY
} ms_ExprType;

/**
* @brief Expression object
*/
struct ms_Expr {
    ms_ExprComponent expr;
    ms_ExprType type;
};

/**
* @brief Create a new @c ms_Expr object.
*/
ms_Expr *ms_ExprNew(ms_ExprType type);

/**
* @brief Create a new @c ms_Expr object containing from a string.
*/
ms_Expr *ms_ExprNumberFromString(const char *str);

/**
* @brief Destroy the given @c ms_Expr and any nested expressions.
*/
void ms_ExprDestroy(ms_Expr *expr);

/**
* @brief Fill a pointer to the operator precedence table for callers.
*/
size_t ms_ExprOpPrecedenceTable(const ms_ExprOpPrecedence **tbl);

/*
* @brief Convert a token type into a binary operation enumeration.
*/
ms_ExprBinaryOp ms_ExprTokenToBinaryOp(ms_TokenType type);

#endif //MSCRIPT_LANG_H
