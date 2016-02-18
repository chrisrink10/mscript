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

#include <assert.h>
#include <stdlib.h>
#include <sys/errno.h>
#include "lang.h"

// Static table of operator precedence values
static const ms_ExprOpPrecedence OP_PRECEDENCE[] = {
    { OP_TIMES, 100, ASSOC_LEFT },
    { OP_DIVIDE, 100, ASSOC_LEFT },
    { OP_IDIVIDE, 100, ASSOC_LEFT },
    { OP_EXPONENTIATE, 100, ASSOC_LEFT },
    { OP_MODULO, 100, ASSOC_LEFT },
    { OP_PLUS, 90, ASSOC_LEFT },
    { OP_MINUS, 90, ASSOC_LEFT },
};

/*
 * PUBLIC FUNCTIONS
 */

ms_Expr *ms_ExprNew(ms_ExprType type) {
    ms_Expr *expr = malloc(sizeof(ms_Expr));
    if (!expr) {
        return NULL;
    }

    expr->type = type;
    switch (type) {
        case EXPRTYPE_UNARY:
            expr->expr.u = malloc(sizeof(ms_ExprUnary));
            if (!expr->expr.u) {
                free(expr);
                return NULL;
            }
            expr->expr.u->expr.expr = NULL;
            expr->expr.u->op = UNARY_NONE;
            break;
        case EXPRTYPE_BINARY:
            expr->expr.b = malloc(sizeof(ms_ExprBinary));
            if (!expr->expr.b) {
                free(expr);
                return NULL;
            }
            expr->expr.b->left.expr = NULL;
            expr->expr.b->ltype = EXPRATOM_EMPTY;
            expr->expr.b->op = BINARY_EMPTY;
            expr->expr.b->rtype = EXPRATOM_EMPTY;
            expr->expr.b->right.expr = NULL;
            break;
    }

    return expr;
}

ms_Expr *ms_ExprNumberFromString(const char *str) {
    assert(str);

    ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
    if (!expr) {
        return NULL;
    }

    errno = 0;
    double val = strtod(str, NULL);
    if (errno != 0) {
        ms_ExprDestroy(expr);
        return NULL;
    }

    expr->expr.u->expr.num = val;
    expr->expr.u->type = EXPRATOM_NUMBER;
    expr->expr.u->op = UNARY_NONE;
    return expr;
}

void ms_ExprDestroy(ms_Expr *expr) {
    if (!expr) { return; }
    switch (expr->type) {
        case EXPRTYPE_UNARY:
            if (expr->expr.u) {
                if (expr->expr.u->type == EXPRATOM_EXPRESSION) {
                    ms_ExprDestroy(expr->expr.u->expr.expr);
                }
                free(expr->expr.u);
            }
            break;
        case EXPRTYPE_BINARY:
            if (expr->expr.b) {
                if (expr->expr.b->ltype == EXPRATOM_EXPRESSION) {
                    ms_ExprDestroy(expr->expr.b->left.expr);
                }
                if (expr->expr.b->rtype == EXPRATOM_EXPRESSION) {
                    ms_ExprDestroy(expr->expr.b->right.expr);
                }
                free(expr->expr.b);
            }
            break;
    }
    free(expr);
}

size_t ms_ExprOpPrecedenceTable(const ms_ExprOpPrecedence **tbl) {
    size_t len = sizeof(OP_PRECEDENCE) / sizeof(OP_PRECEDENCE[0]);
    *tbl = OP_PRECEDENCE;
    return len;
}

ms_ExprBinaryOp ms_ExprTokenToBinaryOp(ms_TokenType type) {
    switch (type) {
        case OP_PLUS:           return BINARY_PLUS;
        case OP_MINUS:          return BINARY_MINUS;
        case OP_TIMES:          return BINARY_TIMES;
        case OP_DIVIDE:         return BINARY_DIVIDE;
        case OP_IDIVIDE:        return BINARY_IDIVIDE;
        case OP_MODULO:         return BINARY_MODULO;
        case OP_EXPONENTIATE:   return BINARY_EXPONENTIATE;
        default:                return BINARY_EMPTY;
    }
}
