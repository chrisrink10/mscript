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

void ExprToOpCodes(ms_Expr *expr, DSArray *stack);
void ExprComponentToOpCodes(ms_ExprAtom *a, ms_ExprAtomType type, DSArray *stack);
void ExprOpToOpCode(ms_Expr *expr, DSArray *stack);

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

ms_VMByteCode *ms_ExprToOpCodes(ms_Expr *expr) {
    if (!expr) { return 0; }

    DSArray *stack = dsarray_new_cap(50, NULL, NULL);
    if (!stack) {
        return 0;
    }

    ExprToOpCodes(expr, stack);
    ms_VMByteCode *bc = ms_VMOpCodesToByteCode(stack);
    dsarray_destroy(stack);
    return bc;
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

/*
 * PRIVATE FUNCTIONS
 */

void ExprToOpCodes(ms_Expr *expr, DSArray *stack) {
    assert(expr);
    assert(stack);

    if (expr->type == EXPRTYPE_UNARY) {
        ExprComponentToOpCodes(&expr->expr.u->expr, expr->expr.u->type, stack);
    } else {
        ExprComponentToOpCodes(&expr->expr.b->left, expr->expr.b->ltype, stack);
        ExprComponentToOpCodes(&expr->expr.b->right, expr->expr.b->rtype, stack);
        ExprOpToOpCode(expr, stack);
    }
}

void ExprComponentToOpCodes(ms_ExprAtom *a, ms_ExprAtomType type, DSArray *stack) {
    assert(a);
    assert(stack);

    if (type == EXPRATOM_EXPRESSION) {
        ExprToOpCodes(a->expr, stack);
    } else if (type == EXPRATOM_NUMBER) {
        ms_VMOpCode *o = malloc(sizeof(ms_VMOpCode));
        if (!o) {
            return;
        }
        o->type = OPC_PUSH;
        o->arg = a->num;
        dsarray_append(stack, o);
    }
}

void ExprOpToOpCode(ms_Expr *expr, DSArray *stack) {
    assert(expr);
    assert(stack);

    ms_VMOpCode *o = malloc(sizeof(ms_VMOpCode));
    if (!o) {
        return;
    }

    if (expr->type == EXPRTYPE_BINARY) {
        switch (expr->expr.b->op) {
            case BINARY_EMPTY:
                return;
            case BINARY_PLUS:
                o->type = OPC_ADD;
                break;
            case BINARY_MINUS:
                o->type = OPC_SUBTRACT;
                break;
            case BINARY_TIMES:
                o->type = OPC_MULTIPLY;
                break;
            case BINARY_DIVIDE:
                o->type = OPC_DIVIDE;
                break;
            case BINARY_IDIVIDE:
                o->type = OPC_IDIVIDE;
                break;
            case BINARY_MODULO:
                o->type = OPC_MODULO;
                break;
            case BINARY_EXPONENTIATE:
                o->type = OPC_EXPONENTIATE;
                break;
        }

        dsarray_append(stack, o);
    }
}
