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
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <sys/errno.h>
#include "lang.h"
#include "vm.h"

static const int EXPR_OPCODE_STACK_LEN = 50;
static const int EXPR_VALUE_STACK_LEN = 50;

// Static table of operator precedence values
static const ms_ExprOpPrecedence OP_PRECEDENCE[] = {
    { OP_UMINUS, 110, ASSOC_RIGHT },
    { OP_TIMES, 100, ASSOC_LEFT },
    { OP_DIVIDE, 100, ASSOC_LEFT },
    { OP_IDIVIDE, 100, ASSOC_LEFT },
    { OP_EXPONENTIATE, 100, ASSOC_LEFT },
    { OP_MODULO, 100, ASSOC_LEFT },
    { OP_PLUS, 90, ASSOC_LEFT },
    { OP_MINUS, 90, ASSOC_LEFT },
    { OP_SHIFT_LEFT, 80, ASSOC_LEFT },
    { OP_SHIFT_RIGHT, 80, ASSOC_LEFT },
    { OP_BITWISE_AND, 75, ASSOC_LEFT },
    { OP_BITWISE_XOR, 70, ASSOC_LEFT },
    { OP_BITWISE_OR, 65, ASSOC_LEFT },
    { OP_LE, 60, ASSOC_LEFT },
    { OP_LT, 60, ASSOC_LEFT },
    { OP_GE, 60, ASSOC_LEFT },
    { OP_GT, 60, ASSOC_LEFT },
    { OP_EQ, 50, ASSOC_LEFT },
    { OP_NOT_EQ, 50, ASSOC_LEFT },
    { OP_AND, 40, ASSOC_LEFT },
    { OP_OR, 30, ASSOC_LEFT }
};

static void ExprToOpCodes(ms_Expr *expr, DSArray *opcodes, DSArray *values);
static void ExprComponentToOpCodes(ms_ExprAtom *a, ms_ExprAtomType type, DSArray *opcodes, DSArray *values);
static void ExprOpToOpCode(ms_Expr *expr, DSArray *opcodes, DSArray *values);

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

ms_Expr *ms_ExprNewWithVal(ms_VMPrimitiveType type, ms_VMPrimitive v) {
    ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
    if (!expr) {
        return NULL;
    }

    expr->expr.u->expr.val.type = type;
    expr->expr.u->expr.val.val = v;
    expr->expr.u->type = EXPRATOM_VALUE;
    expr->expr.u->op = UNARY_NONE;
    return expr;
}

ms_Expr *ms_ExprNumberFromString(const char *str) {
    assert(str);

    ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
    if (!expr) {
        return NULL;
    }

    errno = 0;
    ms_VMFloat f = strtod(str, NULL);
    if (errno != 0) {
        ms_ExprDestroy(expr);
        return NULL;
    }

    ms_VMInt i;
    if (ms_VMFloatIsInt(f, &i)) {
        expr->expr.u->expr.val.type = VMVAL_INT;
        expr->expr.u->expr.val.val.i = i;
    } else {
        expr->expr.u->expr.val.type = VMVAL_FLOAT;
        expr->expr.u->expr.val.val.f = f;
    }

    expr->expr.u->type = EXPRATOM_VALUE;
    expr->expr.u->op = UNARY_NONE;
    return expr;
}

ms_Expr *ms_ExprFlatten(ms_Expr *outer, ms_Expr *inner, ms_ExprLocation loc) {
    if ((!outer) || (!inner)) { return NULL; }

    bool should_flatten = (inner->type == EXPRTYPE_UNARY) &&
                          (inner->expr.u->op == UNARY_NONE);

    switch (loc) {
        case EXPRLOC_UNARY:
            assert(outer->type == EXPRTYPE_UNARY);
            if (!should_flatten) {
                outer->expr.u->expr.expr = inner;
                outer->expr.u->type = EXPRATOM_EXPRESSION;
            } else {
                outer->expr.u->expr = inner->expr.u->expr;
                outer->expr.u->type = inner->expr.u->type;
                ms_ExprDestroy(inner);
            }
            break;
        case EXPRLOC_LEFT:
            assert(outer->type == EXPRTYPE_BINARY);
            if (!should_flatten) {
                outer->expr.b->left.expr = inner;
                outer->expr.b->ltype = EXPRATOM_EXPRESSION;
            } else {
                outer->expr.b->left = inner->expr.u->expr;
                outer->expr.b->ltype = inner->expr.u->type;
                ms_ExprDestroy(inner);
            }
            break;
        case EXPRLOC_RIGHT:
            assert(outer->type == EXPRTYPE_BINARY);
            if (!should_flatten) {
                outer->expr.b->right.expr = inner;
                outer->expr.b->rtype = EXPRATOM_EXPRESSION;
            } else {
                outer->expr.b->right = inner->expr.u->expr;
                outer->expr.b->rtype = inner->expr.u->type;
                ms_ExprDestroy(inner);
            }
            break;
    }

    return outer;
}

ms_VMByteCode *ms_ExprToOpCodes(ms_Expr *expr) {
    if (!expr) { return NULL; }

    DSArray *opcodes = dsarray_new_cap(EXPR_OPCODE_STACK_LEN, NULL,
                                       (dsarray_free_fn)free);
    if (!opcodes) {
        return NULL;
    }

    DSArray *values = dsarray_new_cap(EXPR_VALUE_STACK_LEN, NULL,
                                      (dsarray_free_fn)free);
    if (!values) {
        dsarray_destroy(opcodes);
        return NULL;
    }

    ExprToOpCodes(expr, opcodes, values);
    ms_VMByteCode *bc = ms_VMByteCodeNew(opcodes, values);
    dsarray_destroy(opcodes);
    dsarray_destroy(values);
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
                expr->expr.u = NULL;
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
                expr->expr.b = NULL;
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
        case OP_SHIFT_LEFT:     return BINARY_SHIFT_LEFT;
        case OP_SHIFT_RIGHT:    return BINARY_SHIFT_RIGHT;
        case OP_BITWISE_AND:    return BINARY_BITWISE_AND;
        case OP_BITWISE_XOR:    return BINARY_BITWISE_XOR;
        case OP_BITWISE_OR:     return BINARY_BITWISE_OR;
        case OP_LE:             return BINARY_LE;
        case OP_LT:             return BINARY_LT;
        case OP_GE:             return BINARY_GE;
        case OP_GT:             return BINARY_GT;
        case OP_DOUBLE_EQ:      return BINARY_EQ;
        case OP_NOT_EQ:         return BINARY_NOT_EQ;
        case OP_AND:            return BINARY_AND;
        case OP_OR:             return BINARY_OR;
        default:                return BINARY_EMPTY;
    }
}

/*
 * PRIVATE FUNCTIONS
 */

static void ExprToOpCodes(ms_Expr *expr, DSArray *opcodes, DSArray *values) {
    assert(expr);
    assert(opcodes);
    assert(values);

    if (expr->type == EXPRTYPE_UNARY) {
        ExprComponentToOpCodes(&expr->expr.u->expr, expr->expr.u->type, opcodes, values);
        ExprOpToOpCode(expr, opcodes, values);
    } else {
        ExprComponentToOpCodes(&expr->expr.b->left, expr->expr.b->ltype, opcodes, values);
        ExprComponentToOpCodes(&expr->expr.b->right, expr->expr.b->rtype, opcodes, values);
        ExprOpToOpCode(expr, opcodes, values);
    }
}

static void ExprComponentToOpCodes(ms_ExprAtom *a, ms_ExprAtomType type, DSArray *opcodes, DSArray *values) {
    assert(a);
    assert(opcodes);
    assert(values);

    if (type == EXPRATOM_EXPRESSION) {
        ExprToOpCodes(a->expr, opcodes, values);
    } else if (type == EXPRATOM_VALUE) {
        ms_VMOpCode *o = malloc(sizeof(ms_VMOpCode));
        if (!o) { return; }
        ms_VMValue *v = malloc(sizeof(ms_VMValue));
        if (!v) { free(o); return; }
        *v = a->val;
        dsarray_append(values, v);
        size_t nvals = dsarray_len(values);
        assert(nvals <= INT_MAX);
        *o = ms_VMOpCodeWithArg(OPC_PUSH, (int)(nvals - 1));
        dsarray_append(opcodes, o);
    }
}

static void ExprOpToOpCode(ms_Expr *expr, DSArray *opcodes, DSArray *values) {
    assert(expr);
    assert(opcodes);
    assert(values);

    ms_VMOpCode *o = malloc(sizeof(ms_VMOpCode));
    if (!o) { return; }

    if (expr->type == EXPRTYPE_BINARY) {
        switch (expr->expr.b->op) {
            case BINARY_PLUS:
                *o = OPC_ADD;
                break;
            case BINARY_MINUS:
                *o = OPC_SUBTRACT;
                break;
            case BINARY_TIMES:
                *o = OPC_MULTIPLY;
                break;
            case BINARY_DIVIDE:
                *o = OPC_DIVIDE;
                break;
            case BINARY_IDIVIDE:
                *o = OPC_IDIVIDE;
                break;
            case BINARY_MODULO:
                *o = OPC_MODULO;
                break;
            case BINARY_EXPONENTIATE:
                *o = OPC_EXPONENTIATE;
                break;
            case BINARY_SHIFT_LEFT:
                *o = OPC_SHIFT_LEFT;
                break;
            case BINARY_SHIFT_RIGHT:
                *o = OPC_SHIFT_RIGHT;
                break;
            case BINARY_BITWISE_AND:
                *o = OPC_BITWISE_AND;
                break;
            case BINARY_BITWISE_XOR:
                *o = OPC_BITWISE_XOR;
                break;
            case BINARY_BITWISE_OR:
                *o = OPC_BITWISE_OR;
                break;
            case BINARY_LE:
                *o = OPC_LE;
                break;
            case BINARY_LT:
                *o = OPC_LT;
                break;
            case BINARY_GE:
                *o = OPC_GE;
                break;
            case BINARY_GT:
                *o = OPC_GT;
                break;
            case BINARY_EQ:
                *o = OPC_EQ;
                break;
            case BINARY_NOT_EQ:
                *o = OPC_NOT_EQ;
                break;
            case BINARY_AND:
                *o = OPC_AND;
                break;
            case BINARY_OR:
                *o = OPC_OR;
                break;
            default:
                free(o);
                return;
        }

        dsarray_append(opcodes, o);
    } else {
        if (expr->expr.u->op == UNARY_MINUS) {
            *o = OPC_NEGATE;
            dsarray_append(opcodes, o);
            return;
        }

        free(o);
    }
}
