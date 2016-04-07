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
#include "bytecode.h"
#include "vm.h"

static const int EXPR_OPCODE_STACK_LEN = 50;
static const int EXPR_VALUE_STACK_LEN = 50;
static const int EXPR_IDENT_STACK_LEN = 50;

static ms_VMByteCode *VMByteCodeNew(const DSArray *opcodes, const DSArray *values, const DSArray *idents);
static void ExprToOpCodes(ms_Expr *expr, DSArray *opcodes, DSArray *values, DSArray *idents);
static void ExprComponentToOpCodes(ms_ExprAtom *a, ms_ExprAtomType type, DSArray *opcodes, DSArray *values, DSArray *idents);
static void ExprOpToOpCode(ms_Expr *expr, DSArray *opcodes);

/*
 * PUBLIC FUNCTIONS
 */

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

    /* no dsarray_free_fn required since we pass the pointer to the bytecode */
    DSArray *idents = dsarray_new_cap(EXPR_IDENT_STACK_LEN, NULL, NULL);
    if (!idents) {
        dsarray_destroy(values);
        dsarray_destroy(opcodes);
        return NULL;
    }

    ExprToOpCodes(expr, opcodes, values, idents);
    ms_VMByteCode *bc = VMByteCodeNew(opcodes, values, idents);
    dsarray_destroy(opcodes);
    dsarray_destroy(values);
    dsarray_destroy(idents);
    return bc;
}

void ms_VMByteCodeDestroy(ms_VMByteCode *bc) {
    if (!bc) { return; }
    free(bc->code);
    bc->code = NULL;
    free(bc->values);
    bc->values = NULL;
    for (size_t i = 0; i < bc->nidents; i++) {
        dsbuf_destroy(bc->idents[i]);
        bc->idents[i] = NULL;
    }
    free(bc->idents);
    bc->idents = NULL;
    free(bc);
}

ms_VMOpCode ms_VMOpCodeWithArg(ms_VMOpCodeType c, int arg) {
    return (c | (arg << 16));
}

int ms_VMOpCodeGetArg(ms_VMOpCode c) {
    return (c >> 16);
}

ms_VMOpCodeType ms_VMOpCodeGetCode(ms_VMOpCode c) {
    return (ms_VMOpCodeType)(c & ((1 << 16) - 1));
}

/*
 * PRIVATE FUNCTIONS
 */

static ms_VMByteCode *VMByteCodeNew(const DSArray *opcodes, const DSArray *values, const DSArray *idents) {
    if (!opcodes) { return NULL; }

    ms_VMByteCode *bc = malloc(sizeof(ms_VMByteCode));
    if (!bc) {
        return NULL;
    }

    bc->nops = dsarray_len((DSArray *)opcodes);
    bc->code = malloc(sizeof(ms_VMOpCode) * (bc->nops));
    if (!bc->code) {
        free(bc);
        return NULL;
    }

    bc->nvals = dsarray_len((DSArray *)values);
    bc->values = malloc(sizeof(ms_Value) * (bc->nvals));
    if (!bc->values) {
        free(bc->code);
        free(bc);
        return NULL;
    }

    bc->nidents = dsarray_len((DSArray *)idents);
    bc->idents = malloc(sizeof(ms_Ident *) * (bc->nidents));
    if (!bc->idents) {
        free(bc->values);
        free(bc->code);
        free(bc);
        return NULL;
    }

    for (size_t i = 0; i < bc->nops; i++) {
        bc->code[i] = *(ms_VMOpCode *)dsarray_get((DSArray *)opcodes, i);
    }

    for (size_t i = 0; i < bc->nvals; i++) {
        bc->values[i] = *(ms_Value *)dsarray_get((DSArray *)values, i);
    }

    for (size_t i = 0; i < bc->nidents; i++) {
        bc->idents[i] = dsarray_get((DSArray *)idents, i);
    }

    return bc;
}

static void ExprToOpCodes(ms_Expr *expr, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(expr);
    assert(opcodes);
    assert(values);

    if (expr->type == EXPRTYPE_UNARY) {
        ExprComponentToOpCodes(&expr->cmpnt.u->atom, expr->cmpnt.u->type,
                               opcodes, values, idents);
        ExprOpToOpCode(expr, opcodes);
    } else {
        if (expr->cmpnt.b->op == BINARY_CALL) {
            ExprComponentToOpCodes(&expr->cmpnt.b->ratom, expr->cmpnt.b->rtype,
                                   opcodes, values, idents);
            ExprComponentToOpCodes(&expr->cmpnt.b->latom, expr->cmpnt.b->ltype,
                                   opcodes, values, idents);
            ExprOpToOpCode(expr, opcodes);
        } else {
            ExprComponentToOpCodes(&expr->cmpnt.b->latom, expr->cmpnt.b->ltype,
                                   opcodes, values, idents);
            ExprComponentToOpCodes(&expr->cmpnt.b->ratom, expr->cmpnt.b->rtype,
                                   opcodes, values, idents);
            ExprOpToOpCode(expr, opcodes);
        }
    }
}

static void ExprComponentToOpCodes(ms_ExprAtom *a, ms_ExprAtomType type, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(a);
    assert(opcodes);
    assert(values);

    switch (type) {
        case EXPRATOM_EXPRESSION: {
            ExprToOpCodes(a->expr, opcodes, values, idents);
            break;
        }
        case EXPRATOM_VALUE: {
            ms_VMOpCode *o = malloc(sizeof(ms_VMOpCode));
            if (!o) { return; }

            ms_Value *v = malloc(sizeof(ms_Value));
            if (!v) {
                free(o);
                return;
            }

            *v = a->val;

            dsarray_append(values, v);
            size_t nvals = dsarray_len(values);
            assert(nvals <= INT_MAX);
            *o = ms_VMOpCodeWithArg(OPC_PUSH, (int)(nvals - 1));
            dsarray_append(opcodes, o);
            break;
        }
        case EXPRATOM_IDENT: {
            ms_VMOpCode *o = malloc(sizeof(ms_VMOpCode));
            if (!o) { return; }

            ms_Ident *id = dsbuf_dup(a->ident);
            if (!id) {
                free(o);
                return;
            }

            dsarray_append(idents, id);
            size_t nidents = dsarray_len(idents);
            assert(nidents <= INT_MAX);
            *o = ms_VMOpCodeWithArg(OPC_LOAD_NAME, (int)(nidents - 1));
            dsarray_append(opcodes, o);
            break;
        }
        case EXPRATOM_EXPRLIST: {
            size_t len = dsarray_len(a->list);
            for (size_t i = 0; i < len; i++) {
                ms_Expr *expr = dsarray_get(a->list, i);
                ExprToOpCodes(expr, opcodes, values, idents);
            }
            break;
        }
        case EXPRATOM_EMPTY:
            assert(false);
    }
}

static void ExprOpToOpCode(ms_Expr *expr, DSArray *opcodes) {
    assert(expr);
    assert(opcodes);

    ms_VMOpCode *o = malloc(sizeof(ms_VMOpCode));
    if (!o) { return; }

    if (expr->type == EXPRTYPE_BINARY) {
        switch (expr->cmpnt.b->op) {
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
            case BINARY_CALL:
                *o = OPC_CALL;
                break;
            default:
                free(o);
                return;
        }

        dsarray_append(opcodes, o);
    } else {
        switch (expr->cmpnt.u->op) {
            case UNARY_MINUS:
                *o = OPC_NEGATE;
                break;
            case UNARY_NOT:
                *o = OPC_NOT;
                break;
            case UNARY_BITWISE_NOT:
                *o = OPC_BITWISE_NOT;
                break;
            default:
                free(o);
                return;
        }

        dsarray_append(opcodes, o);
    }
}
