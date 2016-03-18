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
#include <string.h>
#include <sys/errno.h>
#include "bytecode.h"
#include "lang.h"

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

ms_Expr *ms_ExprNewWithVal(ms_ValDataType type, ms_ValData v) {
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

ms_Expr *ms_ExprNewWithIdent(const char *name, size_t len) {
    ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
    if (!expr) {
        return NULL;
    }

    expr->expr.u->expr.ident = dsbuf_new_l(name, len);
    if (!expr->expr.u->expr.ident) {
        free(expr);
        return NULL;
    }

    expr->expr.u->type = EXPRATOM_IDENT;
    expr->expr.u->op = UNARY_NONE;
    return expr;
}

ms_Expr *ms_ExprNewWithList(ms_ExprList *list) {
    if (!list) {
        return NULL;
    }

    ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
    if (!expr) {
        return NULL;
    }

    expr->expr.u->expr.list = list;
    expr->expr.u->type = EXPRATOM_EXPRLIST;
    expr->expr.u->op = UNARY_NONE;
    return expr;
}

ms_Expr *ms_ExprFloatFromString(const char *str) {
    assert(str);

    ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
    if (!expr) {
        return NULL;
    }

    errno = 0;
    ms_ValFloat f = strtod(str, NULL);
    if (errno != 0) {
        ms_ExprDestroy(expr);
        return NULL;
    }

    expr->expr.u->expr.val.type = MSVAL_FLOAT;
    expr->expr.u->expr.val.val.f = f;
    expr->expr.u->type = EXPRATOM_VALUE;
    expr->expr.u->op = UNARY_NONE;
    return expr;
}

ms_Expr *ms_ExprIntFromString(const char *str) {
    assert(str);

    ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
    if (!expr) {
        return NULL;
    }

    errno = 0;
    ms_ValInt i = strtoll(str, NULL, 10);
    if (errno != 0) {
        ms_ExprDestroy(expr);
        return NULL;
    }

    expr->expr.u->expr.val.type = MSVAL_INT;
    expr->expr.u->expr.val.val.i = i;
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
            }
            break;
    }

    /* clear pointers to objects such as lists and strings that would
     * be destroyed when the inner expression is destroyed otherwise */
    if (should_flatten) {
        switch (inner->expr.u->type) {
            case EXPRATOM_EXPRESSION:
                inner->expr.u->expr.expr = NULL;
                break;
            case EXPRATOM_EXPRLIST:
                inner->expr.u->expr.list = NULL;
                break;
            case EXPRATOM_IDENT:
                inner->expr.u->expr.ident = NULL;
                break;
            case EXPRATOM_VALUE:
            case EXPRATOM_EMPTY:
                break;
        }
        ms_ExprDestroy(inner);
    }

    return outer;
}

void ms_ExprDestroy(ms_Expr *expr) {
    if (!expr) { return; }
    switch (expr->type) {
        case EXPRTYPE_UNARY:
            if (expr->expr.u) {
                switch(expr->expr.u->type) {
                    case EXPRATOM_EXPRESSION:
                        ms_ExprDestroy(expr->expr.u->expr.expr);
                        break;
                    case EXPRATOM_IDENT:
                        dsbuf_destroy(expr->expr.u->expr.ident);
                        expr->expr.u->expr.ident = NULL;
                        break;
                    case EXPRATOM_EXPRLIST:
                        dsarray_destroy(expr->expr.u->expr.list);
                        expr->expr.u->expr.list = NULL;
                        break;
                    case EXPRATOM_VALUE:    /* no free required */
                    case EXPRATOM_EMPTY:    /* no free required */
                        break;
                }
                free(expr->expr.u);
                expr->expr.u = NULL;
            }
            break;
        case EXPRTYPE_BINARY:
            if (expr->expr.b) {
                switch (expr->expr.b->ltype) {
                    case EXPRATOM_EXPRESSION:
                        ms_ExprDestroy(expr->expr.b->left.expr);
                        break;
                    case EXPRATOM_IDENT:
                        dsbuf_destroy(expr->expr.b->left.ident);
                        expr->expr.b->left.ident = NULL;
                        break;
                    case EXPRATOM_EXPRLIST:
                        dsarray_destroy(expr->expr.b->left.list);
                        expr->expr.b->left.list = NULL;
                        break;
                    case EXPRATOM_VALUE:    /* no free required */
                    case EXPRATOM_EMPTY:    /* no free required */
                        break;
                }
                switch(expr->expr.b->rtype) {
                    case EXPRATOM_EXPRESSION:
                        ms_ExprDestroy(expr->expr.b->right.expr);
                        break;
                    case EXPRATOM_IDENT:
                        dsbuf_destroy(expr->expr.b->right.ident);
                        expr->expr.b->right.ident = NULL;
                        break;
                    case EXPRATOM_EXPRLIST:
                        dsarray_destroy(expr->expr.b->right.list);
                        expr->expr.b->right.list = NULL;
                        break;
                    case EXPRATOM_VALUE:    /* no free required */
                    case EXPRATOM_EMPTY:    /* no free required */
                        break;
                }
                free(expr->expr.b);
                expr->expr.b = NULL;
            }
            break;
    }
    free(expr);
}
