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
            expr->cmpnt.u = malloc(sizeof(ms_ExprUnary));
            if (!expr->cmpnt.u) {
                free(expr);
                return NULL;
            }
            expr->cmpnt.u->atom.expr = NULL;
            expr->cmpnt.u->op = UNARY_NONE;
            break;
        case EXPRTYPE_BINARY:
            expr->cmpnt.b = malloc(sizeof(ms_ExprBinary));
            if (!expr->cmpnt.b) {
                free(expr);
                return NULL;
            }
            expr->cmpnt.b->latom.expr = NULL;
            expr->cmpnt.b->ltype = EXPRATOM_EMPTY;
            expr->cmpnt.b->op = BINARY_EMPTY;
            expr->cmpnt.b->rtype = EXPRATOM_EMPTY;
            expr->cmpnt.b->ratom.expr = NULL;
            break;
    }

    return expr;
}

ms_Expr *ms_ExprNewWithVal(ms_ValDataType type, ms_ValData v) {
    ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
    if (!expr) {
        return NULL;
    }

    expr->cmpnt.u->atom.val.type = type;
    expr->cmpnt.u->atom.val.val = v;
    expr->cmpnt.u->type = EXPRATOM_VALUE;
    expr->cmpnt.u->op = UNARY_NONE;
    return expr;
}

ms_Expr *ms_ExprNewWithIdent(const char *name, size_t len) {
    ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
    if (!expr) {
        return NULL;
    }

    expr->cmpnt.u->atom.ident = dsbuf_new_l(name, len);
    if (!expr->cmpnt.u->atom.ident) {
        free(expr);
        return NULL;
    }

    expr->cmpnt.u->type = EXPRATOM_IDENT;
    expr->cmpnt.u->op = UNARY_NONE;
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

    expr->cmpnt.u->atom.list = list;
    expr->cmpnt.u->type = EXPRATOM_EXPRLIST;
    expr->cmpnt.u->op = UNARY_NONE;
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

    expr->cmpnt.u->atom.val.type = MSVAL_FLOAT;
    expr->cmpnt.u->atom.val.val.f = f;
    expr->cmpnt.u->type = EXPRATOM_VALUE;
    expr->cmpnt.u->op = UNARY_NONE;
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

    expr->cmpnt.u->atom.val.type = MSVAL_INT;
    expr->cmpnt.u->atom.val.val.i = i;
    expr->cmpnt.u->type = EXPRATOM_VALUE;
    expr->cmpnt.u->op = UNARY_NONE;
    return expr;
}

ms_Expr *ms_ExprFlatten(ms_Expr *outer, ms_Expr *inner, ms_ExprLocation loc) {
    if ((!outer) || (!inner)) { return NULL; }

    bool should_flatten = (inner->type == EXPRTYPE_UNARY) &&
                          (inner->cmpnt.u->op == UNARY_NONE);

    switch (loc) {
        case EXPRLOC_UNARY:
            assert(outer->type == EXPRTYPE_UNARY);
            if (!should_flatten) {
                outer->cmpnt.u->atom.expr = inner;
                outer->cmpnt.u->type = EXPRATOM_EXPRESSION;
            } else {
                outer->cmpnt.u->atom = inner->cmpnt.u->atom;
                outer->cmpnt.u->type = inner->cmpnt.u->type;
            }
            break;
        case EXPRLOC_LEFT:
            assert(outer->type == EXPRTYPE_BINARY);
            if (!should_flatten) {
                outer->cmpnt.b->latom.expr = inner;
                outer->cmpnt.b->ltype = EXPRATOM_EXPRESSION;
            } else {
                outer->cmpnt.b->latom = inner->cmpnt.u->atom;
                outer->cmpnt.b->ltype = inner->cmpnt.u->type;
            }
            break;
        case EXPRLOC_RIGHT:
            assert(outer->type == EXPRTYPE_BINARY);
            if (!should_flatten) {
                outer->cmpnt.b->ratom.expr = inner;
                outer->cmpnt.b->rtype = EXPRATOM_EXPRESSION;
            } else {
                outer->cmpnt.b->ratom = inner->cmpnt.u->atom;
                outer->cmpnt.b->rtype = inner->cmpnt.u->type;
            }
            break;
    }

    /* clear pointers to objects such as lists and strings that would
     * be destroyed when the inner expression is destroyed otherwise */
    if (should_flatten) {
        switch (inner->cmpnt.u->type) {
            case EXPRATOM_EXPRESSION:
                inner->cmpnt.u->atom.expr = NULL;
                break;
            case EXPRATOM_EXPRLIST:
                inner->cmpnt.u->atom.list = NULL;
                break;
            case EXPRATOM_IDENT:
                inner->cmpnt.u->atom.ident = NULL;
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
            if (expr->cmpnt.u) {
                switch(expr->cmpnt.u->type) {
                    case EXPRATOM_EXPRESSION:
                        ms_ExprDestroy(expr->cmpnt.u->atom.expr);
                        break;
                    case EXPRATOM_IDENT:
                        dsbuf_destroy(expr->cmpnt.u->atom.ident);
                        expr->cmpnt.u->atom.ident = NULL;
                        break;
                    case EXPRATOM_EXPRLIST:
                        dsarray_destroy(expr->cmpnt.u->atom.list);
                        expr->cmpnt.u->atom.list = NULL;
                        break;
                    case EXPRATOM_VALUE:    /* no free required */
                    case EXPRATOM_EMPTY:    /* no free required */
                        break;
                }
                free(expr->cmpnt.u);
                expr->cmpnt.u = NULL;
            }
            break;
        case EXPRTYPE_BINARY:
            if (expr->cmpnt.b) {
                switch (expr->cmpnt.b->ltype) {
                    case EXPRATOM_EXPRESSION:
                        ms_ExprDestroy(expr->cmpnt.b->latom.expr);
                        break;
                    case EXPRATOM_IDENT:
                        dsbuf_destroy(expr->cmpnt.b->latom.ident);
                        expr->cmpnt.b->latom.ident = NULL;
                        break;
                    case EXPRATOM_EXPRLIST:
                        dsarray_destroy(expr->cmpnt.b->latom.list);
                        expr->cmpnt.b->latom.list = NULL;
                        break;
                    case EXPRATOM_VALUE:    /* no free required */
                    case EXPRATOM_EMPTY:    /* no free required */
                        break;
                }
                switch(expr->cmpnt.b->rtype) {
                    case EXPRATOM_EXPRESSION:
                        ms_ExprDestroy(expr->cmpnt.b->ratom.expr);
                        break;
                    case EXPRATOM_IDENT:
                        dsbuf_destroy(expr->cmpnt.b->ratom.ident);
                        expr->cmpnt.b->ratom.ident = NULL;
                        break;
                    case EXPRATOM_EXPRLIST:
                        dsarray_destroy(expr->cmpnt.b->ratom.list);
                        expr->cmpnt.b->ratom.list = NULL;
                        break;
                    case EXPRATOM_VALUE:    /* no free required */
                    case EXPRATOM_EMPTY:    /* no free required */
                        break;
                }
                free(expr->cmpnt.b);
                expr->cmpnt.b = NULL;
            }
            break;
    }
    free(expr);
}

void ms_StmtDestroy(ms_Stmt *stmt) {
    if (!stmt) { return; }

    switch (stmt->type) {
        case STMTTYPE_EMPTY:        // Fall through
        case STMTTYPE_BREAK:        // Fall through
        case STMTTYPE_CONTINUE:
            break;
        case STMTTYPE_IF:
            ms_ExprDestroy(stmt->cmpnt.ifstmt->expr);
            stmt->cmpnt.ifstmt->expr = NULL;
            dsarray_destroy(stmt->cmpnt.ifstmt->block);
            stmt->cmpnt.ifstmt->block = NULL;
            break;
        case STMTTYPE_RETURN:
            ms_ExprDestroy(stmt->cmpnt.ret->expr);
            stmt->cmpnt.ret->expr = NULL;
            break;
        case STMTTYPE_ASSIGNMENT:
            ms_ExprDestroy(stmt->cmpnt.assign->ident);
            stmt->cmpnt.assign->ident = NULL;
            ms_ExprDestroy(stmt->cmpnt.assign->expr);
            stmt->cmpnt.assign->expr = NULL;
            break;
        case STMTTYPE_DECLARATION:
            dsbuf_destroy(stmt->cmpnt.decl->ident);
            stmt->cmpnt.decl->ident = NULL;
            ms_ExprDestroy(stmt->cmpnt.decl->expr);
            stmt->cmpnt.decl->expr = NULL;
            break;
        case STMTTYPE_EXPRESSION:
            ms_ExprDestroy(stmt->cmpnt.expr);
            stmt->cmpnt.expr = NULL;
            break;
    }

    free(stmt);
}
