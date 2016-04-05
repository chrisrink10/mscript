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

void ExprAtomDestroy(ms_ExprAtom *atom, ms_ExprAtomType type);
void StmtDeleteDestroy(ms_StmtDelete *del);
void StmtForDestroy(ms_StmtFor *forstmt);
void StmtIfDestroy(ms_StmtIf *ifstmt);
void StmtImportDestroy(ms_StmtImport *import);
void StmtElseDestroy(ms_StmtElse *elstmt);
void StmtMergeDestroy(ms_StmtMerge *merge);
void StmtReturnDestroy(ms_StmtReturn *ret);
void StmtAssignmentDestroy(ms_StmtAssignment *assign);
void StmtDeclarationDestroy(ms_StmtDeclaration *decl);

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

ms_Expr *ms_ExprNewWithFunc(ms_ValFunc *fn) {
    ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
    if (!expr) {
        return NULL;
    }

    expr->cmpnt.u->atom.val.type = MSVAL_FUNC;
    expr->cmpnt.u->atom.val.val.fn = fn;
    expr->cmpnt.u->type = EXPRATOM_VALUE;
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

ms_Expr *ms_ExprDup(const ms_Expr *src) {
    if (!src) { return NULL; }

    ms_Expr *expr = ms_ExprNew(src->type);
    if (!expr) {
        return NULL;
    }

    switch (src->type) {
        case EXPRTYPE_UNARY:
            assert(src->cmpnt.u);
            expr->cmpnt.u->type = src->cmpnt.u->type;
            expr->cmpnt.u->op = src->cmpnt.u->op;
            switch(src->cmpnt.u->type) {
                case EXPRATOM_EXPRESSION:
                    expr->cmpnt.u->atom.expr = ms_ExprDup(src->cmpnt.u->atom.expr);
                    if (!expr->cmpnt.u->atom.expr) {
                        goto expr_dup_fail;
                    }
                    break;
                case EXPRATOM_IDENT:
                    expr->cmpnt.u->atom.ident = dsbuf_dup(src->cmpnt.u->atom.ident);
                    if (!expr->cmpnt.u->atom.ident) {
                        goto expr_dup_fail;
                    }
                    break;
                case EXPRATOM_EXPRLIST: {
                    DSArray *exprlist = dsarray_new_cap(dsarray_cap(expr->cmpnt.u->atom.list),
                                                        NULL, (dsarray_free_fn) ms_ExprDestroy);
                    if (!exprlist) {
                        goto expr_dup_fail;
                    }

                    size_t len = dsarray_len(src->cmpnt.u->atom.list);
                    for (size_t i = 0; i < len; i++) {
                        const ms_Expr *e = dsarray_get(src->cmpnt.u->atom.list, i);
                        ms_Expr *e2 = ms_ExprDup(e);
                        if (!e2) {
                            goto expr_dup_fail;
                        }

                        dsarray_append(exprlist, e2);
                    }

                    expr->cmpnt.u->atom.list = exprlist;
                    }
                    break;
                case EXPRATOM_VALUE:
                    expr->cmpnt.u->atom.val = src->cmpnt.u->atom.val;
                    if (expr->cmpnt.u->atom.val.type == MSVAL_STR) {
                        expr->cmpnt.u->atom.val.val.s = dsbuf_dup(expr->cmpnt.u->atom.val.val.s);
                        if (!expr->cmpnt.u->atom.val.val.s) {
                            goto expr_dup_fail;
                        }
                    }
                    break;
                case EXPRATOM_EMPTY:
                    break;
            }
            break;
        case EXPRTYPE_BINARY:
            assert(expr->cmpnt.b);
            expr->cmpnt.b->ltype = src->cmpnt.b->ltype;
            expr->cmpnt.b->op = src->cmpnt.b->op;
            expr->cmpnt.b->rtype = src->cmpnt.b->rtype;
            switch (src->cmpnt.b->ltype) {
                case EXPRATOM_EXPRESSION:
                    expr->cmpnt.b->latom.expr = ms_ExprDup(src->cmpnt.b->latom.expr);
                    if (!expr->cmpnt.b->latom.expr) {
                        goto expr_dup_fail;
                    }
                    break;
                case EXPRATOM_IDENT:
                    expr->cmpnt.b->latom.ident = dsbuf_dup(src->cmpnt.b->latom.ident);
                    if (!expr->cmpnt.b->latom.ident) {
                        goto expr_dup_fail;
                    }
                    break;
                case EXPRATOM_EXPRLIST: {
                    DSArray *exprlist = dsarray_new_cap(dsarray_cap(expr->cmpnt.b->latom.list),
                                                        NULL, (dsarray_free_fn) ms_ExprDestroy);
                    if (!exprlist) {
                        goto expr_dup_fail;
                    }

                    size_t len = dsarray_len(src->cmpnt.b->latom.list);
                    for (size_t i = 0; i < len; i++) {
                        const ms_Expr *e = dsarray_get(src->cmpnt.b->latom.list, i);
                        ms_Expr *e2 = ms_ExprDup(e);
                        if (!e2) {
                            goto expr_dup_fail;
                        }

                        dsarray_append(exprlist, e2);
                    }

                    expr->cmpnt.b->latom.list = exprlist;
                }
                    break;
                case EXPRATOM_VALUE:
                    expr->cmpnt.b->latom.val = src->cmpnt.b->latom.val;
                    if (expr->cmpnt.b->latom.val.type == MSVAL_STR) {
                        expr->cmpnt.b->latom.val.val.s = dsbuf_dup(expr->cmpnt.b->latom.val.val.s);
                        if (!expr->cmpnt.b->latom.val.val.s) {
                            goto expr_dup_fail;
                        }
                    }
                    break;
                case EXPRATOM_EMPTY:
                    break;
            }
            switch(src->cmpnt.b->rtype) {
                case EXPRATOM_EXPRESSION:
                    expr->cmpnt.b->ratom.expr = ms_ExprDup(src->cmpnt.b->ratom.expr);
                    if (!expr->cmpnt.b->ratom.expr) {
                        goto expr_dup_fail;
                    }
                    break;
                case EXPRATOM_IDENT:
                    expr->cmpnt.b->ratom.ident = dsbuf_dup(src->cmpnt.b->ratom.ident);
                    if (!expr->cmpnt.b->ratom.ident) {
                        goto expr_dup_fail;
                    }
                    break;
                case EXPRATOM_EXPRLIST: {
                    DSArray *exprlist = dsarray_new_cap(dsarray_cap(expr->cmpnt.b->ratom.list),
                                                        NULL, (dsarray_free_fn) ms_ExprDestroy);
                    if (!exprlist) {
                        goto expr_dup_fail;
                    }

                    size_t len = dsarray_len(src->cmpnt.b->ratom.list);
                    for (size_t i = 0; i < len; i++) {
                        const ms_Expr *e = dsarray_get(src->cmpnt.b->ratom.list, i);
                        ms_Expr *e2 = ms_ExprDup(e);
                        if (!e2) {
                            goto expr_dup_fail;
                        }

                        dsarray_append(exprlist, e2);
                    }

                    expr->cmpnt.b->ratom.list = exprlist;
                }
                    break;
                case EXPRATOM_VALUE:
                    expr->cmpnt.b->ratom.val = src->cmpnt.b->ratom.val;
                    if (expr->cmpnt.b->ratom.val.type == MSVAL_STR) {
                        expr->cmpnt.b->ratom.val.val.s = dsbuf_dup(expr->cmpnt.b->ratom.val.val.s);
                        if (!expr->cmpnt.b->ratom.val.val.s) {
                            goto expr_dup_fail;
                        }
                    }
                    break;
                case EXPRATOM_EMPTY:
                    break;
            }
            break;
    }

    return expr;

expr_dup_fail:
    ms_ExprDestroy(expr);
    return NULL;
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

bool ms_ExprIsIdent(const ms_Expr *expr) {
    if (!expr) {
        return false;
    }

    if (expr->type != EXPRTYPE_UNARY) {
        return false;
    }

    if (!expr->cmpnt.u) {
        return false;
    }

    if (expr->cmpnt.u->op != UNARY_NONE) {
        return false;
    }

    if (expr->cmpnt.u->type != EXPRATOM_IDENT) {
        return false;
    }

    return true;
}

bool ms_ExprIsQualifiedIdent(const ms_Expr *expr) {
    if (!expr) {
        return false;
    }

    if (expr->type == EXPRTYPE_UNARY) {
        return ms_ExprIsIdent(expr);
    }

    if (!expr->cmpnt.b) {
        return false;
    }

    if (expr->cmpnt.b->ltype == EXPRATOM_EXPRESSION) {
        if (!ms_ExprIsQualifiedIdent(expr->cmpnt.b->latom.expr)) {
            return false;
        }
    } else if (expr->cmpnt.b->ltype != EXPRATOM_IDENT) {
        return false;
    }

    if (expr->cmpnt.b->rtype != EXPRATOM_IDENT) {
        return false;
    }

    if (expr->cmpnt.b->op != BINARY_GETATTR) {
        return false;
    }

    return true;
}

void ms_ValFuncDestroy(ms_ValFunc *fn) {
    if (!fn) { return; }
    dsbuf_destroy(fn->ident);
    fn->ident = NULL;
    dsarray_destroy(fn->args);
    fn->args = NULL;
    dsarray_destroy(fn->block);
    fn->block = NULL;
    free(fn);
}

void ms_ExprDestroy(ms_Expr *expr) {
    if (!expr) { return; }
    switch (expr->type) {
        case EXPRTYPE_UNARY:
            if (expr->cmpnt.u) {
                ExprAtomDestroy(&expr->cmpnt.u->atom, expr->cmpnt.u->type);
                free(expr->cmpnt.u);
                expr->cmpnt.u = NULL;
            }
            break;
        case EXPRTYPE_BINARY:
            if (expr->cmpnt.b) {
                ExprAtomDestroy(&expr->cmpnt.b->latom, expr->cmpnt.b->ltype);
                ExprAtomDestroy(&expr->cmpnt.b->ratom, expr->cmpnt.b->rtype);
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
        case STMTTYPE_DELETE:
            StmtDeleteDestroy(stmt->cmpnt.del);
            stmt->cmpnt.del = NULL;
            break;
        case STMTTYPE_FOR:
            StmtForDestroy(stmt->cmpnt.forstmt);
            stmt->cmpnt.forstmt = NULL;
            break;
        case STMTTYPE_IF:
            StmtIfDestroy(stmt->cmpnt.ifstmt);
            stmt->cmpnt.ifstmt = NULL;
            break;
        case STMTTYPE_IMPORT:
            StmtImportDestroy(stmt->cmpnt.import);
            stmt->cmpnt.import = NULL;
            break;
        case STMTTYPE_MERGE:
            StmtMergeDestroy(stmt->cmpnt.merge);
            stmt->cmpnt.merge = NULL;
            break;
        case STMTTYPE_RETURN:
            StmtReturnDestroy(stmt->cmpnt.ret);
            stmt->cmpnt.ret = NULL;
            break;
        case STMTTYPE_ASSIGNMENT:
            StmtAssignmentDestroy(stmt->cmpnt.assign);
            stmt->cmpnt.assign = NULL;
            break;
        case STMTTYPE_DECLARATION:
            StmtDeclarationDestroy(stmt->cmpnt.decl);
            stmt->cmpnt.decl = NULL;
            break;
        case STMTTYPE_EXPRESSION:
            ms_ExprDestroy(stmt->cmpnt.expr);
            stmt->cmpnt.expr = NULL;
            break;
    }

    free(stmt);
}

/*
 * PRIVATE FUNCTIONS
 */

void ExprAtomDestroy(ms_ExprAtom *atom, ms_ExprAtomType type) {
    if (!atom) { return; }
    switch(type) {
        case EXPRATOM_EXPRESSION:
            ms_ExprDestroy(atom->expr);
            atom->expr = NULL;
            break;
        case EXPRATOM_IDENT:
            dsbuf_destroy(atom->ident);
            atom->ident = NULL;
            break;
        case EXPRATOM_EXPRLIST:
            dsarray_destroy(atom->list);
            atom->list = NULL;
            break;
        case EXPRATOM_VALUE:
            if (atom->val.type == MSVAL_FUNC) {
                ms_ValFuncDestroy(atom->val.val.fn);
                atom->val.val.fn = NULL;
            }
            break;
        case EXPRATOM_EMPTY:    /* no free required */
            break;
    }
}

void StmtDeleteDestroy(ms_StmtDelete *del) {
    if (!del) { return; }
    ms_ExprDestroy(del->expr);
    del->expr = NULL;
    free(del);
}

void StmtForDestroy(ms_StmtFor *forstmt) {
    if (!forstmt) { return; }
    switch (forstmt->type) {
        case FORSTMT_INCREMENT:
            if (forstmt->clause.inc) {
                ms_ExprDestroy(forstmt->clause.inc->ident);
                forstmt->clause.inc->ident = NULL;
                ms_ExprDestroy(forstmt->clause.inc->init);
                forstmt->clause.inc->init = NULL;
                ms_ExprDestroy(forstmt->clause.inc->end);
                forstmt->clause.inc->end = NULL;
                ms_ExprDestroy(forstmt->clause.inc->step);
                forstmt->clause.inc->step = NULL;
                free(forstmt->clause.inc);
            }
            break;
        case FORSTMT_ITERATOR:
            if (forstmt->clause.iter) {
                ms_ExprDestroy(forstmt->clause.iter->ident);
                forstmt->clause.iter->ident = NULL;
                ms_ExprDestroy(forstmt->clause.iter->iter);
                forstmt->clause.iter->iter = NULL;
                free(forstmt->clause.iter);
            }
            break;
        case FORSTMT_EXPR:
            if (forstmt->clause.expr) {
                ms_ExprDestroy(forstmt->clause.expr->expr);
                forstmt->clause.expr->expr = NULL;
                free(forstmt->clause.expr);
            }
            break;
    }
    free(forstmt);
}

void StmtIfDestroy(ms_StmtIf *ifstmt) {
    if (!ifstmt) { return; }
    ms_ExprDestroy(ifstmt->expr);
    ifstmt->expr = NULL;
    dsarray_destroy(ifstmt->block);
    ifstmt->block = NULL;
    if (ifstmt->elif) {
        switch (ifstmt->elif->type) {
            case IFELSE_IF:
                StmtIfDestroy(ifstmt->elif->clause.ifstmt);
                ifstmt->elif->clause.ifstmt = NULL;
                break;
            case IFELSE_ELSE:
                StmtElseDestroy(ifstmt->elif->clause.elstmt);
                ifstmt->elif->clause.elstmt = NULL;
                break;
        }
    }
    free(ifstmt);
}

void StmtImportDestroy(ms_StmtImport *import) {
    if (!import) { return; }
    ms_ExprDestroy(import->ident);
    import->ident = NULL;
    dsbuf_destroy(import->alias);
    import->alias = NULL;
    free(import);
}

void StmtElseDestroy(ms_StmtElse *elstmt) {
    if (!elstmt) { return; }
    dsarray_destroy(elstmt->block);
    elstmt->block = NULL;
    free(elstmt);
}

void StmtMergeDestroy(ms_StmtMerge *merge) {
    if (!merge) { return; }
    ms_ExprDestroy(merge->left);
    merge->left = NULL;
    ms_ExprDestroy(merge->right);
    merge->right = NULL;
    free(merge);
}

void StmtReturnDestroy(ms_StmtReturn *ret) {
    if (!ret) { return; }
    ms_ExprDestroy(ret->expr);
    ret->expr = NULL;
    free(ret);
}

void StmtAssignmentDestroy(ms_StmtAssignment *assign) {
    if (!assign) { return; }
    ms_ExprDestroy(assign->ident);
    assign->ident = NULL;
    ms_ExprDestroy(assign->expr);
    assign->expr = NULL;
    free(assign);
}

void StmtDeclarationDestroy(ms_StmtDeclaration *decl) {
    if (!decl) { return; }
    dsbuf_destroy(decl->ident);
    decl->ident = NULL;
    ms_ExprDestroy(decl->expr);
    decl->expr = NULL;
    StmtDeclarationDestroy(decl->next);
    decl->next = NULL;
    free(decl);
}
