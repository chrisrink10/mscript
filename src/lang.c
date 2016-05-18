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

static bool ExprAtomDup(const ms_ExprAtom *src, ms_ExprAtom *dest, ms_ExprAtomType type);
static void ExprAtomDestroy(ms_ExprAtom *atom, ms_ExprAtomType type);
static void StmtDeleteDestroy(ms_StmtDelete *del);
static void StmtForDestroy(ms_StmtFor *forstmt);
static void StmtIfDestroy(ms_StmtIf *ifstmt);
static void StmtImportDestroy(ms_StmtImport *import);
static void StmtElseDestroy(ms_StmtElse *elstmt);
static void StmtMergeDestroy(ms_StmtMerge *merge);
static void StmtReturnDestroy(ms_StmtReturn *ret);
static void StmtAssignmentDestroy(ms_StmtAssignment *assign);
static void StmtDeclarationDestroy(ms_StmtDeclaration *decl);

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
        case EXPRTYPE_CONDITIONAL:
            expr->cmpnt.c = malloc(sizeof(ms_ExprConditional));
            if (!expr->cmpnt.c) {
                free(expr);
                return NULL;
            }
            expr->cmpnt.c->cond.expr = NULL;
            expr->cmpnt.c->condtype = EXPRATOM_EMPTY;
            expr->cmpnt.c->iftrue.expr = NULL;
            expr->cmpnt.c->truetype = EXPRATOM_EMPTY;
            expr->cmpnt.c->iffalse.expr = NULL;
            expr->cmpnt.c->falsetype = EXPRATOM_EMPTY;
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

    expr->cmpnt.u->atom.ident = malloc(sizeof(ms_Ident));
    if (!expr->cmpnt.u->atom.ident) {
        free(expr);
        return NULL;
    }

    expr->cmpnt.u->atom.ident->name = dsbuf_new_l(name, len);
    expr->cmpnt.u->atom.ident->type = ms_IdentGetType(name);
    if (!expr->cmpnt.u->atom.ident->name) {
        free(expr->cmpnt.u->atom.ident);
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
            if (!ExprAtomDup(&src->cmpnt.u->atom, &expr->cmpnt.u->atom, expr->cmpnt.u->type)) {
                goto expr_dup_fail;
            }
            break;
        case EXPRTYPE_BINARY:
            assert(expr->cmpnt.b);
            expr->cmpnt.b->ltype = src->cmpnt.b->ltype;
            expr->cmpnt.b->op = src->cmpnt.b->op;
            expr->cmpnt.b->rtype = src->cmpnt.b->rtype;
            if (!ExprAtomDup(&src->cmpnt.b->latom, &expr->cmpnt.b->latom, expr->cmpnt.b->ltype)) {
                goto expr_dup_fail;
            }
            if (!ExprAtomDup(&src->cmpnt.b->ratom, &expr->cmpnt.b->ratom, expr->cmpnt.b->rtype)) {
                goto expr_dup_fail;
            }
            break;
        case EXPRTYPE_CONDITIONAL:
            assert(expr->cmpnt.c);
            if (!ExprAtomDup(&src->cmpnt.c->cond, &expr->cmpnt.c->cond, expr->cmpnt.c->condtype)) {
                goto expr_dup_fail;
            }
            if (!ExprAtomDup(&src->cmpnt.c->iftrue, &expr->cmpnt.c->iftrue, expr->cmpnt.c->truetype)) {
                goto expr_dup_fail;
            }
            if (!ExprAtomDup(&src->cmpnt.c->iffalse, &expr->cmpnt.c->iffalse, expr->cmpnt.c->falsetype)) {
                goto expr_dup_fail;
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
        case EXPRLOC_COND:
            assert(outer->type == EXPRTYPE_CONDITIONAL);
            if (!should_flatten) {
                outer->cmpnt.c->cond.expr = inner;
                outer->cmpnt.c->condtype = EXPRATOM_EXPRESSION;
            } else {
                outer->cmpnt.c->cond = inner->cmpnt.u->atom;
                outer->cmpnt.c->condtype = inner->cmpnt.u->type;
            }
            break;
        case EXPRLOC_TRUE:
            assert(outer->type == EXPRTYPE_CONDITIONAL);
            if (!should_flatten) {
                outer->cmpnt.c->iftrue.expr = inner;
                outer->cmpnt.c->truetype = EXPRATOM_EXPRESSION;
            } else {
                outer->cmpnt.c->iftrue = inner->cmpnt.u->atom;
                outer->cmpnt.c->truetype = inner->cmpnt.u->type;
            }
            break;
        case EXPRLOC_FALSE:
            assert(outer->type == EXPRTYPE_CONDITIONAL);
            if (!should_flatten) {
                outer->cmpnt.c->iffalse.expr = inner;
                outer->cmpnt.c->falsetype = EXPRATOM_EXPRESSION;
            } else {
                outer->cmpnt.c->iffalse = inner->cmpnt.u->atom;
                outer->cmpnt.c->falsetype = inner->cmpnt.u->type;
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
                if (inner->cmpnt.u->atom.val.type == MSVAL_FUNC) {
                    inner->cmpnt.u->atom.val.val.fn = NULL;
                } else if (inner->cmpnt.u->atom.val.type == MSVAL_STR) {
                    inner->cmpnt.u->atom.val.val.s = NULL;
                }
                break;
            case EXPRATOM_EMPTY:
                break;
        }
        ms_ExprDestroy(inner);
    }

    return outer;
}

ms_ExprIdentType ms_ExprGetIdentType(const ms_Expr *expr) {
    assert(expr);

    if (expr->type == EXPRTYPE_UNARY) {
        if (!expr->cmpnt.u) {
            return EXPRIDENT_NONE;
        }

        if (expr->cmpnt.u->op != UNARY_NONE) {
            return EXPRIDENT_NONE;
        }

        if (expr->cmpnt.u->type != EXPRATOM_IDENT) {
            return EXPRIDENT_NONE;
        }

        switch (expr->cmpnt.u->atom.ident->type) {
            case IDENT_GLOBAL:
                return EXPRIDENT_GLOBAL;
            case IDENT_BUILTIN:
                return EXPRIDENT_BUILTIN;
            case IDENT_NAME:
                return EXPRIDENT_NAME;
        }

        return EXPRIDENT_NONE;
    } else if (expr->type == EXPRTYPE_BINARY) {
        if (!expr->cmpnt.b) {
            return EXPRIDENT_NONE;
        }

        if ((expr->cmpnt.b->op != BINARY_GETATTR) &&
            (expr->cmpnt.b->op != BINARY_SAFEGETATTR)) {
            return EXPRIDENT_NONE;
        }

        if (expr->cmpnt.b->rtype == EXPRATOM_VALUE) {
            if (expr->cmpnt.b->ratom.val.type != MSVAL_STR) {
                return EXPRIDENT_NONE;
            }
        } else if (expr->cmpnt.b->rtype == EXPRATOM_EXPRLIST) {
            return EXPRIDENT_NONE;
        }

        if (expr->cmpnt.b->ltype == EXPRATOM_EXPRESSION) {
            ms_ExprIdentType inner_type = ms_ExprGetIdentType(expr->cmpnt.b->latom.expr);
            if ((inner_type == EXPRIDENT_NAME) ||
                (inner_type == EXPRIDENT_QUALIFIED)) {
                return EXPRIDENT_QUALIFIED;
            } else {
                return inner_type;
            }
        } else if (expr->cmpnt.b->ltype == EXPRATOM_IDENT) {
            switch (expr->cmpnt.b->latom.ident->type) {
                case IDENT_GLOBAL:
                    return EXPRIDENT_GLOBAL;
                case IDENT_BUILTIN:
                    return EXPRIDENT_BUILTIN;
                case IDENT_NAME:
                    return EXPRIDENT_QUALIFIED;
            }
        }

        return EXPRIDENT_NONE;
    } else {
        return EXPRIDENT_NONE;
    }
}

ms_IdentType ms_IdentGetType(const char *ident) {
    assert(ident);

    switch (ident[0]) {
        case '$':
            return IDENT_BUILTIN;
        case '@':
            return IDENT_GLOBAL;
        default:
            return IDENT_NAME;
    }
}

void ms_IdentDestroy(ms_Ident *ident) {
    if (!ident) { return; }
    dsbuf_destroy(ident->name);
    ident->name = NULL;
    free(ident);
}

void ms_ValFuncDestroy(ms_ValFunc *fn) {
    if (!fn) { return; }
    ms_IdentDestroy(fn->ident);
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
        case EXPRTYPE_CONDITIONAL:
            if (expr->cmpnt.c) {
                ExprAtomDestroy(&expr->cmpnt.c->cond, expr->cmpnt.c->condtype);
                ExprAtomDestroy(&expr->cmpnt.c->iftrue, expr->cmpnt.c->truetype);
                ExprAtomDestroy(&expr->cmpnt.c->iffalse, expr->cmpnt.c->falsetype);
                free(expr->cmpnt.c);
                expr->cmpnt.c = NULL;
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

static bool ExprAtomDup(const ms_ExprAtom *src, ms_ExprAtom *dest, ms_ExprAtomType type) {
    assert(src);
    assert(dest);

    switch(type) {
        case EXPRATOM_EXPRESSION:
            dest->expr = ms_ExprDup(src->expr);
            if (!dest->expr) {
                goto expr_atom_dup_fail;
            }
            break;
        case EXPRATOM_IDENT:
            dest->ident = malloc(sizeof(ms_Ident));
            if (!dest->ident) {
                goto expr_atom_dup_fail;
            }

            dest->ident->type = src->ident->type;
            dest->ident->name = dsbuf_dup(src->ident->name);
            if (!dest->ident->name) {
                goto expr_atom_dup_fail;
            }
            break;
        case EXPRATOM_EXPRLIST: {
            size_t srclen = dsarray_cap(src->list);
            DSArray *exprlist = dsarray_new_cap(srclen, NULL, (dsarray_free_fn) ms_ExprDestroy);
            if (!exprlist) {
                goto expr_atom_dup_fail;
            }

            size_t len = dsarray_len(src->list);
            for (size_t i = 0; i < len; i++) {
                const ms_Expr *e = dsarray_get(src->list, i);
                ms_Expr *e2 = ms_ExprDup(e);
                if (!e2) {
                    goto expr_atom_dup_fail;
                }

                dsarray_append(exprlist, e2);
            }

            dest->list = exprlist;
            break;
        }
        case EXPRATOM_VALUE:
            dest->val = src->val;
            if (dest->val.type == MSVAL_STR) {
                dest->val.val.s = dsbuf_dup(src->val.val.s);
                if (!dest->val.val.s) {
                    goto expr_atom_dup_fail;
                }
            }
            break;
        case EXPRATOM_EMPTY:
            assert(false && "expression atom should have a type");
            break;
    }

    return true;

expr_atom_dup_fail:
    return false;
}

static void ExprAtomDestroy(ms_ExprAtom *atom, ms_ExprAtomType type) {
    if (!atom) { return; }
    switch(type) {
        case EXPRATOM_EXPRESSION:
            ms_ExprDestroy(atom->expr);
            atom->expr = NULL;
            break;
        case EXPRATOM_IDENT:
            ms_IdentDestroy(atom->ident);
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
            } else if (atom->val.type == MSVAL_STR) {
                dsbuf_destroy(atom->val.val.s);
                atom->val.val.s = NULL;
            }
            break;
        case EXPRATOM_EMPTY:    /* no free required */
            break;
    }
}

static void StmtDeleteDestroy(ms_StmtDelete *del) {
    if (!del) { return; }
    ms_ExprDestroy(del->expr);
    del->expr = NULL;
    free(del);
}

static void StmtForDestroy(ms_StmtFor *forstmt) {
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

    dsarray_destroy(forstmt->block);
    forstmt->block = NULL;
    free(forstmt);
}

static void StmtIfDestroy(ms_StmtIf *ifstmt) {
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
        free(ifstmt->elif);
    }

    free(ifstmt);
}

static void StmtImportDestroy(ms_StmtImport *import) {
    if (!import) { return; }
    ms_ExprDestroy(import->ident);
    import->ident = NULL;
    ms_IdentDestroy(import->alias);
    import->alias = NULL;
    free(import);
}

static void StmtElseDestroy(ms_StmtElse *elstmt) {
    if (!elstmt) { return; }
    dsarray_destroy(elstmt->block);
    elstmt->block = NULL;
    free(elstmt);
}

static void StmtMergeDestroy(ms_StmtMerge *merge) {
    if (!merge) { return; }
    ms_ExprDestroy(merge->left);
    merge->left = NULL;
    ms_ExprDestroy(merge->right);
    merge->right = NULL;
    free(merge);
}

static void StmtReturnDestroy(ms_StmtReturn *ret) {
    if (!ret) { return; }
    ms_ExprDestroy(ret->expr);
    ret->expr = NULL;
    free(ret);
}

static void StmtAssignmentDestroy(ms_StmtAssignment *assign) {
    if (!assign) { return; }
    ms_ExprDestroy(assign->ident);
    assign->ident = NULL;
    ms_ExprDestroy(assign->expr);
    assign->expr = NULL;
    free(assign);
}

static void StmtDeclarationDestroy(ms_StmtDeclaration *decl) {
    if (!decl) { return; }
    ms_IdentDestroy(decl->ident);
    decl->ident = NULL;
    ms_ExprDestroy(decl->expr);
    decl->expr = NULL;
    StmtDeclarationDestroy(decl->next);
    decl->next = NULL;
    free(decl);
}
