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
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include "libds/array.h"
#include "libds/dict.h"
#include "libds/list.h"
#include "verifier.h"
#include "lang.h"

static int VERIFIER_SYMBOL_VAL = 1;

/*
 * FORWARD DECLARATIONS
 */

typedef enum {
    ASTCTX_MODULE,
    ASTCTX_FUNCTION,
    ASTCTX_IFSTMT,
    ASTCTX_FORSTMT,
} ASTElementContextType;

typedef struct ASTContext ASTContext;
struct ASTContext {
    ASTContext *parent;
    ASTElementContextType type;
    DSDict *symbols;
};

typedef DSList ContextEvalQueue;

typedef struct {
    ASTContext *ctx;
    const ms_Stmt *stmt;
} ContextQueueTuple;

typedef struct {
    ContextEvalQueue *queue;
    DSArray *ctxlist;
} QueueContext;

static const char *const ERR_BREAK_OUTSIDE_FOR = "`break` statements may only appear within a `for` statement block";
static const char *const ERR_CONTINUE_OUTSIDE_FOR = "`continue` statements may only appear within a `for` statement block";
static const char *const ERR_REDECLARATION_IN_IMPORT = "cannot redclare variable `%s` using import alias";
static const char *const ERR_RETURN_OUTSIDE_FUNC = "`return` statements must appear within a function declaration";
static const char *const ERR_BAD_IDENT_IN_DECLARATION = "builtin identifiers and globals may not appear in `var` statement";
static const char *const ERR_VAR_REDECLARATION = "redeclaration of variable `%s` prohibited";
static const char *const ERR_ASSIGNMENT_COUNT_MISMATCH = "number of assignment targets does not match number of assignment expressions";
static const char *const ERR_REFERENCE_TO_UNDEFINED = "reference to undefined variable `%s`";
static const char *const ERR_INVALID_STATEMENT_TYPE = "Invalid statement type encountered.";
static const char *const ERR_EMPTY_EXPR_ATOM = "encountered an empty expression atom";

static ASTContext *ASTContextNew(ASTContext *parent, ASTElementContextType type);
static void ASTContextDestroy(ASTContext *ctx);
static ms_Result QueueContextInit(QueueContext *qctx);
static void QueueContextClean(QueueContext *qctx);
static void QueueContextTupleDestroy(ContextQueueTuple *tuple);

static ms_Result ParserEnqueueModule(const ms_Module *module, QueueContext *qctx);
static ms_Result ParserEnqueueStatement(const ms_Stmt *stmt, ASTContext *ctx, QueueContext *qctx);
static ms_Result ParserEnqueueIfStmt(const ms_StmtIf *ifstmt, ASTContext *parent, QueueContext *qctx);
static ms_Result ParserEnqueueElseIfStmt(const ms_StmtIfElse *elif, ASTContext *parent, QueueContext *qctx);
static ms_Result ParserEnqueueExpression(const ms_Expr *expr, ASTContext *parent, QueueContext *qctx);
static ms_Result ParserEnqueueExprAtom(const ms_ExprAtom *atom, ms_ExprAtomType atomtype, ASTContext *parent, QueueContext *qctx);
static ms_Result ParserEnqueueExprValue(const ms_Value *val, ASTContext *parent, QueueContext *qctx);
static ms_Result ParserEnqueueBlock(const ms_StmtBlock *block, ASTContext *parent, ASTElementContextType type, QueueContext *qctx);
static ms_Result ParserEnqueueBlockWithChild(const ms_StmtBlock *block, ASTContext *parent, ASTElementContextType type, QueueContext *qctx, ASTContext **blockctx);

static ms_Result ParserDepleteQueue(QueueContext *qctx, ms_Error **err);

static ms_Result ParserVerifyStatement(const ms_Stmt *stmt, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyBreakStmt(const ms_StmtBreak *brk, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyContinueStmt(const ms_StmtContinue *cont, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyDeleteStmt(const ms_StmtDelete *del, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyForStmt(const ms_StmtFor *forstmt, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyForIncrement(const ms_StmtForIncrement *inc, ASTContext *ctx, QueueContext *qctx, ASTContext *childctx, ms_Error **err);
static ms_Result ParserVerifyForIterator(const ms_StmtForIterator *iter, ASTContext *ctx, QueueContext *qctx, ASTContext *childctx, ms_Error **err);
static ms_Result ParserVerifyIfStmt(const ms_StmtIf *ifstmt, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyElseIfStmt(const ms_StmtIfElse *elif, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyImportStmt(const ms_StmtImport *import, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyReturnStmt(const ms_StmtReturn *ret, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyDeclaration(const ms_StmtDeclaration *decl, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyAssignment(const ms_StmtAssignment *assign, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyExpression(const ms_Expr *expr, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyExprAtom(const ms_ExprAtom *atom, ms_ExprAtomType type, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyExprAtomValue(const ms_Value *val, ASTContext *ctx, QueueContext *qctx, ms_Error **err);
static ms_Result ParserVerifyIdent(const ms_Ident *ident, ASTContext *ctx, QueueContext *qctx, ms_Error **err);

static inline bool VerifierInContext(ASTContext *ctx, ASTElementContextType type);
static inline bool VerifierInConstrainedContext(ASTContext *ctx, ASTElementContextType ancestor, ASTElementContextType type);
static inline bool VerifierSymbolExistsInCurrentScope(ASTContext *ctx, DSBuffer *buf);
static inline bool VerifierSymbolExistsInLexicalScope(ASTContext *ctx, DSBuffer *buf);
static void VerifierErrorSet(ms_Error **err, const char *msg, ...);

/*
 * PUBLIC FUNCTIONS
 */

ms_Result ms_ParserVerifyAST(const ms_AST *ast, ms_Error **err) {
    assert(ast);
    assert(err);

    ms_Result res;
    QueueContext qctx = { .queue = NULL, .ctxlist = NULL };
    *err = NULL;

    if ((res = QueueContextInit(&qctx)) == MS_RESULT_ERROR) {
        goto cleanup_verify_ast;
    }

    if ((res = ParserEnqueueModule(ast, &qctx)) == MS_RESULT_ERROR) {
        goto cleanup_verify_ast;
    }

    if ((res = ParserDepleteQueue(&qctx, err)) == MS_RESULT_ERROR) {
        goto cleanup_verify_ast;
    }

cleanup_verify_ast:
    QueueContextClean(&qctx);
    return res;
}

/*
 * CONTEXT FUNCTIONS
 */

static ASTContext *ASTContextNew(ASTContext *parent, ASTElementContextType type) {
    ASTContext *ctx = malloc(sizeof(ASTContext));
    if (!ctx) {
        return NULL;
    }

    ctx->symbols = dsdict_new((dsdict_hash_fn)dsbuf_hash,
                              (dsdict_compare_fn)dsbuf_compare, NULL, NULL);
    if (!ctx->symbols) {
        free(ctx);
        return NULL;
    }

    ctx->parent = parent;
    ctx->type = type;
    return ctx;
}

static void ASTContextDestroy(ASTContext *ctx) {
    if (!ctx) { return; }
    ctx->parent = NULL;
    dsdict_destroy(ctx->symbols);
    ctx->symbols = NULL;
    free(ctx);
}

static ms_Result QueueContextInit(QueueContext *qctx) {
    assert(qctx);

    /* array of ASTContexts to make sure they're all freed */
    qctx->ctxlist = dsarray_new(NULL, (dsarray_free_fn)ASTContextDestroy);
    if (!qctx->ctxlist) {
        return MS_RESULT_ERROR;
    }

    /* queue of statements to be evaluated breadth-first to make sure
     * we always have access to identifiers declared later in an enclosing
     * lexical context */
    qctx->queue = dslist_new(NULL, (dslist_free_fn)QueueContextTupleDestroy);
    if (!qctx->queue) {
        return MS_RESULT_ERROR;
    }

    return MS_RESULT_SUCCESS;
}

static void QueueContextClean(QueueContext *qctx) {
    if (!qctx) { return; }
    dsarray_destroy(qctx->ctxlist);
    dslist_destroy(qctx->queue);
}

static void QueueContextTupleDestroy(ContextQueueTuple *tuple) {
    if (!tuple) { return; }
    tuple->ctx = NULL;
    tuple->stmt = NULL;
    free(tuple);
}

/*
 * ENQUEUE FUNCTIONS
 *
 * The verifier reads the entire abstract syntax tree statement-by-statement
 * into a queue in a breadth-first pattern. This allows the verifier to
 * analyze each nested lexical block within the full context of the
 * containing block.
 *
 * In a module, this means that code like this:
 *
 *     func test() { return x; }
 *     var x = 5;
 *     test();
 *
 * ...would return `5` because the statement block for function `test` is
 * evaluated _after_ the containing module.
 */

static ms_Result ParserEnqueueModule(const ms_Module *module, QueueContext *qctx) {
    assert(module);
    assert(qctx);

    ASTContext *ctx = ASTContextNew(NULL, ASTCTX_MODULE);
    if (!ctx) {
        return MS_RESULT_ERROR;
    }
    dsarray_append(qctx->ctxlist, ctx);

    size_t len = dsarray_len(module);
    for(size_t i = 0; i < len; i++) {
        ms_Stmt *stmt = dsarray_get(module, i);
        if (ParserEnqueueStatement(stmt, ctx, qctx) == MS_RESULT_ERROR) {
            return MS_RESULT_ERROR;
        }
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserEnqueueStatement(const ms_Stmt *stmt, ASTContext *ctx, QueueContext *qctx) {
    assert(stmt);
    assert(ctx);
    assert(qctx);

    ContextQueueTuple *tuple = malloc(sizeof(ContextQueueTuple));
    if (!tuple) {
        return MS_RESULT_ERROR;
    }

    tuple->stmt = stmt;
    tuple->ctx = ctx;
    dslist_enqueue(qctx->queue, tuple);
    return MS_RESULT_SUCCESS;
}

static ms_Result ParserEnqueueIfStmt(const ms_StmtIf *ifstmt, ASTContext *parent, QueueContext *qctx) {
    assert(ifstmt);
    assert(parent);
    assert(qctx);

    if (ParserEnqueueBlock(ifstmt->block, parent, ASTCTX_IFSTMT, qctx) == MS_RESULT_ERROR) {
        return MS_RESULT_ERROR;
    }

    if (!ifstmt->elif) {
        return MS_RESULT_SUCCESS;
    }

    if (ParserEnqueueElseIfStmt(ifstmt->elif, parent, qctx) == MS_RESULT_ERROR) {
        return MS_RESULT_ERROR;
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserEnqueueElseIfStmt(const ms_StmtIfElse *elif, ASTContext *parent, QueueContext *qctx) {
    assert(elif);
    assert(parent);
    assert(qctx);

    switch (elif->type) {
        case IFELSE_IF: {
            if (ParserEnqueueBlock(elif->clause.ifstmt->block, parent, ASTCTX_IFSTMT, qctx) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            return ParserEnqueueElseIfStmt(elif->clause.ifstmt->elif, parent, qctx);
        }
        case IFELSE_ELSE:
            return ParserEnqueueBlock(elif->clause.elstmt->block, parent, ASTCTX_IFSTMT, qctx);
    }

    assert(false);
    return MS_RESULT_ERROR;
}

static ms_Result ParserEnqueueExpression(const ms_Expr *expr, ASTContext *parent, QueueContext *qctx) {
    assert(expr);
    assert(parent);
    assert(qctx);

    switch (expr->type) {
        case EXPRTYPE_UNARY: {
            return ParserEnqueueExprAtom(&expr->cmpnt.u->atom, expr->cmpnt.u->type, parent, qctx);
        }
        case EXPRTYPE_BINARY: {
            if (ParserEnqueueExprAtom(&expr->cmpnt.b->latom, expr->cmpnt.b->ltype, parent, qctx) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }

            if (ParserEnqueueExprAtom(&expr->cmpnt.b->ratom, expr->cmpnt.b->rtype, parent, qctx) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }

            return MS_RESULT_SUCCESS;
        }
        case EXPRTYPE_CONDITIONAL: {
            if (ParserEnqueueExprAtom(&expr->cmpnt.c->cond, expr->cmpnt.c->condtype, parent, qctx) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }

            if (ParserEnqueueExprAtom(&expr->cmpnt.c->iftrue, expr->cmpnt.c->truetype, parent, qctx) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }

            if (ParserEnqueueExprAtom(&expr->cmpnt.c->iffalse, expr->cmpnt.c->falsetype, parent, qctx) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }

            return MS_RESULT_SUCCESS;
        }
    }

    assert(false);
    return MS_RESULT_ERROR;
}

static ms_Result ParserEnqueueExprAtom(const ms_ExprAtom *atom, ms_ExprAtomType atomtype, ASTContext *parent, QueueContext *qctx) {
    assert(atom);
    assert(parent);
    assert(qctx);

    switch (atomtype) {
        case EXPRATOM_IDENT:        /* fall through */
        case EXPRATOM_EMPTY:
            return MS_RESULT_SUCCESS;
        case EXPRATOM_EXPRLIST: {
            size_t len = dsarray_len(atom->list);
            for (size_t i = 0; i < len; i++) {
                ms_Expr *expr = dsarray_get(atom->list, i);
                if (ParserEnqueueExpression(expr, parent, qctx) == MS_RESULT_ERROR) {
                    return MS_RESULT_ERROR;
                }
            }
            return MS_RESULT_SUCCESS;
        }
        case EXPRATOM_VALUE:
            return ParserEnqueueExprValue(&atom->val, parent, qctx);
        case EXPRATOM_EXPRESSION:
            return ParserEnqueueExpression(atom->expr, parent, qctx);
    }

    assert(false);
    return MS_RESULT_ERROR;
}

static ms_Result ParserEnqueueExprValue(const ms_Value *val, ASTContext *parent, QueueContext *qctx) {
    assert(val);
    assert(parent);
    assert(qctx);

    switch (val->type) {
        case MSVAL_FLOAT:       /* fall through */
        case MSVAL_INT:         /* fall through */
        case MSVAL_NULL:        /* fall through */
        case MSVAL_BOOL:        /* fall through */
        case MSVAL_STR:
            return MS_RESULT_SUCCESS;
        case MSVAL_ARRAY: {
            size_t len = dsarray_len(val->val.a);
            for (size_t i = 0; i < len; i++) {
                ms_Expr *expr = dsarray_get(val->val.a, i);
                if (ParserEnqueueExpression(expr, parent, qctx) == MS_RESULT_ERROR) {
                    return MS_RESULT_ERROR;
                }
            }
            return MS_RESULT_SUCCESS;
        }
        case MSVAL_OBJECT: {
            size_t len = dsarray_len(val->val.o);
            for (size_t i = 0; i < len; i++) {
                ms_ValObjectTuple *tuple = dsarray_get(val->val.o, i);
                if (ParserEnqueueExpression(tuple->key, parent, qctx) == MS_RESULT_ERROR) {
                    return MS_RESULT_ERROR;
                }
                if (ParserEnqueueExpression(tuple->val, parent, qctx) == MS_RESULT_ERROR) {
                    return MS_RESULT_ERROR;
                }
            }
            return MS_RESULT_SUCCESS;
        }
        case MSVAL_FUNC:
            return ParserEnqueueBlock(val->val.fn->block, parent, ASTCTX_FUNCTION, qctx);
    }

    assert(false);
    return MS_RESULT_ERROR;
}

static ms_Result ParserEnqueueBlock(const ms_StmtBlock *block, ASTContext *parent, ASTElementContextType type, QueueContext *qctx) {
    assert(block);
    assert(parent);
    assert(qctx);

    ASTContext *blockctx;
    return ParserEnqueueBlockWithChild(block, parent, type, qctx, &blockctx);
}

static ms_Result ParserEnqueueBlockWithChild(const ms_StmtBlock *block, ASTContext *parent, ASTElementContextType type, QueueContext *qctx, ASTContext **blockctx) {
    assert(block);
    assert(parent);
    assert(qctx);

    *blockctx = ASTContextNew(parent, type);
    if (!(*blockctx)) {
        return MS_RESULT_ERROR;
    }
    dsarray_append(qctx->ctxlist, (*blockctx));

    size_t len = dsarray_len(block);
    for(size_t i = 0; i < len; i++) {
        ms_Stmt *stmt = dsarray_get(block, i);
        if (ParserEnqueueStatement(stmt, *blockctx, qctx) == MS_RESULT_ERROR) {
            ASTContextDestroy(*blockctx);
            return MS_RESULT_ERROR;
        }
    }

    return MS_RESULT_SUCCESS;
}

/*
 * QUEUE PROCESSOR
 *
 * Process the queue statement by statement. Each statement with a containing
 * statement block will enqueue the statements contained in the block as
 * the statement is evaluated.
 */

static ms_Result ParserDepleteQueue(QueueContext *qctx, ms_Error **err) {
    assert(qctx);
    assert(err);

    ContextQueueTuple *tuple;
    while ((tuple = dslist_dequeue(qctx->queue)) != NULL) {
        if (ParserVerifyStatement(tuple->stmt, tuple->ctx, qctx, err) == MS_RESULT_ERROR) {
            QueueContextTupleDestroy(tuple);
            return MS_RESULT_ERROR;
        }
        QueueContextTupleDestroy(tuple);
    }

    return MS_RESULT_SUCCESS;
}

/*
 * VERIFICATION FUNCTIONS
 *
 * For each statement added to the queue, the verifier calls one of the
 * following functions to verify that the statement complies with its
 * proper contextual semantics.
 *
 * Some of the verifications performed:
 *  - Redeclaration of a name in the same block
 *  - Referencing an undefined name
 *  - Shadowing a name from an enclosing scope (warning only)
 *  - `break` and `continue` statements only appear within a `for` loop
 *  - `return` statements only appear within a function definition
 */

static ms_Result ParserVerifyStatement(const ms_Stmt *stmt, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(stmt);
    assert(ctx);
    assert(err);

    switch (stmt->type) {
        case STMTTYPE_BREAK:
            return ParserVerifyBreakStmt(stmt->cmpnt.brk, ctx, qctx, err);
        case STMTTYPE_CONTINUE:
            return ParserVerifyContinueStmt(stmt->cmpnt.cont, ctx, qctx, err);
        case STMTTYPE_DELETE:
            return ParserVerifyDeleteStmt(stmt->cmpnt.del, ctx, qctx, err);
        case STMTTYPE_FOR:
            return ParserVerifyForStmt(stmt->cmpnt.forstmt, ctx, qctx, err);
        case STMTTYPE_IF:
            return ParserVerifyIfStmt(stmt->cmpnt.ifstmt, ctx, qctx, err);
        case STMTTYPE_IMPORT:
            return ParserVerifyImportStmt(stmt->cmpnt.import, ctx, qctx, err);
        case STMTTYPE_RETURN:
            return ParserVerifyReturnStmt(stmt->cmpnt.ret, ctx, qctx, err);
        case STMTTYPE_DECLARATION:
            return ParserVerifyDeclaration(stmt->cmpnt.decl, ctx, qctx, err);
        case STMTTYPE_ASSIGNMENT:
            return ParserVerifyAssignment(stmt->cmpnt.assign, ctx, qctx, err);
        case STMTTYPE_EXPRESSION:
            if (ParserEnqueueExpression(stmt->cmpnt.expr, ctx, qctx) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            return ParserVerifyExpression(stmt->cmpnt.expr, ctx, qctx, err);
        case STMTTYPE_EMPTY:
            assert(false);
            VerifierErrorSet(err, ERR_INVALID_STATEMENT_TYPE);
            return MS_RESULT_ERROR;
    }

    return MS_RESULT_ERROR;
}

static ms_Result ParserVerifyBreakStmt(const ms_StmtBreak *brk, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(!brk);
    assert(ctx);
    assert(qctx);
    assert(err);

    if (!VerifierInConstrainedContext(ctx, ASTCTX_FUNCTION, ASTCTX_FORSTMT)) {
        VerifierErrorSet(err, ERR_BREAK_OUTSIDE_FOR);
        return MS_RESULT_ERROR;
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyContinueStmt(const ms_StmtContinue *cont, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(!cont);
    assert(ctx);
    assert(qctx);
    assert(err);

    if (!VerifierInConstrainedContext(ctx, ASTCTX_FUNCTION, ASTCTX_FORSTMT)) {
        VerifierErrorSet(err, ERR_CONTINUE_OUTSIDE_FOR);
        return MS_RESULT_ERROR;
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyDeleteStmt(const ms_StmtDelete *del, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(del);
    assert(ctx);
    assert(qctx);
    assert(err);

    return ParserVerifyExpression(del->expr, ctx, qctx, err);
}

static ms_Result ParserVerifyForStmt(const ms_StmtFor *forstmt, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(forstmt);
    assert(ctx);
    assert(qctx);
    assert(err);

    ASTContext *blockctx;
    if (ParserEnqueueBlockWithChild(forstmt->block, ctx, ASTCTX_FORSTMT, qctx, &blockctx) == MS_RESULT_ERROR) {
        return MS_RESULT_ERROR;
    }

    switch (forstmt->type) {
        case FORSTMT_INCREMENT:
            if (ParserVerifyForIncrement(forstmt->clause.inc, ctx, qctx, blockctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            break;
        case FORSTMT_ITERATOR:
            if (ParserVerifyForIterator(forstmt->clause.iter, ctx, qctx, blockctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            break;
        case FORSTMT_EXPR:
            if (ParserVerifyExpression(forstmt->clause.expr->expr, ctx, qctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            break;
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyForIncrement(const ms_StmtForIncrement *inc, ASTContext *ctx, QueueContext *qctx, ASTContext *childctx, ms_Error **err) {
    assert(inc);
    assert(ctx);
    assert(qctx);
    assert(err);

    if (inc->declare) {
        if (ms_ExprGetIdentType(inc->ident) != EXPRIDENT_NAME) {
            VerifierErrorSet(err, ERR_BAD_IDENT_IN_DECLARATION);
            return MS_RESULT_ERROR;
        }

        assert(inc->ident->type == EXPRTYPE_UNARY);
        assert(inc->ident->cmpnt.u);
        assert(inc->ident->cmpnt.u->type == EXPRATOM_IDENT);
        assert(inc->ident->cmpnt.u->atom.ident);
        dsdict_put(childctx->symbols, inc->ident->cmpnt.u->atom.ident->name, &VERIFIER_SYMBOL_VAL);
    } else {
        if (ParserVerifyExpression(inc->ident, ctx, qctx, err) == MS_RESULT_ERROR) {
            return MS_RESULT_ERROR;
        }
    }

    if (ParserVerifyExpression(inc->init, ctx, qctx, err) == MS_RESULT_ERROR) {
        return MS_RESULT_ERROR;
    }

    if (ParserVerifyExpression(inc->end, ctx, qctx, err) == MS_RESULT_ERROR) {
        return MS_RESULT_ERROR;
    }

    if (!inc->step) {
        return MS_RESULT_SUCCESS;
    }

    if (ParserVerifyExpression(inc->end, ctx, qctx, err) == MS_RESULT_ERROR) {
        return MS_RESULT_ERROR;
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyForIterator(const ms_StmtForIterator *iter, ASTContext *ctx, QueueContext *qctx, ASTContext *childctx, ms_Error **err) {
    assert(iter);
    assert(ctx);
    assert(qctx);
    assert(err);

    if (iter->declare) {
        if (ms_ExprGetIdentType(iter->ident) != EXPRIDENT_NAME) {
            VerifierErrorSet(err, ERR_BAD_IDENT_IN_DECLARATION);
            return MS_RESULT_ERROR;
        }

        assert(iter->ident->type == EXPRTYPE_UNARY);
        assert(iter->ident->cmpnt.u);
        assert(iter->ident->cmpnt.u->type == EXPRATOM_IDENT);
        assert(iter->ident->cmpnt.u->atom.ident);
        dsdict_put(childctx->symbols, iter->ident->cmpnt.u->atom.ident->name, &VERIFIER_SYMBOL_VAL);
    } else {
        if (ParserVerifyExpression(iter->ident, ctx, qctx, err) == MS_RESULT_ERROR) {
            return MS_RESULT_ERROR;
        }
    }

    if (ParserVerifyExpression(iter->iter, ctx, qctx, err) == MS_RESULT_ERROR) {
        return MS_RESULT_ERROR;
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyIfStmt(const ms_StmtIf *ifstmt, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(ifstmt);
    assert(ctx);
    assert(qctx);
    assert(err);

    if (ParserVerifyExpression(ifstmt->expr, ctx, qctx, err) == MS_RESULT_ERROR) {
        return MS_RESULT_ERROR;
    }

    if (ParserEnqueueIfStmt(ifstmt, ctx, qctx) == MS_RESULT_ERROR) {
        return MS_RESULT_ERROR;
    }

    return (ifstmt->elif) ?
           ParserVerifyElseIfStmt(ifstmt->elif, ctx, qctx, err) :
           MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyElseIfStmt(const ms_StmtIfElse *elif, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(elif);
    assert(ctx);
    assert(qctx);
    assert(err);

    switch (elif->type) {
        case IFELSE_IF:
            return ParserVerifyIfStmt(elif->clause.ifstmt, ctx, qctx, err);
        case IFELSE_ELSE:
            /* enqueued by ParserEnqueueIfStmt */
            break;
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyImportStmt(const ms_StmtImport *import, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(import);
    assert(ctx);
    assert(qctx);
    assert(err);

    if (import->alias) {
        if (VerifierSymbolExistsInCurrentScope(ctx, import->alias->name)) {
            VerifierErrorSet(err, ERR_REDECLARATION_IN_IMPORT, dsbuf_char_ptr(import->alias->name));
            return MS_RESULT_ERROR;
        }
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyReturnStmt(const ms_StmtReturn *ret, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(ret);
    assert(ctx);
    assert(qctx);
    assert(err);

    if (!VerifierInContext(ctx, ASTCTX_FUNCTION)) {
        VerifierErrorSet(err, ERR_RETURN_OUTSIDE_FUNC);
        return MS_RESULT_ERROR;
    }

    return ParserVerifyExpression(ret->expr, ctx, qctx, err);
}

static ms_Result ParserVerifyDeclaration(const ms_StmtDeclaration *decl, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(decl);
    assert(ctx);
    assert(qctx);
    assert(err);

    if (decl->ident->type != IDENT_NAME) {
        VerifierErrorSet(err, ERR_BAD_IDENT_IN_DECLARATION);
        return MS_RESULT_ERROR;
    }

    if (VerifierSymbolExistsInCurrentScope(ctx, decl->ident->name)) {
        VerifierErrorSet(err, ERR_VAR_REDECLARATION, dsbuf_char_ptr(decl->ident->name));
        return MS_RESULT_ERROR;
    }

    /* issue a warning if declaration merely shadows a name from an enclosing scope */
    if (VerifierSymbolExistsInLexicalScope(ctx, decl->ident->name)) {
        /* TODO: add warning to err object */
        return MS_RESULT_WARNINGS;
    }

    dsdict_put(ctx->symbols, decl->ident->name, &VERIFIER_SYMBOL_VAL);

    if ((decl->expr) &&
        (ParserVerifyExpression(decl->expr, ctx, qctx, err) == MS_RESULT_ERROR)) {
        return MS_RESULT_ERROR;
    }

    return (decl->next) ?
           ParserVerifyDeclaration(decl->next, ctx, qctx, err) :
           MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyAssignment(const ms_StmtAssignment *assign, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(assign);
    assert(ctx);
    assert(qctx);
    assert(err);

    size_t ntargets = 0;
    ms_StmtAssignTarget *target = assign->ident;
    while (target) {
        if (ParserVerifyExpression(target->target, ctx, qctx, err) == MS_RESULT_ERROR) {
            return MS_RESULT_ERROR;
        }
        ntargets += 1;
        target = target->next;
    }

    size_t nexprs = 0;
    ms_StmtAssignExpr *expr = assign->expr;
    while (expr) {
        if (ParserVerifyExpression(expr->expr, ctx, qctx, err) == MS_RESULT_ERROR) {
            return MS_RESULT_ERROR;
        }
        nexprs += 1;
        expr = expr->next;
    }

    if (ntargets != nexprs) {
        VerifierErrorSet(err, ERR_ASSIGNMENT_COUNT_MISMATCH);
        return MS_RESULT_ERROR;
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyExpression(const ms_Expr *expr, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(expr);
    assert(ctx);
    assert(qctx);
    assert(err);

    switch (expr->type) {
        case EXPRTYPE_UNARY:
            if (ParserVerifyExprAtom(&expr->cmpnt.u->atom, expr->cmpnt.u->type, ctx, qctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            break;
        case EXPRTYPE_BINARY:
            if (ParserVerifyExprAtom(&expr->cmpnt.b->latom, expr->cmpnt.b->ltype, ctx, qctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            if (ParserVerifyExprAtom(&expr->cmpnt.b->ratom, expr->cmpnt.b->rtype, ctx, qctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            break;
        case EXPRTYPE_CONDITIONAL:
            if (ParserVerifyExprAtom(&expr->cmpnt.c->cond, expr->cmpnt.c->condtype, ctx, qctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            if (ParserVerifyExprAtom(&expr->cmpnt.c->iftrue, expr->cmpnt.c->truetype, ctx, qctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            if (ParserVerifyExprAtom(&expr->cmpnt.c->iffalse, expr->cmpnt.c->falsetype, ctx, qctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            break;
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyExprAtom(const ms_ExprAtom *atom, ms_ExprAtomType type, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(atom);
    assert(ctx);
    assert(qctx);
    assert(err);

    switch (type) {
        case EXPRATOM_VALUE:
            if (ParserVerifyExprAtomValue(&atom->val, ctx, qctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            break;
        case EXPRATOM_EXPRESSION:
            if (ParserVerifyExpression(atom->expr, ctx, qctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            break;
        case EXPRATOM_IDENT:
            if (ParserVerifyIdent(atom->ident, ctx, qctx, err) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }
            break;
        case EXPRATOM_EXPRLIST: {
            size_t len = dsarray_len(atom->list);
            for (size_t i = 0; i < len; i++) {
                ms_Expr *elem = dsarray_get(atom->list, i);
                if (ParserVerifyExpression(elem, ctx, qctx, err) == MS_RESULT_ERROR) {
                    return MS_RESULT_ERROR;
                }
            }
            break;
        }
        case EXPRATOM_EMPTY:
            assert(false);
            VerifierErrorSet(err, ERR_EMPTY_EXPR_ATOM);
            return MS_RESULT_ERROR;
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyExprAtomValue(const ms_Value *val, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(val);
    assert(ctx);
    assert(qctx);
    assert(err);

    switch (val->type) {
        case MSVAL_FLOAT:       /* fall through */
        case MSVAL_INT:         /* fall through */
        case MSVAL_BOOL:        /* fall through */
        case MSVAL_STR:         /* fall through */
        case MSVAL_NULL:
            break;
        case MSVAL_ARRAY: {
            size_t len = dsarray_len(val->val.a);
            for (size_t i = 0; i < len; i++) {
                ms_Expr *elem = dsarray_get(val->val.a, i);
                if (ParserVerifyExpression(elem, ctx, qctx, err) == MS_RESULT_ERROR) {
                    return MS_RESULT_ERROR;
                }
            }
            break;
        }
        case MSVAL_OBJECT: {
            size_t len = dsarray_len(val->val.o);
            for (size_t i = 0; i < len; i++) {
                ms_ValObjectTuple *tuple = dsarray_get(val->val.o, i);
                if (ParserVerifyExpression(tuple->key, ctx, qctx, err) == MS_RESULT_ERROR) {
                    return MS_RESULT_ERROR;
                }
                if (ParserVerifyExpression(tuple->val, ctx, qctx, err) == MS_RESULT_ERROR) {
                    return MS_RESULT_ERROR;
                }
            }
            break;
        }
        case MSVAL_FUNC: {
            ASTContext *blockctx;
            if (ParserEnqueueBlockWithChild(val->val.fn->block, ctx, ASTCTX_FUNCTION, qctx, &blockctx) == MS_RESULT_ERROR) {
                return MS_RESULT_ERROR;
            }

            size_t nargs = dsarray_len(val->val.fn->args);
            for (size_t i = 0; i < nargs; i++) {
                ms_Ident *arg = dsarray_get(val->val.fn->args, i);
                dsdict_put(blockctx->symbols, arg->name, &VERIFIER_SYMBOL_VAL);
            }

            return MS_RESULT_SUCCESS;
        }
    }

    return MS_RESULT_SUCCESS;
}

static ms_Result ParserVerifyIdent(const ms_Ident *ident, ASTContext *ctx, QueueContext *qctx, ms_Error **err) {
    assert(ident);
    assert(ctx);
    assert(qctx);
    assert(err);

    if ((ident->type == IDENT_GLOBAL) || (ident->type == IDENT_BUILTIN)) {
        return MS_RESULT_SUCCESS;
    }

    if (!VerifierSymbolExistsInLexicalScope(ctx, ident->name)) {
        VerifierErrorSet(err, ERR_REFERENCE_TO_UNDEFINED, dsbuf_char_ptr(ident->name));
        return MS_RESULT_ERROR;
    }

    dsdict_put(ctx->symbols, ident->name, &VERIFIER_SYMBOL_VAL);

    return MS_RESULT_SUCCESS;
}

/*
 * UTILITY FUNCTIONS
 */

static inline bool VerifierInContext(ASTContext *ctx, ASTElementContextType type) {
    assert(ctx);

    ASTContext *cur = ctx;
    while (cur) {
        if (cur->type == type) {
            return true;
        }
        cur = cur->parent;
    }

    return false;
}

static inline bool VerifierInConstrainedContext(ASTContext *ctx, ASTElementContextType ancestor, ASTElementContextType type) {
    assert(ctx);

    /*
     * We want to assert that an AST element appears in a context which
     * does does _not_ cross the boundaries of another context. In the
     * example which precipated creating this function, that constrained
     * context is that `break` and `continue` statements must appear in
     * loops, but they may not appear _alone_ inside of a function definition
     * block within a loop, since that would invoke undefined behavior
     * upon executing that function body outside of the loop context.
     *
     * e.g. this code would be invalid
     *
     *     var index;
     *     for index := 1 : 10 {
     *         var f := func() {
     *             continue;
     *         };
     *     }
     *
     * ...but this code would not:
     *
     *     var index;
     *     for index := 1 : 10 {
     *         var f := func() {
     *             var i;
     *             for i := index : 10 {
     *                 continue;
     *             }
     *         };
     *     }
     */

    bool found_ancestor_context = false;
    ASTContext *cur = ctx;
    while (cur) {
        if (cur->type == ancestor) {
            found_ancestor_context = true;
        }
        if (cur->type == type) {
            if (found_ancestor_context) {
                return false;
            }
            return true;
        }
        cur = cur->parent;
    }

    return false;
}

static inline bool VerifierSymbolExistsInCurrentScope(ASTContext *ctx, DSBuffer *buf) {
    assert(ctx);
    assert(buf);
    return dsdict_get(ctx->symbols, buf) != NULL;
}

static inline bool VerifierSymbolExistsInLexicalScope(ASTContext *ctx, DSBuffer *buf) {
    assert(ctx);
    assert(buf);

    ASTContext *cur = ctx;
    while (cur) {
        if (VerifierSymbolExistsInCurrentScope(cur, buf)) {
            return true;
        }
        cur = cur->parent;
    }

    return false;
}

static void VerifierErrorSet(ms_Error **err, const char *msg, ...) {
    assert(err);

    *err = malloc(sizeof(ms_Error));
    if (!(*err)) {
        return;
    }
    (*err)->type = MS_ERROR_VERIFIER;

    va_list args;
    va_list argscpy;
    va_start(args, msg);
    va_copy(argscpy, args);

    int len = vsnprintf(NULL, 0, msg, args);
    if (len < 0) {
        goto verifier_close_error_va_args;
    }

    (*err)->len = (size_t)len;
    (*err)->msg = malloc((size_t)len + 1);
    if ((*err)->msg) {
        vsnprintf((*err)->msg, len + 1, msg, argscpy);
    }

verifier_close_error_va_args:
    va_end(args);
    va_end(argscpy);
    return;
}
