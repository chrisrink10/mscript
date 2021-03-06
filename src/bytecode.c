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
#include <stdbool.h>
#include <stdio.h>
#include "bytecode.h"
#include "libds/dict.h"
#include "vm.h"
#include "lang.h"

#define OPC_BITS                ((sizeof(int) / 2) * 8)
#define OPC_ARG_MAX             ((1 << OPC_BITS) - 1)

#define BLOCK_NO_PUSH_OR_POP    (0)
#define BLOCK_INCL_PUSH         (1)
#define BLOCK_INCL_POP          (2)

typedef struct {
    DSArray *opcodes;
    DSArray *values;
    DSArray *idents;
    DSDict *ident_cache;        /** cache of previously used identifier names */
    ms_Result res;              /** indicates if any errors or warnings occurred */
    ms_Error **err;             /** error details if an error occurred */
} CodeGenContext;

typedef struct {
    CodeGenContext *parent;
    int push_or_pop;            /** bitmask for whether block should be pushed or popped */
} CodeGenContextBlock;

typedef struct {
    CodeGenContext *parent;
    size_t start;               /** index of the first block statement */
    size_t end;                 /** index of the last block statement */
    int break_arg;              /** argument to place on a BREAK opcode */
    int cont_arg;               /** argument to place on a CONTINUE opcode */
} CodeGenContextFor;

typedef struct {
    CodeGenContext *parent;
    size_t attrcount;           /** count of attributes seen in a single expr */
    size_t attrdepth;           /** depth of attributes in current stack frame */
    bool suppress_get_attr;     /** suppress GET_ATTR opcodes */
    bool get_name_as_push;      /** convert GET_NAME opcodes to PUSH */
} CodeGenContextExpr;

static const int EXPR_OPCODE_STACK_LEN = 50;
static const int EXPR_VALUE_STACK_LEN = 50;
static const int EXPR_IDENT_STACK_LEN = 50;

static char *OpCodeArgToString(const ms_VMByteCode *bc, size_t i);
static char *ByteCodeValueToString(const ms_VMByteCode *bc, size_t i);
static char *ByteCodeIdentToString(const ms_VMByteCode *bc, size_t i);
static char *ByteCodeArgToString(const ms_VMByteCode *bc, int arg);
static bool CodeGenContextCreate(CodeGenContext *ctx);
static void CodeGenContextClean(CodeGenContext *ctx);
static ms_VMByteCode *VMByteCodeNew(const CodeGenContext *ctx);
static void VMValueClean(ms_VMValue *v);
static void BlockToOpCodes(const ms_StmtBlock *blk, CodeGenContextBlock *ctx);
static void StmtToOpCodes(const ms_Stmt *stmt, CodeGenContext *ctx);
static void StmtDeleteToOpCodes(const ms_StmtDelete *del, CodeGenContext *ctx);
static void StmtForToOpCodes(const ms_StmtFor *forstmt, CodeGenContext *ctx);
static void StmtForIncToOpCodes(const ms_StmtFor *forstmt, CodeGenContext *ctx);
static void StmtForIterToOpCodes(const ms_StmtFor *forstmt, CodeGenContext *ctx);
static void StmtForExprToOpCodes(const ms_StmtFor *forstmt, CodeGenContext *ctx);
static void StmtForFixBreakAndContinue(CodeGenContextFor *ctx);
static void StmtIfToOpCodes(const ms_StmtIf *ifstmt, CodeGenContext *ctx);
static void StmtElseIfToOpCodes(const ms_StmtIfElse *elif, CodeGenContext *ctx);
static void StmtImportToOpCodes(const ms_StmtImport *import, CodeGenContext *ctx);
static void StmtReturnToOpCodes(const ms_StmtReturn *ret, CodeGenContext *ctx);
static void StmtAssignmentToOpCodes(const ms_StmtAssignment *assign, CodeGenContext *ctx);
static void StmtAssignmentTargetToOpCodes(const ms_StmtAssignTarget *target, CodeGenContext *ctx);
static void StmtDeclarationToOpCodes(const ms_StmtDeclaration *decl, CodeGenContext *ctx);
static void IdentSetToOpCodes(const ms_Expr *ident, CodeGenContext *ctx, bool new_name);
static void GlobalIdentExprToOpCodes(const ms_Expr *ident, int *nsubscripts, CodeGenContext *ctx);
static void QualifiedIdentExprToOpCodes(const ms_Expr *ident, CodeGenContext *ctx);
static void IdentExprToOpCodes(const ms_Expr *ident, int *index, CodeGenContext *ctx);
static void ExprToOpCodes(const ms_Expr *expr, CodeGenContext *ctx);
static void ExprToOpCodesInner(const ms_Expr *expr, CodeGenContextExpr *ctx);
static void ExprUnaryToOpCodes(const ms_ExprUnary *u, CodeGenContextExpr *ctx);
static void ExprBinaryToOpCodes(const ms_ExprBinary *b, CodeGenContextExpr *ctx);
static void ExprSafeGetAttrToOpCodes(const ms_ExprBinary *b, CodeGenContextExpr *ctx);
static void ExprConditionalToOpCodes(const ms_ExprConditional *c, CodeGenContextExpr *ctx);
static void ExprComponentToOpCodes(const ms_ExprAtom *a, ms_ExprAtomType type, CodeGenContextExpr *ctx);
static void ExprComponentValueToOpCodes(const ms_Value *val, int index_or_len, CodeGenContext *ctx);
static void ExprUnaryOpToOpCode(ms_ExprUnaryOp op, CodeGenContext *ctx);
static void ExprBinaryOpToOpCode(ms_ExprBinaryOp op, CodeGenContext *ctx);
static void ExprBinaryAttrListToOpCode(const ms_ExprBinary *b, CodeGenContextExpr *ctx);
static ms_VMFunc *ExprFunctionExprToOpCodes(const ms_ValFunc *fn, CodeGenContext *ctx);
static ms_ExprIdentType ExprAtomGetIdentType(const ms_ExprAtom *atom, ms_ExprAtomType type);
static void PushValue(const ms_Value *val, int *index_or_len, CodeGenContext *ctx);
static void PushIdent(const ms_Ident *ident, int *index, CodeGenContext *ctx);
static void PushOpCode(ms_VMOpCodeType type, int arg, CodeGenContext *ctx);
static void CodeGenContextErrorSet(CodeGenContext *ctx, const char *msg);

/*
 * PUBLIC FUNCTIONS
 */

ms_Result ms_VMByteCodeGenerateFromAST(const ms_AST *ast, ms_VMByteCode **code, ms_Error **err) {
    if (!ast) {
        return MS_RESULT_ERROR;
    }

    *err = NULL;
    CodeGenContext ctx = { .err = err, .res = MS_RESULT_SUCCESS };
    if (!CodeGenContextCreate(&ctx)) {
        return MS_RESULT_ERROR;
    }

    size_t len = dsarray_len(ast);
    for (size_t i = 0; i < len; i++) {
        ms_Stmt *stmt = dsarray_get(ast, i);
        StmtToOpCodes(stmt, &ctx);
    }

    *code = VMByteCodeNew(&ctx);
    CodeGenContextClean(&ctx);
    return ctx.res;
}

void ms_VMByteCodeDestroy(ms_VMByteCode *bc) {
    if (!bc) { return; }
    free(bc->code);
    bc->code = NULL;
    for (size_t i = 0; i < bc->nvals; i++) {
        VMValueClean(&bc->values[i]);
    }
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

void ms_VMByteCodePrint(const ms_VMByteCode *bc) {
    assert(bc);
    FILE *outfile = stdout;
    for (size_t i = 0; i < bc->nops; i++) {
        ms_VMOpCode opc = bc->code[i];
        const char *name = ms_VMOpCodeToString(opc);
        char *strarg = OpCodeArgToString(bc, i);
        fprintf(outfile, "%5zu %-20s %s\n", i, name, (strarg) ? (strarg) : "");
    }
}

void ms_VMValueDestroy(ms_VMValue *v) {
    if (!v) { return; }
    VMValueClean(v);
    free(v);
}

ms_VMOpCode ms_VMOpCodeWithArg(ms_VMOpCodeType c, int arg) {
    return (c | (arg << OPC_BITS));
}

int ms_VMOpCodeGetArg(ms_VMOpCode c) {
    return (c >> OPC_BITS);
}

ms_VMOpCodeType ms_VMOpCodeGetCode(ms_VMOpCode c) {
    return (ms_VMOpCodeType)(c & ((1 << OPC_BITS) - 1));
}

const char *ms_VMOpCodeToString(ms_VMOpCode c) {
    ms_VMOpCodeType type = ms_VMOpCodeGetCode(c);
    switch (type) {
        case OPC_PRINT:             return "PRINT";
        case OPC_PUSH:              return "PUSH";
        case OPC_POP:               return "POP";
        case OPC_SWAP:              return "SWAP";
        case OPC_DUP:               return "DUP";
        case OPC_ADD:               return "ADD";
        case OPC_SUBTRACT:          return "SUBTRACT";
        case OPC_MULTIPLY:          return "MULTIPLY";
        case OPC_DIVIDE:            return "DIVIDE";
        case OPC_IDIVIDE:           return "IDIVIDE";
        case OPC_MODULO:            return "MODULO";
        case OPC_EXPONENTIATE:      return "EXPONENTIATE";
        case OPC_NEGATE:            return "NEGATE";
        case OPC_SHIFT_LEFT:        return "SHIFT_LEFT";
        case OPC_SHIFT_RIGHT:       return "SHIFT_RIGHT";
        case OPC_BITWISE_AND:       return "BITWISE_AND";
        case OPC_BITWISE_XOR:       return "BITWISE_XOR";
        case OPC_BITWISE_OR:        return "BITWISE_OR";
        case OPC_BITWISE_NOT:       return "BITWISE_NOT";
        case OPC_LE:                return "LE";
        case OPC_LT:                return "LT";
        case OPC_GE:                return "GE";
        case OPC_GT:                return "GT";
        case OPC_EQ:                return "EQ";
        case OPC_NOT_EQ:            return "NOT_EQ";
        case OPC_NOT:               return "NOT";
        case OPC_AND:               return "AND";
        case OPC_OR:                return "OR";
        case OPC_CALL:              return "CALL";
        case OPC_CALL_BUILTIN:      return "CALL_BUILTIN";
        case OPC_PUSH_BLOCK:        return "PUSH_BLOCK";
        case OPC_POP_BLOCK:         return "POP_BLOCK";
        case OPC_RETURN:            return "RETURN";
        case OPC_GET_ATTR:          return "GET_ATTR";
        case OPC_SET_ATTR:          return "SET_ATTR";
        case OPC_DEL_ATTR:          return "DEL_ATTR";
        case OPC_GET_GLO:           return "GET_GLO";
        case OPC_SET_GLO:           return "SET_GLO";
        case OPC_DEL_GLO:           return "DEL_GLO";
        case OPC_NEW_NAME:          return "NEW_NAME";
        case OPC_GET_NAME:          return "GET_NAME";
        case OPC_SET_NAME:          return "SET_NAME";
        case OPC_DEL_NAME:          return "DEL_NAME";
        case OPC_MAKE_LIST:         return "MAKE_LIST";
        case OPC_MAKE_OBJ:          return "MAKE_OBJ";
        case OPC_NEXT:              return "NEXT";
        case OPC_IMPORT:            return "IMPORT";
        case OPC_JUMP_IF_FALSE:     return "JUMP_IF_FALSE";
        case OPC_GOTO:              return "GOTO";
        case OPC_BREAK:             return "BREAK";
        case OPC_CONTINUE:          return "CONTINUE";
        default:
            assert(false && "invalid opcode given");
            return "invalidopc";
    }
}

/*
 * PRIVATE FUNCTIONS
 */

static bool CodeGenContextCreate(CodeGenContext *ctx) {
    assert(ctx);

    ctx->opcodes = dsarray_new_cap(EXPR_OPCODE_STACK_LEN, NULL,
                                   (dsarray_free_fn)free);
    if (!ctx->opcodes) {
        return false;
    }

    ctx->values = dsarray_new_cap(EXPR_VALUE_STACK_LEN, NULL,
                                  (dsarray_free_fn)ms_VMValueDestroy);
    if (!ctx->values) {
        dsarray_destroy(ctx->opcodes);
        return false;
    }

    /* no dsarray_free_fn required since we pass the pointer to the bytecode */
    ctx->idents = dsarray_new_cap(EXPR_IDENT_STACK_LEN, NULL, NULL);
    if (!ctx->idents) {
        dsarray_destroy(ctx->opcodes);
        dsarray_destroy(ctx->values);
        return false;
    }

    ctx->ident_cache = dsdict_new((dsdict_hash_fn)dsbuf_hash,
                                  (dsdict_compare_fn)dsbuf_compare,
                                  NULL, (dsdict_free_fn)free);
    if (!ctx->ident_cache) {
        dsarray_destroy(ctx->opcodes);
        dsarray_destroy(ctx->values);
        dsarray_destroy(ctx->idents);
        return false;
    }

    return true;
}

static void CodeGenContextClean(CodeGenContext *ctx) {
    dsarray_destroy(ctx->opcodes);
    dsarray_destroy(ctx->values);
    dsarray_destroy(ctx->idents);
    dsdict_destroy(ctx->ident_cache);
}

static ms_VMByteCode *VMByteCodeNew(const CodeGenContext *ctx) {
    if (!ctx) { return NULL; }

    ms_VMByteCode *bc = malloc(sizeof(ms_VMByteCode));
    if (!bc) {
        return NULL;
    }

    bc->nops = dsarray_len(ctx->opcodes);
    bc->code = malloc(sizeof(ms_VMOpCode) * (bc->nops));
    if (!bc->code) {
        free(bc);
        return NULL;
    }

    bc->nvals = dsarray_len(ctx->values);
    bc->values = malloc(sizeof(ms_VMValue) * (bc->nvals));
    if (!bc->values) {
        free(bc->code);
        free(bc);
        return NULL;
    }

    bc->nidents = dsarray_len(ctx->idents);
    bc->idents = malloc(sizeof(ms_Ident *) * (bc->nidents));
    if (!bc->idents) {
        free(bc->values);
        free(bc->code);
        free(bc);
        return NULL;
    }

    for (size_t i = 0; i < bc->nops; i++) {
        bc->code[i] = *(ms_VMOpCode *)dsarray_get(ctx->opcodes, i);
    }

    for (size_t i = 0; i < bc->nvals; i++) {
        ms_VMValue *v = dsarray_get(ctx->values, i);
        bc->values[i] = *v;

        /* clear the pointers in the old value so they aren't deallocated
         * when the value stack is destroyed */
        if (v->type == VMVAL_STR) {
            v->val.s = NULL;
        } else if (v->type == VMVAL_FUNC) {
            v->val.fn = NULL;
        }
    }

    for (size_t i = 0; i < bc->nidents; i++) {
        bc->idents[i] = dsarray_get(ctx->idents, i);
    }

    return bc;
}

static void VMValueClean(ms_VMValue *v) {
    assert(v);

    switch(v->type) {
        case VMVAL_STR:
            if (v->val.s) {
                dsbuf_destroy(v->val.s);
                v->val.s = NULL;
            }
            break;
        case VMVAL_FUNC:
            if (v->val.fn) {
                dsarray_destroy(v->val.fn->args);
                v->val.fn->args = NULL;
                ms_VMByteCodeDestroy(v->val.fn->code);
                v->val.fn->code = NULL;
                free(v->val.fn);
                v->val.fn = NULL;
            }
            break;
        case VMVAL_INT:         /* fall through */
        case VMVAL_FLOAT:       /* fall through */
        case VMVAL_BOOL:        /* fall through */
        case VMVAL_NULL:
            break;
    }
}

static char *OpCodeArgToString(const ms_VMByteCode *bc, size_t i) {
    assert(bc);

    ms_VMOpCodeType type = ms_VMOpCodeGetCode(bc->code[i]);
    int arg = ms_VMOpCodeGetArg(bc->code[i]);
    switch (type) {
        case OPC_PUSH:              return ByteCodeValueToString(bc, (size_t)arg);
        case OPC_CALL:              return ByteCodeArgToString(bc, arg);
        case OPC_CALL_BUILTIN:      return ByteCodeArgToString(bc, arg);
        case OPC_GET_ATTR:          return ByteCodeArgToString(bc, arg);
        case OPC_SET_ATTR:          return ByteCodeArgToString(bc, arg);
        case OPC_DEL_ATTR:          return ByteCodeArgToString(bc, arg);
        case OPC_GET_GLO:           return ByteCodeArgToString(bc, arg);
        case OPC_SET_GLO:           return ByteCodeArgToString(bc, arg);
        case OPC_DEL_GLO:           return ByteCodeArgToString(bc, arg);
        case OPC_NEW_NAME:          return ByteCodeIdentToString(bc, (size_t)arg);
        case OPC_GET_NAME:          return ByteCodeIdentToString(bc, (size_t)arg);
        case OPC_SET_NAME:          return ByteCodeIdentToString(bc, (size_t)arg);
        case OPC_DEL_NAME:          return ByteCodeIdentToString(bc, (size_t)arg);
        case OPC_MAKE_LIST:         return ByteCodeArgToString(bc, arg);
        case OPC_MAKE_OBJ:          return ByteCodeArgToString(bc, arg);
        case OPC_IMPORT:            return ByteCodeArgToString(bc, arg);
        case OPC_JUMP_IF_FALSE:     return ByteCodeArgToString(bc, arg);
        case OPC_GOTO:              return ByteCodeArgToString(bc, arg);
        case OPC_BREAK:             return ByteCodeArgToString(bc, arg);
        case OPC_CONTINUE:          return ByteCodeArgToString(bc, arg);
        default:                    return NULL;
    }
}

static char *ByteCodeValueToString(const ms_VMByteCode *bc, size_t i) {
    assert(bc);
    char *buf = NULL;
    size_t len = 0;
    ms_VMValue *v = &bc->values[i];

gen_val_string:
    switch(v->type) {
        case VMVAL_BOOL:
            len = snprintf(buf, len, "%s\n", v->val.b ? "true" : "false");
            break;
        case VMVAL_NULL:
            len = snprintf(buf, len, "%s\n", "null");
            break;
        case VMVAL_FLOAT:
            len = snprintf(buf, len, "%f\n", v->val.f);
            break;
        case VMVAL_INT:
            len = snprintf(buf, len, "%lld\n", v->val.i);
            break;
        case VMVAL_STR:
            len = snprintf(buf, len, "\"%s\"\n", dsbuf_char_ptr(v->val.s));
            break;
        case VMVAL_FUNC:
            len = snprintf(buf, len, "<func %p>", (void *)v->val.fn);
            break;
    }

    if (buf) {
        return buf;
    }

    buf = malloc(len + 1);
    if (!buf) {
        return NULL;
    }

    goto gen_val_string;
}

static char *ByteCodeIdentToString(const ms_VMByteCode *bc, size_t i) {
    assert(bc);
    return dsbuf_to_char_array(bc->idents[i]);
}

static char *ByteCodeArgToString(const ms_VMByteCode *bc, int arg) {
    assert(bc);
    size_t len = snprintf(NULL, 0, "%d", arg);
    char *buf = malloc(len + 1);
    if (!buf) {
        return NULL;
    }

    snprintf(buf, len+1, "%d", arg);
    return buf;
}

static void BlockToOpCodes(const ms_StmtBlock *blk, CodeGenContextBlock *ctx) {
    assert(blk);
    assert(ctx);
    assert(ctx->parent);

    if ((ctx->push_or_pop & BLOCK_INCL_PUSH) == BLOCK_INCL_PUSH) {
        PushOpCode(OPC_PUSH_BLOCK, 0, ctx->parent);
    }

    size_t len = dsarray_len(blk);
    for (size_t i = 0; i < len; i++) {
        ms_Stmt *stmt = dsarray_get(blk, i);
        StmtToOpCodes(stmt, ctx->parent);
    }

    if ((ctx->push_or_pop & BLOCK_INCL_POP) == BLOCK_INCL_POP) {
        PushOpCode(OPC_POP_BLOCK, 0, ctx->parent);
    }
}

static void StmtToOpCodes(const ms_Stmt *stmt, CodeGenContext *ctx) {
    assert(stmt);
    assert(ctx);

    switch (stmt->type) {
        case STMTTYPE_EMPTY:
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "should not have an empty statement");
            assert(false);
            break;
        case STMTTYPE_BREAK:
            PushOpCode(OPC_BREAK, 0, ctx);
            break;
        case STMTTYPE_CONTINUE:
            PushOpCode(OPC_CONTINUE, 0, ctx);
            break;
        case STMTTYPE_DELETE:
            StmtDeleteToOpCodes(stmt->cmpnt.del, ctx);
            break;
        case STMTTYPE_FOR:
            StmtForToOpCodes(stmt->cmpnt.forstmt, ctx);
            break;
        case STMTTYPE_IF:
            StmtIfToOpCodes(stmt->cmpnt.ifstmt, ctx);
            break;
        case STMTTYPE_IMPORT:
            StmtImportToOpCodes(stmt->cmpnt.import, ctx);
            break;
        case STMTTYPE_RETURN:
            StmtReturnToOpCodes(stmt->cmpnt.ret, ctx);
            break;
        case STMTTYPE_ASSIGNMENT:
            StmtAssignmentToOpCodes(stmt->cmpnt.assign, ctx);
            break;
        case STMTTYPE_DECLARATION:
            StmtDeclarationToOpCodes(stmt->cmpnt.decl, ctx);
            break;
        case STMTTYPE_EXPRESSION:
            ExprToOpCodes(stmt->cmpnt.expr, ctx);
            break;
    }
}

static void StmtDeleteToOpCodes(const ms_StmtDelete *del, CodeGenContext *ctx) {
    assert(del);
    assert(ctx);

    ms_ExprIdentType type = ms_ExprGetIdentType(del->expr);
    switch (type) {
        case EXPRIDENT_NONE:
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "delete expression is not an identifier");
            assert(false);
            break;
        case EXPRIDENT_BUILTIN:
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "cannot delete a builtin name");
            assert(false);
            break;
        case EXPRIDENT_NAME: {
            int index;
            IdentExprToOpCodes(del->expr, &index, ctx);
            PushOpCode(OPC_DEL_NAME, index, ctx);
            break;
        }
        case EXPRIDENT_GLOBAL: {
            int nsubscripts;
            GlobalIdentExprToOpCodes(del->expr, &nsubscripts, ctx);
            PushOpCode(OPC_DEL_GLO, nsubscripts, ctx);
            break;
        }
        case EXPRIDENT_QUALIFIED: {
            QualifiedIdentExprToOpCodes(del->expr, ctx);
            PushOpCode(OPC_DEL_ATTR, 0, ctx);
            break;
        }
    }
}

static void StmtForToOpCodes(const ms_StmtFor *forstmt, CodeGenContext *ctx) {
    assert(forstmt);
    assert(ctx);

    switch (forstmt->type) {
        case FORSTMT_INCREMENT:
            StmtForIncToOpCodes(forstmt, ctx);
            break;
        case FORSTMT_ITERATOR:
            StmtForIterToOpCodes(forstmt, ctx);
            break;
        case FORSTMT_EXPR:
            StmtForExprToOpCodes(forstmt, ctx);
            break;
    }
}

static void StmtForIncToOpCodes(const ms_StmtFor *forstmt, CodeGenContext *ctx) {
    assert(forstmt);
    assert(forstmt->type == FORSTMT_INCREMENT);
    assert(forstmt->clause.inc);
    assert(ctx);

    /***************************************************************************
     * FOR increment statements should end up looking like this in bytecode:
     *
     * index        instruction     arg
     * -----        -----------     ----
     * ...          PUSH_BLOCK                  <--- push a new block so everything is evaluated in that context
     * ...          (expr)          init        <--- initial value expression
     * ...          (expr)          ident       <--- identifier
     * ...          [NEW_NAME]      ident       <--- if the name was declared in the block, new it
     * ...          SET_NAME/ATTR   ident       <--- duplicate that value
     * i            LOAD_NAME       ident       <--- set that value to the name
     * ...          PUSH            end         <--- push the end value on the stack
     * ...          LT                          <--- compare current value and end value
     * j            IF              n           <--- if TOS evaluated to false, jump to end-of-block
     * ...          (block)
     * ...          LOAD_NAME       ident       <--- load back up our increment variable
     * ...          PUSH            step        <--- push the step value onto the stack
     * ...          ADD                         <--- add the step to the increment value
     * ...          DUP                         <--- duplicate the value on the stack (for the IF comparison)
     * ...          SET_NAME        ident       <--- save the incremented value
     * ...          GOTO            i           <--- go back to compare the value again
     * n            POP_BLOCK                   <--- always pop the block
     ***************************************************************************/

    const ms_StmtForIncrement *inc = forstmt->clause.inc;

    /* start a block context and push the initial expression */
    PushOpCode(OPC_PUSH_BLOCK, 0, ctx);
    ExprToOpCodes(inc->init, ctx);

    /* save the initial value to the loop identifier */
    bool should_new_name = inc->declare;
    IdentSetToOpCodes(inc->ident, ctx, should_new_name);

    /* load up the expression value and compare it to the end
     * to make sure we're still within the valid range */
    size_t i = dsarray_len(ctx->opcodes);
    ExprToOpCodes(inc->ident, ctx);
    ExprToOpCodes(inc->end, ctx);
    PushOpCode(OPC_LE, 0, ctx);
    size_t j = dsarray_len(ctx->opcodes);
    PushOpCode(OPC_JUMP_IF_FALSE, 0, ctx);

    /* load up the block */
    size_t start = dsarray_len(ctx->opcodes);
    CodeGenContextBlock blkctx = { .parent = ctx, .push_or_pop = BLOCK_NO_PUSH_OR_POP };
    BlockToOpCodes(forstmt->block, &blkctx);
    size_t end = dsarray_len(ctx->opcodes);

    /* reload the identifier, increment it, and go back */
    ExprToOpCodes(inc->ident, ctx);
    ExprToOpCodes(inc->step, ctx);
    PushOpCode(OPC_ADD, 0, ctx);
    IdentSetToOpCodes(inc->ident, ctx, false);

    /* go back to the comparison at the beginning of the loop */
    PushOpCode(OPC_GOTO, (int)i, ctx);
    PushOpCode(OPC_POP_BLOCK, 0, ctx);

    /* update the conditional instruction with the index of the POP_BLOCK instruction */
    size_t pop = dsarray_len(ctx->opcodes) - 1;
    ms_VMOpCode *opcif = dsarray_get(ctx->opcodes, j);
    *opcif = ms_VMOpCodeWithArg(OPC_JUMP_IF_FALSE, (int)pop);

    /* update the BREAK and CONTINUE opcodes with the correct gotos */
    CodeGenContextFor ctxfor = { ctx, start, end, (int)pop, (int)i };
    StmtForFixBreakAndContinue(&ctxfor);
}

static void StmtForIterToOpCodes(const ms_StmtFor *forstmt, CodeGenContext *ctx) {
    assert(forstmt);
    assert(forstmt->type == FORSTMT_ITERATOR);
    assert(forstmt->clause.iter);
    assert(ctx);

    /***************************************************************************
     * FOR iterator statements should end up looking like this in bytecode:
     *
     * index        instruction     arg
     * -----        -----------     ----
     * ...          PUSH_BLOCK                  <--- push a new block so everything is evaluated in that context
     * ...          [NEW_NAME]      ident       <--- if the name was declared in the block, new it
     * i            (expr)          iter        <--- iterable expression
     * ...          NEXT                        <--- call $next on the expression
     * ...          (expr)          ident       <--- identifier
     * ...          SET_NAME/ATTR   ident       <--- duplicate that value
     * ...          LOAD_NAME       ident       <--- set that value to the name
     * ...          PUSH            null        <--- push a null onto the stack
     * ...          EQ                          <--- compare the next result with null
     * j            IF              n           <--- if TOS evaluated to null, jump to end-of-block
     * ...          (block)
     * ...          GOTO            i           <--- go back to compare the value again
     * n            POP_BLOCK                   <--- always pop the block
     ***************************************************************************/

    const ms_StmtForIterator *iter = forstmt->clause.iter;

    PushOpCode(OPC_PUSH_BLOCK, 0, ctx);

    /* make sure we declare the name at the top of the block, so we can
     * jump back to an instruction after this one and don't renew the
     * name on each iteration */
    size_t new_index = 0;
    if (iter->declare) {
        new_index = dsarray_len(ctx->opcodes);
        PushOpCode(OPC_NEW_NAME, 0, ctx);
    }

    size_t i = dsarray_len(ctx->opcodes);
    ExprToOpCodes(iter->iter, ctx);
    PushOpCode(OPC_NEXT, 0, ctx);

    /* update the NEW_NAME opcode with the index of the identifier to new */
    if (iter->declare) {
        int index;
        IdentExprToOpCodes(iter->ident, &index, ctx);
        PushOpCode(OPC_SET_NAME, index, ctx);

        ms_VMOpCode *opc = dsarray_get(ctx->opcodes, new_index);
        *opc = ms_VMOpCodeWithArg(OPC_NEW_NAME, index);
    } else {
        IdentSetToOpCodes(iter->ident, ctx, false);
    }

    /* reload the expression value */
    ExprToOpCodes(iter->ident, ctx);

    /* compare to null */
    int val_index;
    ms_Value v = { .type = MSVAL_NULL, .val = { .n = MS_VM_NULL_POINTER } };
    PushValue(&v, &val_index, ctx);
    PushOpCode(OPC_PUSH, val_index, ctx);
    PushOpCode(OPC_EQ, 0, ctx);

    /* jump before the block if no value was returned from next */
    size_t j = dsarray_len(ctx->opcodes);
    PushOpCode(OPC_JUMP_IF_FALSE, 0, ctx);

    /* load up the block */
    size_t start = dsarray_len(ctx->opcodes);
    CodeGenContextBlock blkctx = { .parent = ctx, .push_or_pop = BLOCK_NO_PUSH_OR_POP };
    BlockToOpCodes(forstmt->block, &blkctx);
    size_t end = dsarray_len(ctx->opcodes);

    /* go back to the comparison at the beginning of the loop */
    PushOpCode(OPC_GOTO, (int)i, ctx);
    PushOpCode(OPC_POP_BLOCK, 0, ctx);

    /* update the conditional instruction with the index of the POP_BLOCK instruction */
    size_t pop = dsarray_len(ctx->opcodes) - 1;
    ms_VMOpCode *opc = dsarray_get(ctx->opcodes, j);
    *opc = ms_VMOpCodeWithArg(OPC_JUMP_IF_FALSE, (int)pop);

    /* update the BREAK and CONTINUE opcodes with the correct gotos */
    CodeGenContextFor ctxfor = { ctx, start, end, (int)pop, (int)i };
    StmtForFixBreakAndContinue(&ctxfor);
}

static void StmtForExprToOpCodes(const ms_StmtFor *forstmt, CodeGenContext *ctx) {
    assert(forstmt);
    assert(forstmt->type == FORSTMT_EXPR);
    assert(forstmt->clause.expr);
    assert(ctx);

    /***************************************************************************
     * FOR expr statements should end up looking like this in bytecode:
     *
     * index        instruction     arg
     * -----        -----------     ----
     * ...          PUSH_BLOCK                  <--- push a new block so everything is evaluated in that context
     * i            (expr)                      <--- expression to be evaluated
     * j            IF              j+n+1       <--- if TOS evaluated to false, jump to
     * ...          (block)
     * j+n          GOTO            j           <--- go back to the conditional
     * j+n+1        POP_BLOCK                   <--- always pop the block
     ***************************************************************************/

    PushOpCode(OPC_PUSH_BLOCK, 0, ctx);

    size_t i = dsarray_len(ctx->opcodes);
    ExprToOpCodes(forstmt->clause.expr->expr, ctx);

    size_t j = dsarray_len(ctx->opcodes);
    PushOpCode(OPC_JUMP_IF_FALSE, 0, ctx);

    size_t start = dsarray_len(ctx->opcodes);
    CodeGenContextBlock blkctx = { .parent = ctx, .push_or_pop = BLOCK_NO_PUSH_OR_POP };
    BlockToOpCodes(forstmt->block, &blkctx);
    size_t end = dsarray_len(ctx->opcodes);

    PushOpCode(OPC_GOTO, (int)i, ctx);
    PushOpCode(OPC_POP_BLOCK, 0, ctx);

    /* update the conditional instruction with the index of the POP_BLOCK instruction */
    size_t pop = dsarray_len(ctx->opcodes) - 1;
    ms_VMOpCode *opcif = dsarray_get(ctx->opcodes, j);
    *opcif = ms_VMOpCodeWithArg(OPC_JUMP_IF_FALSE, (int)pop);

    /* update the BREAK and CONTINUE opcodes with the correct gotos */
    CodeGenContextFor ctxfor = { ctx, start, end, (int)pop, (int)i };
    StmtForFixBreakAndContinue(&ctxfor);
}

static void StmtForFixBreakAndContinue(CodeGenContextFor *ctx) {
    assert(ctx);
    assert(ctx->parent);
    assert(ctx->start <= ctx->end);

    /* update BREAK and CONTINUE goto arguments in for loop blocks */
    for (size_t i = ctx->start; i < ctx->end; i++) {
        ms_VMOpCode *opc = dsarray_get(ctx->parent->opcodes, i);
        ms_VMOpCodeType type = ms_VMOpCodeGetCode((*opc));

        switch(type) {
            case OPC_BREAK:
                *opc = ms_VMOpCodeWithArg(OPC_GOTO, ctx->break_arg);
                break;
            case OPC_CONTINUE:
                *opc = ms_VMOpCodeWithArg(OPC_GOTO, ctx->cont_arg);
                break;
            default:
                break;
        }
    }
}

static void StmtIfToOpCodes(const ms_StmtIf *ifstmt, CodeGenContext *ctx) {
    assert(ifstmt);
    assert(ctx);

    /***************************************************************************
     * IF/ELSE IF/ELSE statements should end up looking like this in bytecode:
     *
     * index        instruction     arg
     * -----        -----------     ----
     * ...          (expr)                      <--- if statement expression
     * ...          IF              n+1         <--- if TOS evaluated to false, jump to next OPC_IF (if exists)
     * ...          (block)
     * n            GOTO            n+m+q       <--- block executed successfully, jump to end of IF/ELSE IF branch
     * n+1          IF              n+m+2       <--- if TOS evaluated to false, jump to final ELSE block
     * ...          (block)
     * n+m+1        GOTO            n+m+q       <--- block executed successfully, jump to end of IF/ELSE IF branch
     * ...          (block)
     * n+m+q        (...)                       <--- end of branch
     ***************************************************************************/

    /* if condition expression */
    ExprToOpCodes(ifstmt->expr, ctx);

    /* if opcode and location */
    size_t i = dsarray_len(ctx->opcodes);
    PushOpCode(OPC_JUMP_IF_FALSE, 0, ctx);

    /* push the entire IF block onto the opcode stack and get the new top index */
    CodeGenContextBlock blkctx = { .parent = ctx, .push_or_pop = BLOCK_INCL_PUSH | BLOCK_INCL_POP };
    BlockToOpCodes(ifstmt->block, &blkctx);
    size_t n = dsarray_len(ctx->opcodes);

    if (ifstmt->elif) {
        /* update IF opcode with n+1 argument */
        ms_VMOpCode *opcif = dsarray_get(ctx->opcodes, i);
        *opcif = ms_VMOpCodeWithArg(OPC_JUMP_IF_FALSE, (int)n+1);
        assert((n + 1) <= OPC_ARG_MAX);

        /* push the GOTO instruction on and cache its index */
        size_t j = dsarray_len(ctx->opcodes);
        PushOpCode(OPC_GOTO, 0, ctx);

        /* push any opcodes associated with subordinate branches */
        StmtElseIfToOpCodes(ifstmt->elif, ctx);

        /* update the original GOTO instruction argument */
        size_t eob = dsarray_len(ctx->opcodes);
        ms_VMOpCode *opcgoto = dsarray_get(ctx->opcodes, j);
        *opcgoto = ms_VMOpCodeWithArg(OPC_GOTO, (int)eob);
        assert(eob <= OPC_ARG_MAX);
    } else {
        /* update IF opcode with n argument */
        ms_VMOpCode *opcif = dsarray_get(ctx->opcodes, i);
        *opcif = ms_VMOpCodeWithArg(OPC_JUMP_IF_FALSE, (int)n);
        assert(n <= OPC_ARG_MAX);
    }
}

static void StmtElseIfToOpCodes(const ms_StmtIfElse *elif, CodeGenContext *ctx) {
    assert(elif);
    assert(ctx);

    switch (elif->type) {
        case IFELSE_IF:
            StmtIfToOpCodes(elif->clause.ifstmt, ctx);
            break;
        case IFELSE_ELSE: {
            CodeGenContextBlock blkctx = { .parent = ctx, .push_or_pop = BLOCK_INCL_PUSH | BLOCK_INCL_POP };
            BlockToOpCodes(elif->clause.elstmt->block, &blkctx);
            break;
        }
    }
}

static void StmtImportToOpCodes(const ms_StmtImport *import, CodeGenContext *ctx) {
    assert(import);
    assert(ctx);

    ms_ExprIdentType type = ms_ExprGetIdentType(import->ident);
    switch (type) {
        case EXPRIDENT_NONE:
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "import identifier is not an identifier");
            assert(false);
            break;
        case EXPRIDENT_BUILTIN:
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "cannot import a builtin name");
            assert(false);
            break;
        case EXPRIDENT_NAME: {
            assert(import->ident->type == EXPRTYPE_UNARY);
            assert(import->ident->cmpnt.u->type == EXPRATOM_IDENT);
            ms_Ident *ident = import->ident->cmpnt.u->atom.ident;
            int index;
            ms_Value v = { .type = MSVAL_STR, .val = { .s = ident->name } };
            PushValue(&v, &index, ctx);
            PushOpCode(OPC_PUSH, index, ctx);
            PushOpCode(OPC_IMPORT, 0, ctx);
            break;
        }
        case EXPRIDENT_GLOBAL:
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "cannot import a global");
            assert(false);
            break;
        case EXPRIDENT_QUALIFIED: {
            assert(import->ident->type == EXPRTYPE_BINARY);
            assert(import->ident->cmpnt.b->op == BINARY_GETATTR);
            CodeGenContextExpr ctxe = { .parent = ctx, .suppress_get_attr = true, .get_name_as_push = true, };
            ExprBinaryAttrListToOpCode(import->ident->cmpnt.b, &ctxe);
            int nsubscripts = (int)ctxe.attrcount;
            PushOpCode(OPC_IMPORT, nsubscripts, ctx);
            break;
        }
    }

    if (import->alias) {
        int index;
        PushIdent(import->alias, &index, ctx);
        PushOpCode(OPC_SET_NAME, index, ctx);
    }
}

static void StmtReturnToOpCodes(const ms_StmtReturn *ret, CodeGenContext *ctx) {
    assert(ret);
    assert(ctx);

    ExprToOpCodes(ret->expr, ctx);
    PushOpCode(OPC_RETURN, 0, ctx);
}

static void StmtAssignmentToOpCodes(const ms_StmtAssignment *assign, CodeGenContext *ctx) {
    assert(assign);
    assert(ctx);

    ms_StmtAssignExpr *expr = assign->expr;
    while (expr) {
        ExprToOpCodes(expr->expr, ctx);
        expr = expr->next;
    }

    StmtAssignmentTargetToOpCodes(assign->ident, ctx);
}

static void StmtAssignmentTargetToOpCodes(const ms_StmtAssignTarget *target, CodeGenContext *ctx) {
    assert(target);
    assert(ctx);

    /* take advantage of the stack unwinding to reverse order sets in post-order */
    if (target->next) {
        StmtAssignmentTargetToOpCodes(target->next, ctx);
    }
    IdentSetToOpCodes(target->target, ctx, false);
}

static void StmtDeclarationToOpCodes(const ms_StmtDeclaration *decl, CodeGenContext *ctx) {
    assert(decl);
    assert(ctx);
    assert(decl->ident->type == IDENT_NAME);

    /*
     * if a variable declaration includes an initializing expression, it is
     * important that the expression value is pushed first in the case that
     * the initializing expression contains the new name (e.g. the variable
     * shadows a variable name from a parent scope), otherwise the value
     * for that name in a shadowing declaration will _always_ be `null`
     *
     * e.g. the value associated with the name `index` declared within the
     *      loop body below will always be equal to `null + 1`, rather than
     *      the value of index from the enclosing scope `index` value
     *
     *     for var index := 1 : 10 {
     *         var index := index + 1;
     *     }
     */

    if (decl->expr) {
        ExprToOpCodes(decl->expr, ctx);
    }

    int index;
    PushIdent(decl->ident, &index, ctx);
    PushOpCode(OPC_NEW_NAME, index, ctx);

    if (decl->expr) {
        PushOpCode(OPC_SET_NAME, index, ctx);
    }

    if (decl->next) {
        StmtDeclarationToOpCodes(decl->next, ctx);
    }
}

static void IdentSetToOpCodes(const ms_Expr *ident, CodeGenContext *ctx, bool new_name) {
    assert(ident);
    assert(ctx);

    ms_ExprIdentType type = ms_ExprGetIdentType(ident);
    switch (type) {
        case EXPRIDENT_NONE:
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "assignment identifier is not an identifier");
            assert(false);
            break;
        case EXPRIDENT_BUILTIN:
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "cannot assign to builtin name");
            assert(false);
            break;
        case EXPRIDENT_NAME: {
            int index;
            IdentExprToOpCodes(ident, &index, ctx);
            if (new_name) {
                PushOpCode(OPC_NEW_NAME, index, ctx);
            }
            PushOpCode(OPC_SET_NAME, index, ctx);
            break;
        }
        case EXPRIDENT_GLOBAL: {
            int nsubscripts = 0;
            GlobalIdentExprToOpCodes(ident, &nsubscripts, ctx);
            PushOpCode(OPC_SET_GLO, nsubscripts, ctx);
            break;
        }
        case EXPRIDENT_QUALIFIED: {
            QualifiedIdentExprToOpCodes(ident, ctx);
            PushOpCode(OPC_SET_ATTR, 0, ctx);
            break;
        }
    }
}

static void GlobalIdentExprToOpCodes(const ms_Expr *ident, int *nsubscripts, CodeGenContext *ctx) {
    assert(ident);
    assert(ms_ExprGetIdentType(ident) == EXPRIDENT_GLOBAL);
    assert(ctx);

    if (ident->type == EXPRTYPE_UNARY) {
        ms_ExprUnary *u = ident->cmpnt.u;
        CodeGenContextExpr ctxe = { ctx };
        ExprComponentToOpCodes(&u->atom, ident->cmpnt.u->type, &ctxe);
        *nsubscripts = 0;
    } else {
        assert(ident->cmpnt.b->op == BINARY_GETATTR);
        ms_ExprBinary *b = ident->cmpnt.b;
        CodeGenContextExpr ctxe = { ctx };
        ExprBinaryAttrListToOpCode(b, &ctxe);
        *nsubscripts = (int)ctxe.attrcount;
    }
    /* intentionally provide no opcode */
}

static void QualifiedIdentExprToOpCodes(const ms_Expr *ident, CodeGenContext *ctx) {
    assert(ident);
    assert(ident->type == EXPRTYPE_BINARY);
    assert(ident->cmpnt.b->op == BINARY_GETATTR);
    assert(ms_ExprGetIdentType(ident) == EXPRIDENT_QUALIFIED);
    assert(ctx);

    const ms_ExprBinary *b = ident->cmpnt.b;
    CodeGenContextExpr ctxe = { ctx };
    ExprComponentToOpCodes(&b->latom, b->ltype, &ctxe);
    ExprComponentToOpCodes(&b->ratom, b->rtype, &ctxe);
    /* intentionally provide no opcode */
}

static void IdentExprToOpCodes(const ms_Expr *ident, int *index, CodeGenContext *ctx) {
    assert(ident);
    assert(ident->type == EXPRTYPE_UNARY);
    assert(ident->cmpnt.u->op == UNARY_NONE);
    assert(ident->cmpnt.u->type == EXPRATOM_IDENT);
    assert((ms_ExprGetIdentType(ident) == EXPRIDENT_NAME) ||
           (ms_ExprGetIdentType(ident) == EXPRIDENT_GLOBAL) ||
           (ms_ExprGetIdentType(ident) == EXPRIDENT_BUILTIN));

    const ms_ExprUnary *u = ident->cmpnt.u;
    PushIdent(u->atom.ident, index, ctx);
    /* intentionally provide no opcode */
}

static void ExprToOpCodes(const ms_Expr *expr, CodeGenContext *ctx) {
    assert(expr);
    assert(ctx);

    CodeGenContextExpr ctxe = { ctx };
    ExprToOpCodesInner(expr, &ctxe);
}

static void ExprToOpCodesInner(const ms_Expr *expr, CodeGenContextExpr *ctx) {
    assert(expr);
    assert(ctx);

    switch (expr->type) {
        case EXPRTYPE_UNARY:
            ExprUnaryToOpCodes(expr->cmpnt.u, ctx);
            break;
        case EXPRTYPE_BINARY:
            ExprBinaryToOpCodes(expr->cmpnt.b, ctx);
            break;
        case EXPRTYPE_CONDITIONAL:
            ExprConditionalToOpCodes(expr->cmpnt.c, ctx);
            break;
    }
}

static void ExprUnaryToOpCodes(const ms_ExprUnary *u, CodeGenContextExpr *ctx) {
    assert(u);
    assert(ctx);

    ms_ExprIdentType ident_type = ExprAtomGetIdentType(&u->atom, u->type);
    ExprComponentToOpCodes(&u->atom, u->type, ctx);

    if (ident_type == EXPRIDENT_GLOBAL) {
        PushOpCode(OPC_GET_GLO, 0, ctx->parent);
    }

    ExprUnaryOpToOpCode(u->op, ctx->parent);
}

static void ExprBinaryToOpCodes(const ms_ExprBinary *b, CodeGenContextExpr *ctx) {
    assert(b);
    assert(ctx);

    switch (b->op) {
        case BINARY_CALL: {
            assert(b->rtype == EXPRATOM_EXPRLIST);
            ms_ExprIdentType ident_type = ExprAtomGetIdentType(&b->latom, b->ltype);
            ExprComponentToOpCodes(&b->ratom, b->rtype, ctx);
            ExprComponentToOpCodes(&b->latom, b->ltype, ctx);
            size_t len = dsarray_len(b->ratom.list);
            assert(len <= OPC_ARG_MAX);
            ms_VMOpCodeType opc = (ident_type == EXPRIDENT_BUILTIN)
                                  ? (OPC_CALL_BUILTIN)
                                  : (OPC_CALL);
            PushOpCode(opc, (int) len, ctx->parent);
            break;
        }
        case BINARY_SAFEGETATTR: {
            ExprSafeGetAttrToOpCodes(b, ctx);
            break;
        }
        case BINARY_GETATTR: {
            assert(b->rtype != EXPRATOM_EXPRLIST);
            ms_ExprIdentType ident_type = ExprAtomGetIdentType(&b->latom, b->ltype);
            ExprBinaryAttrListToOpCode(b, ctx);

            if (ident_type == EXPRIDENT_GLOBAL) {
                if (ctx->attrdepth == 0) {
                    PushOpCode(OPC_GET_GLO, (int)ctx->attrcount, ctx->parent);
                }
            } else {
                if (!ctx->suppress_get_attr) {
                    PushOpCode(OPC_GET_ATTR, 0, ctx->parent);
                }
            }
            break;
        }
        default: {
            ms_ExprIdentType lident_type = ExprAtomGetIdentType(&b->latom, b->ltype);
            ExprComponentToOpCodes(&b->latom, b->ltype, ctx);
            if ((lident_type == EXPRIDENT_GLOBAL) && (ctx->attrcount == 0)) {
                PushOpCode(OPC_GET_GLO, 0, ctx->parent);
            }

            ms_ExprIdentType rident_type = ExprAtomGetIdentType(&b->ratom, b->rtype);
            ExprComponentToOpCodes(&b->ratom, b->rtype, ctx);
            if ((rident_type == EXPRIDENT_GLOBAL) && (ctx->attrcount == 0)) {
                PushOpCode(OPC_GET_GLO, 0, ctx->parent);
            }

            ExprBinaryOpToOpCode(b->op, ctx->parent);
            break;
        }
    }
}

static void ExprSafeGetAttrToOpCodes(const ms_ExprBinary *b, CodeGenContextExpr *ctx) {
    assert(b);
    assert(ctx);
    assert(b->op == BINARY_SAFEGETATTR);
    assert(b->rtype != EXPRATOM_EXPRLIST);
    assert(ExprAtomGetIdentType(&b->latom, b->ltype) != EXPRIDENT_GLOBAL);

    /***************************************************************************
     * Safe-get attribute should end up looking like this in bytecode:
     *
     * index        instruction     arg
     * -----        -----------     ----
     * ...          (expr)                      <--- object
     * ...          DUP                         <--- duplicate the object reference
     * ...          PUSH                        <--- push a null onto the stack
     * ...          EQ                          <--- compare the object to null
     * i            IF              k           <--- if the object is not null, jump to the GET_ATTR
     * ...          POP                         <--- pop the attribute
     * ...          PUSH                        <--- push the null back onto the stack
     * j            GOTO            k+1         <--- skip past the GET_ATTR
     * k            GET_ATTR
     ***************************************************************************/

    size_t i = 0;
    size_t j = 0;
    size_t k = 0;

    ctx->attrdepth += 1;
    ctx->attrcount += 1;
    ExprComponentToOpCodes(&b->latom, b->ltype, ctx);

    if (!ctx->suppress_get_attr) {
        PushOpCode(OPC_DUP, 0, ctx->parent);

        int val_index;
        ms_Value v = { .type = MSVAL_NULL, .val = { .n = MS_VM_NULL_POINTER } };
        PushValue(&v, &val_index, ctx->parent);
        PushOpCode(OPC_PUSH, val_index, ctx->parent);
        PushOpCode(OPC_EQ, 0, ctx->parent);

        i = dsarray_len(ctx->parent->opcodes);
        PushOpCode(OPC_JUMP_IF_FALSE, 0, ctx->parent);

        PushOpCode(OPC_POP, 0, ctx->parent);
        PushOpCode(OPC_PUSH, val_index, ctx->parent);

        j = dsarray_len(ctx->parent->opcodes);
        PushOpCode(OPC_GOTO, 0, ctx->parent);

        k = dsarray_len(ctx->parent->opcodes);
    }

    ExprComponentToOpCodes(&b->ratom, b->rtype, ctx);
    ctx->attrdepth -= 1;

    /* fix opcode arguments */
    if (!ctx->suppress_get_attr) {
        PushOpCode(OPC_GET_ATTR, 0, ctx->parent);
        size_t diff = dsarray_len(ctx->parent->opcodes) - k;

        ms_VMOpCode *opcif = dsarray_get(ctx->parent->opcodes, i);
        *opcif = ms_VMOpCodeWithArg(OPC_JUMP_IF_FALSE, (int)k);

        ms_VMOpCode *opcgoto = dsarray_get(ctx->parent->opcodes, j);
        *opcgoto = ms_VMOpCodeWithArg(OPC_GOTO, (int)(k+diff));
    }
}

static void ExprConditionalToOpCodes(const ms_ExprConditional *c, CodeGenContextExpr *ctx) {
    assert(c);
    assert(ctx);
    assert(ctx->parent);

    /***************************************************************************
     * Conditional expressions should end up looking like this in bytecode:
     *
     * index        instruction     arg
     * -----        -----------     ----
     * ...          (expr)                      <--- conditional expression
     * i            IF              j           <--- if TOS evaluated to false, jump to false value
     * ...          (expr)                      <--- expression if true
     * ...          GOTO            k           <--- jump past false expression
     * j            (expr)                      <--- expression if false
     * k            (...)                       <--- end of branch
     ***************************************************************************/

    ms_ExprIdentType cond_ident_type = ExprAtomGetIdentType(&c->cond, c->condtype);
    ExprComponentToOpCodes(&c->cond, c->condtype, ctx);

    if (cond_ident_type == EXPRIDENT_GLOBAL) {
        PushOpCode(OPC_GET_GLO, 0, ctx->parent);
    }

    size_t i = dsarray_len(ctx->parent->opcodes);
    PushOpCode(OPC_JUMP_IF_FALSE, 0, ctx->parent);

    ms_ExprIdentType true_ident_type = ExprAtomGetIdentType(&c->iftrue, c->truetype);
    ExprComponentToOpCodes(&c->iftrue, c->truetype, ctx);

    if (true_ident_type == EXPRIDENT_GLOBAL) {
        PushOpCode(OPC_GET_GLO, 0, ctx->parent);
    }

    PushOpCode(OPC_GOTO, 0, ctx->parent);
    size_t j = dsarray_len(ctx->parent->opcodes);

    ms_ExprIdentType false_ident_type = ExprAtomGetIdentType(&c->iffalse, c->falsetype);
    ExprComponentToOpCodes(&c->iffalse, c->falsetype, ctx);

    if (false_ident_type == EXPRIDENT_GLOBAL) {
        PushOpCode(OPC_GET_GLO, 0, ctx->parent);
    }

    /* fix the opcode arguments */
    size_t k = dsarray_len(ctx->parent->opcodes);
    ms_VMOpCode *ifopc = dsarray_get(ctx->parent->opcodes, i);
    *ifopc = ms_VMOpCodeWithArg(OPC_JUMP_IF_FALSE, (int)j);
    ms_VMOpCode *gotoopc = dsarray_get(ctx->parent->opcodes, j-1);
    *gotoopc = ms_VMOpCodeWithArg(OPC_GOTO, (int)k);
}

static void ExprComponentToOpCodes(const ms_ExprAtom *a, ms_ExprAtomType type, CodeGenContextExpr *ctx) {
    assert(a);
    assert(ctx);

    switch (type) {
        case EXPRATOM_EXPRESSION:
            ExprToOpCodesInner(a->expr, ctx);
            break;
        case EXPRATOM_VALUE: {
            int index_or_len;
            PushValue(&a->val, &index_or_len, ctx->parent);
            ExprComponentValueToOpCodes(&a->val, index_or_len, ctx->parent);
            break;
        }
        case EXPRATOM_IDENT: {
            int index;
            if ((a->ident->type == IDENT_GLOBAL) || (ctx->get_name_as_push)) {
                /* convert identifier to a string value and push it;
                 * do this for known global identifiers or if the context
                 * requires that we do (such as for `import` statements) */
                ms_Value v = { .type = MSVAL_STR, .val = { .s = a->ident->name } };
                PushValue(&v, &index, ctx->parent);
                PushOpCode(OPC_PUSH, index, ctx->parent);
            } else {
                PushIdent(a->ident, &index, ctx->parent);
                PushOpCode(OPC_GET_NAME, index, ctx->parent);
            }
            break;
        }
        case EXPRATOM_EXPRLIST: {
            size_t len = dsarray_len(a->list);
            for (size_t i = 0; i < len; i++) {
                ms_Expr *expr = dsarray_get(a->list, i);
                ExprToOpCodes(expr, ctx->parent);
            }
            break;
        }
        case EXPRATOM_EMPTY:
            assert(false);
    }
}

static void ExprComponentValueToOpCodes(const ms_Value *val, int index_or_len, CodeGenContext *ctx) {
    assert(val);
    assert(ctx);

    switch (val->type) {
        case MSVAL_FLOAT:   /* fall through */
        case MSVAL_INT:     /* fall through */
        case MSVAL_BOOL:    /* fall through */
        case MSVAL_NULL:    /* fall through */
        case MSVAL_STR:     /* fall through */
        case MSVAL_FUNC:
            PushOpCode(OPC_PUSH, index_or_len, ctx);
            break;
        case MSVAL_ARRAY:
            PushOpCode(OPC_MAKE_LIST, index_or_len, ctx);
            break;
        case MSVAL_OBJECT:
            assert((index_or_len % 2) == 0);
            PushOpCode(OPC_MAKE_OBJ, index_or_len, ctx);
            break;
    }
}

static void ExprUnaryOpToOpCode(ms_ExprUnaryOp op, CodeGenContext *ctx) {
    assert(ctx);

    ms_VMOpCodeType o;
    switch (op) {
        case UNARY_MINUS:           o = OPC_NEGATE;         break;
        case UNARY_NOT:             o = OPC_NOT;            break;
        case UNARY_BITWISE_NOT:     o = OPC_BITWISE_NOT;    break;
        case UNARY_NONE:
            /* this function may be called with UNARY_NONE expressions */
            return;
        default:
            assert(false);
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "invalid unary expression op found");
            return;
    }

    PushOpCode(o, 0, ctx);
}

static void ExprBinaryOpToOpCode(ms_ExprBinaryOp op, CodeGenContext *ctx) {
    assert(ctx);

    ms_VMOpCodeType o;
    switch (op) {
        case BINARY_PLUS:               o = OPC_ADD;            break;
        case BINARY_MINUS:              o = OPC_SUBTRACT;       break;
        case BINARY_TIMES:              o = OPC_MULTIPLY;       break;
        case BINARY_DIVIDE:             o = OPC_DIVIDE;         break;
        case BINARY_IDIVIDE:            o = OPC_IDIVIDE;        break;
        case BINARY_MODULO:             o = OPC_MODULO;         break;
        case BINARY_EXPONENTIATE:       o = OPC_EXPONENTIATE;   break;
        case BINARY_SHIFT_LEFT:         o = OPC_SHIFT_LEFT;     break;
        case BINARY_SHIFT_RIGHT:        o = OPC_SHIFT_RIGHT;    break;
        case BINARY_BITWISE_AND:        o = OPC_BITWISE_AND;    break;
        case BINARY_BITWISE_XOR:        o = OPC_BITWISE_XOR;    break;
        case BINARY_BITWISE_OR:         o = OPC_BITWISE_OR;     break;
        case BINARY_LE:                 o = OPC_LE;             break;
        case BINARY_LT:                 o = OPC_LT;             break;
        case BINARY_GE:                 o = OPC_GE;             break;
        case BINARY_GT:                 o = OPC_GT;             break;
        case BINARY_EQ:                 o = OPC_EQ;             break;
        case BINARY_NOT_EQ:             o = OPC_NOT_EQ;         break;
        case BINARY_AND:                o = OPC_AND;            break;
        case BINARY_OR:                 o = OPC_OR;             break;
        case BINARY_CALL:
            assert(false);
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "should not be generating call opcode here");
            return;
        case BINARY_GETATTR:
            assert(false);
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "should not be generating getattr opcode here");
            return;
        case BINARY_SAFEGETATTR:
            assert(false);
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "should not be generating safegetattr opcode here");
            return;
        default:
            assert(false);
            ctx->res = MS_RESULT_ERROR;
            CodeGenContextErrorSet(ctx, "invalid binary expression op found");
            return;
    }

    PushOpCode(o, 0, ctx);
}

static void ExprBinaryAttrListToOpCode(const ms_ExprBinary *b, CodeGenContextExpr *ctx) {
    assert(b);
    assert(b->rtype != EXPRATOM_EXPRLIST);
    assert(ctx);
    ctx->attrdepth += 1;
    ctx->attrcount += 1;
    ExprComponentToOpCodes(&b->latom, b->ltype, ctx);
    ExprComponentToOpCodes(&b->ratom, b->rtype, ctx);
    ctx->attrdepth -= 1;
}

static ms_VMFunc *ExprFunctionExprToOpCodes(const ms_ValFunc *fn, CodeGenContext *ctx) {
    assert(fn);
    assert(ctx);

    if (!CodeGenContextCreate(ctx)) {
        return NULL;
    }

    size_t nstmts = dsarray_len(fn->block);
    for (size_t i = 0; i < nstmts; i++) {
        ms_Stmt *stmt = dsarray_get(fn->block, i);
        StmtToOpCodes(stmt, ctx);
    }

    ms_VMFunc *func = malloc(sizeof(ms_VMFunc));
    if (!func) {
        CodeGenContextClean(ctx);
        return NULL;
    }

    size_t nargs = dsarray_len(fn->args);
    func->args = dsarray_new_cap(nargs, (dsarray_compare_fn)dsbuf_compare,
                                 (dsarray_free_fn)dsbuf_destroy);
    if (!func->args) {
        free(func);
        CodeGenContextClean(ctx);
        return NULL;
    }

    for (size_t i = 0; i < nargs; i++) {
        ms_Ident *ident = dsarray_get(fn->args, i);
        DSBuffer *name = dsbuf_dup(ident->name);
        if (!name) {
            free(func);
            dsarray_destroy(func->args);
            CodeGenContextClean(ctx);
            return NULL;
        }
        dsarray_append(func->args, name);
    }

    func->code = VMByteCodeNew(ctx);
    CodeGenContextClean(ctx);
    return func;
}

static ms_ExprIdentType ExprAtomGetIdentType(const ms_ExprAtom *atom, ms_ExprAtomType type) {
    assert(atom);
    ms_ExprIdentType ident_type = EXPRIDENT_NONE;
    if (type == EXPRATOM_EXPRESSION) {
        ident_type = ms_ExprGetIdentType(atom->expr);
    } else if (type == EXPRATOM_IDENT) {
        switch (atom->ident->type) {
            case IDENT_GLOBAL:
                return EXPRIDENT_GLOBAL;
            case IDENT_BUILTIN:
                return EXPRIDENT_BUILTIN;
            case IDENT_NAME:
                return EXPRIDENT_NAME;
        }
    }
    return ident_type;
}

static void PushValue(const ms_Value *val, int *index_or_len, CodeGenContext *ctx) {
    assert(val);
    assert(index_or_len);
    assert(ctx);

    ms_VMValue *newv = malloc(sizeof(ms_VMValue));
    if (!newv) {
        ctx->res = MS_RESULT_ERROR;
        CodeGenContextErrorSet(ctx, "could not allocate memory for a value");
        return;
    }

    switch (val->type) {
        case MSVAL_FLOAT:
            newv->type = VMVAL_FLOAT;
            newv->val.f = val->val.f;
            break;
        case MSVAL_INT:
            newv->type = VMVAL_INT;
            newv->val.i = val->val.i;
            break;
        case MSVAL_BOOL:
            newv->type = VMVAL_BOOL;
            newv->val.b = val->val.b;
            break;
        case MSVAL_STR:
            newv->type = VMVAL_STR;
            newv->val.s = dsbuf_dup(val->val.s);
            if (!newv->val.s) {
                ctx->res = MS_RESULT_ERROR;
                CodeGenContextErrorSet(ctx, "could not allocate memory a string");
            }
            assert(newv->val.s);
            break;
        case MSVAL_NULL:
            newv->type = VMVAL_NULL;
            newv->val.n = val->val.n;
            break;
        case MSVAL_ARRAY: {
            size_t len = dsarray_len(val->val.a);
            for (size_t i = 0; i < len; i++) {
                ms_Expr *elem = dsarray_get(val->val.a, i);
                ExprToOpCodes(elem, ctx);
            }
            *index_or_len = (int)len;
            free(newv);
            return;     /* return so length isn't overwritten */
        }
        case MSVAL_OBJECT: {
            size_t len = dsarray_len(val->val.o);
            for (size_t i = 0; i < len; i++) {
                ms_ValObjectTuple *tuple = dsarray_get(val->val.o, i);
                ExprToOpCodes(tuple->key, ctx);
                ExprToOpCodes(tuple->val, ctx);
            }
            *index_or_len = (int)(len * 2);
            free(newv);
            return;     /* return so length isn't overwritten */
        }
        case MSVAL_FUNC: {
            CodeGenContext fnctx = { .err = ctx->err, .res = MS_RESULT_SUCCESS };
            newv->type = VMVAL_FUNC;
            newv->val.fn = ExprFunctionExprToOpCodes(val->val.fn, &fnctx);
            if (fnctx.res == MS_RESULT_ERROR) {
                ctx->res = fnctx.res;
                /* do not overwrite the error message from inner routines */
            } else if (!newv->val.fn) {
                ctx->res = MS_RESULT_ERROR;
                CodeGenContextErrorSet(ctx, "could not allocate memory for a function value");
            }
            assert(newv->val.fn);
            break;
        }
    }

    dsarray_append(ctx->values, newv);
    size_t nvals = dsarray_len(ctx->values);
    assert(nvals != 0);
    assert(nvals <= OPC_ARG_MAX);
    *index_or_len = (int)(nvals - 1);
}

static void PushIdent(const ms_Ident *ident, int *index, CodeGenContext *ctx) {
    assert(ident);
    assert(index);
    assert(ctx);

    /* see if we had already used this identifier and we don't need to
     * duplicate it in the bytecode representation */
    size_t *existing_index = dsdict_get(ctx->ident_cache, ident->name);
    if (existing_index) {
        *index = (int)(*existing_index);
        return;
    }

    /* push the identifer onto the context stack */
    DSBuffer *name = dsbuf_dup(ident->name);
    if (!name) {
        ctx->res = MS_RESULT_ERROR;
        CodeGenContextErrorSet(ctx, "could not allocate memory for an ident");
        assert(false);
        return;
    }

    dsarray_append(ctx->idents, name);
    size_t nidents = dsarray_len(ctx->idents);
    assert(nidents != 0);
    assert(nidents <= OPC_ARG_MAX);
    *index = (int)(nidents - 1);

    /* cache this identifier */
    size_t *new_index = malloc(sizeof(size_t));
    if (!new_index) {
        ctx->res = MS_RESULT_ERROR;
        CodeGenContextErrorSet(ctx, "could not allocate memory for an ident index");
        assert(false);
        return;
    }
    *new_index = (size_t)(*index);
    dsdict_put(ctx->ident_cache, name, new_index);
}

static void PushOpCode(ms_VMOpCodeType type, int arg, CodeGenContext *ctx) {
    assert(ctx);
    assert(arg <= OPC_ARG_MAX);

    ms_VMOpCode *o = malloc(sizeof(ms_VMOpCode));
    if (!o) {
        ctx->res = MS_RESULT_ERROR;
        CodeGenContextErrorSet(ctx, "could not allocate memory for an opcode");
        assert(false);
        return;
    }
    *o = ms_VMOpCodeWithArg(type, arg);
    dsarray_append(ctx->opcodes, o);
}

static void CodeGenContextErrorSet(CodeGenContext *ctx, const char *msg) {
    assert(ctx);

    ms_Error **err = ctx->err;
    assert(!(*err));
    *err = malloc(sizeof(ms_Error));
    if (!(*err)) {
        return;
    }
    (*err)->type = MS_ERROR_CODEGEN;

    int len = snprintf(NULL, 0, msg);
    if (len < 0) {
        goto codegen_close_error;
    }

    (*err)->len = (size_t)len;
    (*err)->msg = malloc((size_t)len + 1);
    if ((*err)->msg) {
        snprintf((*err)->msg, len + 1, msg);
    }

codegen_close_error:
    return;
}
