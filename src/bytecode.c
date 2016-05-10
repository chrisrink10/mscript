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
#include "bytecode.h"
#include "vm.h"
#include "lang.h"

#define OPC_BITS ((sizeof(int) / 2) * 8)
#define OPC_ARG_MAX ((1 << OPC_BITS) - 1)

#define BLOCK_NO_PUSH_OR_POP (0)
#define BLOCK_INCL_PUSH      (1)
#define BLOCK_INCL_POP       (2)

static const int EXPR_OPCODE_STACK_LEN = 50;
static const int EXPR_VALUE_STACK_LEN = 50;
static const int EXPR_IDENT_STACK_LEN = 50;

static bool CreateByteCodeStacks(DSArray **opcodes, DSArray **values, DSArray **idents);
static ms_VMByteCode *VMByteCodeNew(const DSArray *opcodes, const DSArray *values, const DSArray *idents);
static void BlockToOpCodes(const ms_StmtBlock *blk, int push_or_pop, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtToOpCodes(const ms_Stmt *stmt, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtDeleteToOpCodes(const ms_StmtDelete *del, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtForToOpCodes(const ms_StmtFor *forstmt, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtForIncToOpCodes(const ms_StmtFor *forstmt, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtForIterToOpCodes(const ms_StmtFor *forstmt, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtForExprToOpCodes(const ms_StmtFor *forstmt, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtForFixBreakAndContinue(DSArray *opcodes, size_t start, size_t end, int break_arg, int cont_arg);
static void StmtIfToOpCodes(const ms_StmtIf *ifstmt, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtElseIfToOpCodes(const ms_StmtIfElse *elif, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtImportToOpCodes(const ms_StmtImport *import, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtMergeToOpCodes(const ms_StmtMerge *merge, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtReturnToOpCodes(const ms_StmtReturn *ret, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtAssignmentToOpCodes(const ms_StmtAssignment *assign, DSArray *opcodes, DSArray *values, DSArray *idents);
static void StmtDeclarationToOpCodes(const ms_StmtDeclaration *decl, DSArray *opcodes, DSArray *values, DSArray *idents);
static void IdentSetToOpCodes(const ms_Expr *ident, DSArray *opcodes, DSArray *values, DSArray *idents, bool new_name);
static void QualifiedIdentExprToOpCodes(const ms_Expr *ident, int *nsubscripts, DSArray *opcodes, DSArray *values, DSArray *idents);
static void IdentExprToOpCodes(const ms_Expr *ident, int *index, DSArray *idents);
static void ExprToOpCodes(const ms_Expr *expr, DSArray *opcodes, DSArray *values, DSArray *idents);
static void ExprUnaryToOpCodes(const ms_ExprUnary *u, DSArray *opcodes, DSArray *values, DSArray *idents);
static void ExprBinaryToOpCodes(const ms_ExprBinary *b, DSArray *opcodes, DSArray *values, DSArray *idents);
static void ExprComponentToOpCodes(const ms_ExprAtom *a, ms_ExprAtomType type, DSArray *opcodes, DSArray *values, DSArray *idents);
static void ExprUnaryOpToOpCode(ms_ExprUnaryOp op, DSArray *opcodes);
static void ExprBinaryOpToOpCode(ms_ExprBinaryOp op, DSArray *opcodes);
static void PushValue(const ms_Value *val, int *index, DSArray *values);
static void PushIdent(const ms_Ident *ident, int *index, DSArray *idents);
static void PushOpCode(ms_VMOpCodeType type, int arg, DSArray *opcodes);

/*
 * PUBLIC FUNCTIONS
 */

ms_VMByteCode *ms_ASTToOpCodes(ms_AST *ast) {
    if (!ast) { return NULL; }

    DSArray *opcodes = NULL;
    DSArray *values = NULL;
    DSArray *idents = NULL;
    if (!CreateByteCodeStacks(&opcodes, &values, &idents)) {
        return NULL;
    }

    size_t len = dsarray_len(ast);
    for (size_t i = 0; i < len; i++) {
        ms_Stmt *stmt = dsarray_get(ast, i);
        StmtToOpCodes(stmt, opcodes, values, idents);
    }

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
    return (c | (arg << OPC_BITS));
}

int ms_VMOpCodeGetArg(ms_VMOpCode c) {
    return (c >> OPC_BITS);
}

ms_VMOpCodeType ms_VMOpCodeGetCode(ms_VMOpCode c) {
    return (ms_VMOpCodeType)(c & ((1 << OPC_BITS) - 1));
}

/*
 * PRIVATE FUNCTIONS
 */

static bool CreateByteCodeStacks(DSArray **opcodes, DSArray **values, DSArray **idents) {
    assert(opcodes);
    assert(values);
    assert(idents);

    *opcodes = dsarray_new_cap(EXPR_OPCODE_STACK_LEN, NULL, (dsarray_free_fn)free);
    if (!(*opcodes)) {
        return false;
    }

    *values = dsarray_new_cap(EXPR_VALUE_STACK_LEN, NULL, (dsarray_free_fn)free);
    if (!(*values)) {
        dsarray_destroy(*opcodes);
        return false;
    }

    /* no dsarray_free_fn required since we pass the pointer to the bytecode */
    *idents = dsarray_new_cap(EXPR_IDENT_STACK_LEN, NULL, NULL);
    if (!(*idents)) {
        dsarray_destroy(*values);
        dsarray_destroy(*opcodes);
        return false;
    }

    return true;
}

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

static void BlockToOpCodes(const ms_StmtBlock *blk, int push_or_pop, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(blk);
    assert(opcodes);
    assert(values);
    assert(idents);

    if ((push_or_pop & BLOCK_INCL_PUSH) == BLOCK_INCL_PUSH) {
        PushOpCode(OPC_PUSH_BLOCK, 0, opcodes);
    }

    size_t len = dsarray_len(blk);
    for (size_t i = 0; i < len; i++) {
        ms_Stmt *stmt = dsarray_get(blk, i);
        StmtToOpCodes(stmt, opcodes, values, idents);
    }

    if ((push_or_pop & BLOCK_INCL_POP) == BLOCK_INCL_POP) {
        PushOpCode(OPC_POP_BLOCK, 0, opcodes);
    }
}

static void StmtToOpCodes(const ms_Stmt *stmt, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(stmt);
    assert(opcodes);
    assert(values);
    assert(idents);

    switch (stmt->type) {
        case STMTTYPE_EMPTY:
            assert(false && "should not have an empty statement");
            break;
        case STMTTYPE_BREAK:
            PushOpCode(OPC_BREAK, 0, opcodes);
            break;
        case STMTTYPE_CONTINUE:
            PushOpCode(OPC_CONTINUE, 0, opcodes);
            break;
        case STMTTYPE_DELETE:
            StmtDeleteToOpCodes(stmt->cmpnt.del, opcodes, values, idents);
            break;
        case STMTTYPE_FOR:
            StmtForToOpCodes(stmt->cmpnt.forstmt, opcodes, values, idents);
            break;
        case STMTTYPE_IF:
            StmtIfToOpCodes(stmt->cmpnt.ifstmt, opcodes, values, idents);
            break;
        case STMTTYPE_IMPORT:
            StmtImportToOpCodes(stmt->cmpnt.import, opcodes, values, idents);
            break;
        case STMTTYPE_MERGE:
            StmtMergeToOpCodes(stmt->cmpnt.merge, opcodes, values, idents);
            break;
        case STMTTYPE_RETURN:
            StmtReturnToOpCodes(stmt->cmpnt.ret, opcodes, values, idents);
            break;
        case STMTTYPE_ASSIGNMENT:
            StmtAssignmentToOpCodes(stmt->cmpnt.assign, opcodes, values, idents);
            break;
        case STMTTYPE_DECLARATION:
            StmtDeclarationToOpCodes(stmt->cmpnt.decl, opcodes, values, idents);
            break;
        case STMTTYPE_EXPRESSION:
            ExprToOpCodes(stmt->cmpnt.expr, opcodes, values, idents);
            break;
    }
}

static void StmtDeleteToOpCodes(const ms_StmtDelete *del, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(del);
    assert(opcodes);
    assert(values);
    assert(idents);

    if (ms_ExprIsIdent(del->expr)) {
        int index;
        IdentExprToOpCodes(del->expr, &index, idents);
        PushOpCode(OPC_DEL_NAME, index, opcodes);
    } else if (ms_ExprIsQualifiedIdent(del->expr)) {
        int nsubscripts = 0;
        QualifiedIdentExprToOpCodes(del->expr, &nsubscripts, opcodes, values, idents);
        PushOpCode(OPC_DEL_ATTR, nsubscripts, opcodes);
    } else {
        assert(false && "assignment identifier is not an identifier");
    }
}

static void StmtForToOpCodes(const ms_StmtFor *forstmt, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(forstmt);
    assert(opcodes);
    assert(values);
    assert(idents);

    switch (forstmt->type) {
        case FORSTMT_INCREMENT:
            StmtForIncToOpCodes(forstmt, opcodes, values, idents);
            break;
        case FORSTMT_ITERATOR:
            StmtForIterToOpCodes(forstmt, opcodes, values, idents);
            break;
        case FORSTMT_EXPR:
            StmtForExprToOpCodes(forstmt, opcodes, values, idents);
            break;
    }
}

static void StmtForIncToOpCodes(const ms_StmtFor *forstmt, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(forstmt);
    assert(forstmt->type == FORSTMT_INCREMENT);
    assert(forstmt->clause.inc);
    assert(opcodes);
    assert(values);
    assert(idents);

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
    PushOpCode(OPC_PUSH_BLOCK, 0, opcodes);
    ExprToOpCodes(inc->init, opcodes, values, idents);

    /* save the initial value to the loop identifier */
    bool should_new_name = inc->declare;
    IdentSetToOpCodes(inc->ident, opcodes, values, idents, should_new_name);

    /* load up the expression value and compare it to the end
     * to make sure we're still within the valid range */
    size_t i = dsarray_len(opcodes);
    ExprToOpCodes(inc->ident, opcodes, values, idents);
    ExprToOpCodes(inc->end, opcodes, values, idents);
    PushOpCode(OPC_LE, 0, opcodes);
    size_t j = dsarray_len(opcodes);
    PushOpCode(OPC_JUMP_IF_FALSE, 0, opcodes);

    /* load up the block */
    size_t start = dsarray_len(opcodes);
    BlockToOpCodes(forstmt->block, BLOCK_NO_PUSH_OR_POP, opcodes, values, idents);
    size_t end = dsarray_len(opcodes);

    /* reload the identifier, increment it, and go back */
    ExprToOpCodes(inc->ident, opcodes, values, idents);
    ExprToOpCodes(inc->step, opcodes, values, idents);
    PushOpCode(OPC_ADD, 0, opcodes);
    IdentSetToOpCodes(inc->ident, opcodes, values, idents, false);

    /* go back to the comparison at the beginning of the loop */
    PushOpCode(OPC_GOTO, (int)i, opcodes);
    PushOpCode(OPC_POP_BLOCK, 0, opcodes);

    /* update the conditional instruction with the index of the POP_BLOCK instruction */
    size_t pop = dsarray_len(opcodes) - 1;
    ms_VMOpCode *opcif = dsarray_get(opcodes, j);
    *opcif = ms_VMOpCodeWithArg(OPC_JUMP_IF_FALSE, (int)pop);

    /* update the BREAK and CONTINUE opcodes with the correct gotos */
    StmtForFixBreakAndContinue(opcodes, start, end, (int)pop, (int)i);
}

static void StmtForIterToOpCodes(const ms_StmtFor *forstmt, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(forstmt);
    assert(forstmt->type == FORSTMT_ITERATOR);
    assert(forstmt->clause.iter);
    assert(opcodes);
    assert(values);
    assert(idents);

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

    PushOpCode(OPC_PUSH_BLOCK, 0, opcodes);

    /* make sure we declare the name at the top of the block, so we can
     * jump back to an instruction after this one and don't renew the
     * name on each iteration */
    size_t new_index = 0;
    if (iter->declare) {
        new_index = dsarray_len(opcodes);
        PushOpCode(OPC_NEW_NAME, 0, opcodes);
    }

    size_t i = dsarray_len(opcodes);
    ExprToOpCodes(iter->iter, opcodes, values, idents);
    PushOpCode(OPC_NEXT, 0, opcodes);

    /* update the NEW_NAME opcode with the index of the identifier to new */
    if (iter->declare) {
        int index;
        IdentExprToOpCodes(iter->ident, &index, idents);
        PushOpCode(OPC_SET_NAME, index, opcodes);

        ms_VMOpCode *opc = dsarray_get(opcodes, new_index);
        *opc = ms_VMOpCodeWithArg(OPC_NEW_NAME, index);
    } else {
        IdentSetToOpCodes(iter->ident, opcodes, values, idents, false);
    }

    /* reload the expression value */
    ExprToOpCodes(iter->ident, opcodes, values, idents);

    /* compare to null */
    ms_Value *v = malloc(sizeof(ms_Value));     /* TODO: this leaks memory for now */
    if (!v) {
        return;
    }
    v->type = MSVAL_NULL;
    v->val.n = MS_VM_NULL_POINTER;

    int val_index;
    PushValue(v, &val_index, values);
    PushOpCode(OPC_PUSH, val_index, opcodes);
    PushOpCode(OPC_EQ, 0, opcodes);

    /* jump before the block if no value was returned from next */
    size_t j = dsarray_len(opcodes);
    PushOpCode(OPC_JUMP_IF_FALSE, 0, opcodes);

    /* load up the block */
    size_t start = dsarray_len(opcodes);
    BlockToOpCodes(forstmt->block, BLOCK_NO_PUSH_OR_POP, opcodes, values, idents);
    size_t end = dsarray_len(opcodes);

    /* go back to the comparison at the beginning of the loop */
    PushOpCode(OPC_GOTO, (int)i, opcodes);
    PushOpCode(OPC_POP_BLOCK, 0, opcodes);

    /* update the conditional instruction with the index of the POP_BLOCK instruction */
    size_t pop = dsarray_len(opcodes) - 1;
    ms_VMOpCode *opc = dsarray_get(opcodes, j);
    *opc = ms_VMOpCodeWithArg(OPC_JUMP_IF_FALSE, (int)pop);

    /* update the BREAK and CONTINUE opcodes with the correct gotos */
    StmtForFixBreakAndContinue(opcodes, start, end, (int)pop, (int)i);
}

static void StmtForExprToOpCodes(const ms_StmtFor *forstmt, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(forstmt);
    assert(forstmt->type == FORSTMT_EXPR);
    assert(forstmt->clause.expr);
    assert(opcodes);
    assert(values);
    assert(idents);

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

    PushOpCode(OPC_PUSH_BLOCK, 0, opcodes);

    size_t i = dsarray_len(opcodes);
    ExprToOpCodes(forstmt->clause.expr->expr, opcodes, values, idents);

    size_t j = dsarray_len(opcodes);
    PushOpCode(OPC_JUMP_IF_FALSE, 0, opcodes);

    size_t start = dsarray_len(opcodes);
    BlockToOpCodes(forstmt->block, BLOCK_NO_PUSH_OR_POP, opcodes, values, idents);
    size_t end = dsarray_len(opcodes);

    PushOpCode(OPC_GOTO, (int)i, opcodes);
    PushOpCode(OPC_POP_BLOCK, 0, opcodes);

    /* update the conditional instruction with the index of the POP_BLOCK instruction */
    size_t pop = dsarray_len(opcodes) - 1;
    ms_VMOpCode *opcif = dsarray_get(opcodes, j);
    *opcif = ms_VMOpCodeWithArg(OPC_JUMP_IF_FALSE, (int)pop);

    /* update the BREAK and CONTINUE opcodes with the correct gotos */
    StmtForFixBreakAndContinue(opcodes, start, end, (int)pop, (int)i);
}

static void StmtForFixBreakAndContinue(DSArray *opcodes, size_t start, size_t end, int break_arg, int cont_arg) {
    assert(opcodes);
    assert(start <= end);

    /* update BREAK and CONTINUE goto arguments in for loop blocks */
    for (size_t i = start; i < end; i++) {
        ms_VMOpCode *opc = dsarray_get(opcodes, i);
        ms_VMOpCodeType type = ms_VMOpCodeGetCode((*opc));

        switch(type) {
            case OPC_BREAK:
                *opc = ms_VMOpCodeWithArg(OPC_GOTO, break_arg);
                break;
            case OPC_CONTINUE:
                *opc = ms_VMOpCodeWithArg(OPC_GOTO, cont_arg);
                break;
            default:
                break;
        }
    }
}

static void StmtIfToOpCodes(const ms_StmtIf *ifstmt, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(ifstmt);
    assert(opcodes);
    assert(values);
    assert(idents);

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
    ExprToOpCodes(ifstmt->expr, opcodes, values, idents);

    /* if opcode and location */
    size_t i = dsarray_len(opcodes);
    PushOpCode(OPC_JUMP_IF_FALSE, 0, opcodes);

    /* push the entire IF block onto the opcode stack and get the new top index */
    BlockToOpCodes(ifstmt->block, BLOCK_INCL_PUSH | BLOCK_INCL_POP, opcodes, values, idents);
    size_t n = dsarray_len(opcodes);

    /* update IF opcode with n+1 argument */
    ms_VMOpCode *opcif = dsarray_get(opcodes, i);
    *opcif = ms_VMOpCodeWithArg(OPC_JUMP_IF_FALSE, (int)n+1);
    assert((n + 1) <= OPC_ARG_MAX);

    /* push the GOTO instruction on and cache its index */
    size_t j = dsarray_len(opcodes);
    PushOpCode(OPC_GOTO, 0, opcodes);

    /* push any opcodes associated with subordinate branches */
    if (ifstmt->elif) {
        StmtElseIfToOpCodes(ifstmt->elif, opcodes, values, idents);
    }

    /* update the original GOTO instruction argument */
    size_t eob = dsarray_len(opcodes) - i;
    ms_VMOpCode *opcgoto = dsarray_get(opcodes, j);
    *opcgoto = ms_VMOpCodeWithArg(OPC_GOTO, (int)eob);
    assert(eob <= OPC_ARG_MAX);
}

static void StmtElseIfToOpCodes(const ms_StmtIfElse *elif, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(elif);
    assert(opcodes);
    assert(values);
    assert(idents);

    switch (elif->type) {
        case IFELSE_IF:
            StmtIfToOpCodes(elif->clause.ifstmt, opcodes, values, idents);
            break;
        case IFELSE_ELSE:
            BlockToOpCodes(elif->clause.elstmt->block, BLOCK_INCL_PUSH | BLOCK_INCL_POP, opcodes, values, idents);
            break;
    }
}

static void StmtImportToOpCodes(const ms_StmtImport *import, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(import);
    assert(opcodes);
    assert(values);
    assert(idents);

    ExprToOpCodes(import->ident, opcodes, values, idents);
    PushOpCode(OPC_IMPORT, 0, opcodes);
    // TODO: alias the import
}

static void StmtMergeToOpCodes(const ms_StmtMerge *merge, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(merge);
    assert(opcodes);
    assert(values);
    assert(idents);

    ExprToOpCodes(merge->right, opcodes, values, idents);
    ExprToOpCodes(merge->left, opcodes, values, idents);
    PushOpCode(OPC_MERGE, 0, opcodes);
}

static void StmtReturnToOpCodes(const ms_StmtReturn *ret, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(ret);
    assert(opcodes);
    assert(values);
    assert(idents);

    ExprToOpCodes(ret->expr, opcodes, values, idents);
    PushOpCode(OPC_RETURN, 0, opcodes);
}

static void StmtAssignmentToOpCodes(const ms_StmtAssignment *assign, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(assign);
    assert(opcodes);
    assert(values);
    assert(idents);

    ExprToOpCodes(assign->expr, opcodes, values, idents);
    IdentSetToOpCodes(assign->ident, opcodes, values, idents, false);
}

static void StmtDeclarationToOpCodes(const ms_StmtDeclaration *decl, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(decl);
    assert(opcodes);
    assert(values);
    assert(idents);

    int index;
    PushIdent(decl->ident, &index, idents);
    PushOpCode(OPC_NEW_NAME, index, opcodes);

    if (decl->expr) {
        ExprToOpCodes(decl->expr, opcodes, values, idents);
        PushOpCode(OPC_SET_NAME, index, opcodes);
    }

    if (decl->next) {
        StmtDeclarationToOpCodes(decl->next, opcodes, values, idents);
    }
}

static void IdentSetToOpCodes(const ms_Expr *ident, DSArray *opcodes, DSArray *values, DSArray *idents, bool new_name) {
    assert(ident);
    assert(opcodes);
    assert(values);
    assert(idents);

    if (ms_ExprIsIdent(ident)) {
        int index;
        IdentExprToOpCodes(ident, &index, idents);
        if (new_name) {
            PushOpCode(OPC_NEW_NAME, index, opcodes);
        }
        PushOpCode(OPC_SET_NAME, index, opcodes);
    } else if (ms_ExprIsQualifiedIdent(ident)) {
        int nsubscripts = 0;
        QualifiedIdentExprToOpCodes(ident, &nsubscripts, opcodes, values, idents);
        PushOpCode(OPC_SET_ATTR, nsubscripts, opcodes);
    } else {
        assert(false && "assignment identifier is not an identifier");
    }
}

static void QualifiedIdentExprToOpCodes(const ms_Expr *ident, int *nsubscripts, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(ident);
    assert(ident->type == EXPRTYPE_BINARY);
    assert(ident->cmpnt.b->op == BINARY_GETATTR);
    assert(ms_ExprIsQualifiedIdent(ident));
    assert(opcodes);
    assert(values);
    assert(idents);

    const ms_ExprBinary *b = ident->cmpnt.b;
    if (b->rtype == EXPRATOM_EXPRLIST) {
        size_t len = dsarray_len(b->ratom.list);
        assert(len <= (size_t)OPC_ARG_MAX);
        *nsubscripts = (int)len;
    } else {
        *nsubscripts = 0;
    }

    ExprComponentToOpCodes(&b->latom, b->ltype, opcodes, values, idents);
    ExprComponentToOpCodes(&b->ratom, b->rtype, opcodes, values, idents);
    /* intentionally provide no opcode */
}

static void IdentExprToOpCodes(const ms_Expr *ident, int *index, DSArray *idents) {
    assert(ident);
    assert(ident->type == EXPRTYPE_UNARY);
    assert(ident->cmpnt.u->op == UNARY_NONE);
    assert(ident->cmpnt.u->type == EXPRATOM_IDENT);
    assert(ms_ExprIsIdent(ident));
    assert(idents);

    const ms_ExprUnary *u = ident->cmpnt.u;
    PushIdent(u->atom.ident, index, idents);
    /* intentionally provide no opcode */
}

static void ExprToOpCodes(const ms_Expr *expr, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(expr);
    assert(opcodes);
    assert(values);
    assert(idents);

    switch (expr->type) {
        case EXPRTYPE_UNARY:
            ExprUnaryToOpCodes(expr->cmpnt.u, opcodes, values, idents);
            break;
        case EXPRTYPE_BINARY:
            ExprBinaryToOpCodes(expr->cmpnt.b, opcodes, values, idents);
            break;
    }
}

static void ExprUnaryToOpCodes(const ms_ExprUnary *u, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(u);
    assert(opcodes);
    assert(values);
    assert(idents);

    ExprComponentToOpCodes(&u->atom, u->type, opcodes, values, idents);
    ExprUnaryOpToOpCode(u->op, opcodes);
}

static void ExprBinaryToOpCodes(const ms_ExprBinary *b, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(b);
    assert(opcodes);
    assert(values);
    assert(idents);

    switch (b->op) {
        case BINARY_CALL: {
            assert(b->rtype == EXPRATOM_EXPRLIST);
            ExprComponentToOpCodes(&b->ratom, b->rtype, opcodes, values, idents);
            ExprComponentToOpCodes(&b->latom, b->ltype, opcodes, values, idents);
            size_t len = dsarray_len(b->ratom.list);
            assert(len <= OPC_ARG_MAX);
            PushOpCode(OPC_CALL, (int) len, opcodes);
            break;
        }
        case BINARY_GETATTR: {
            assert((b->rtype == EXPRATOM_EXPRLIST) || (b->rtype == EXPRATOM_VALUE));
            ExprComponentToOpCodes(&b->latom, b->ltype, opcodes, values, idents);
            ExprComponentToOpCodes(&b->ratom, b->rtype, opcodes, values, idents);
            if (b->rtype == EXPRATOM_EXPRLIST) {
                size_t len = dsarray_len(b->ratom.list);
                assert(len <= OPC_ARG_MAX);
                PushOpCode(OPC_GET_ATTR, (int) len, opcodes);
            } else {
                PushOpCode(OPC_GET_ATTR, 0, opcodes);
            }
            break;
        }
        default:
            ExprComponentToOpCodes(&b->latom, b->ltype, opcodes, values, idents);
            ExprComponentToOpCodes(&b->ratom, b->rtype, opcodes, values, idents);
            ExprBinaryOpToOpCode(b->op, opcodes);
            break;
    }
}

static void ExprComponentToOpCodes(const ms_ExprAtom *a, ms_ExprAtomType type, DSArray *opcodes, DSArray *values, DSArray *idents) {
    assert(a);
    assert(opcodes);
    assert(values);
    assert(idents);

    switch (type) {
        case EXPRATOM_EXPRESSION:
            ExprToOpCodes(a->expr, opcodes, values, idents);
            break;
        case EXPRATOM_VALUE: {
            int index;
            PushValue(&a->val, &index, values);
            PushOpCode(OPC_PUSH, index, opcodes);
            break;
        }
        case EXPRATOM_IDENT: {
            int index;
            PushIdent(a->ident, &index, idents);
            PushOpCode(OPC_LOAD_NAME, index, opcodes);
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

static void ExprUnaryOpToOpCode(ms_ExprUnaryOp op, DSArray *opcodes) {
    assert(opcodes);

    ms_VMOpCode *o = malloc(sizeof(ms_VMOpCode));
    if (!o) { return; }

    switch (op) {
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

static void ExprBinaryOpToOpCode(ms_ExprBinaryOp op, DSArray *opcodes) {
    assert(opcodes);

    ms_VMOpCode *o = malloc(sizeof(ms_VMOpCode));
    if (!o) { return; }

    switch (op) {
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
            assert(false && "should not be generating call opcode here");
            break;
        case BINARY_GETATTR:
            assert(false && "should not be generating getattr opcode here");
            break;
        default:
            free(o);
            return;
    }

    dsarray_append(opcodes, o);
}

static void PushValue(const ms_Value *val, int *index, DSArray *values) {
    assert(val);
    assert(index);
    assert(values);

    ms_Value *v = malloc(sizeof(ms_Value));
    if (!v) {
        return;
    }

    // TODO: deep copy reference values
    *v = *val;

    dsarray_append(values, v);
    size_t nvals = dsarray_len(values);
    assert(nvals != 0);
    assert(nvals <= OPC_ARG_MAX);
    *index = (int)(nvals - 1);
}

static void PushIdent(const ms_Ident *ident, int *index, DSArray *idents) {
    assert(ident);
    assert(index);
    assert(idents);

    DSBuffer *name = dsbuf_dup(ident->name);
    if (!name) {
        return;
    }

    dsarray_append(idents, name);
    size_t nidents = dsarray_len(idents);
    assert(nidents != 0);
    assert(nidents <= OPC_ARG_MAX);
    *index = (int)(nidents - 1);
}

static void PushOpCode(ms_VMOpCodeType type, int arg, DSArray *opcodes) {
    assert(opcodes);
    assert(arg <= OPC_ARG_MAX);

    ms_VMOpCode *o = malloc(sizeof(ms_VMOpCode));
    if (!o) {
        return;
    }
    *o = ms_VMOpCodeWithArg(type, arg);
    dsarray_append(opcodes, o);
}
