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
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libds/array.h"
#include "libds/buffer.h"
#include "libds/dict.h"
#include "libds/hash.h"
#include "obj.h"
#include "vm.h"
#include "lang.h"

#define FRAME_DATA_STACK_LIMIT_L (256)
#define VM_FRAME_STACK_LIMIT_L (256)
static const size_t FRAME_DATA_STACK_LIMIT = FRAME_DATA_STACK_LIMIT_L;
static const size_t VM_FRAME_STACK_LIMIT = VM_FRAME_STACK_LIMIT_L;
static const size_t VM_FRAME_BLOCK_STACK_CAP = 10;
static const ms_VMValue EMPTY_STACK_VAL;

static const int MS_VM_NULL = 0;
const void *MS_VM_NULL_POINTER = &MS_VM_NULL;

static const char *const ERR_IF_EXPR_NOT_BOOL = "if statement expressions must be bool";
static const char *const ERR_NAME_NOT_DEFINED = "name '%s' not defined in the current scope";
static const char *const ERR_NOT_IMPLEMENTED = "not implemented";
static const char *const ERR_OUT_OF_MEMORY = "out of memory";

typedef struct {
    DSDict *env;                                    /* block level symbol table */
} ms_VMBlock;

typedef struct {
    size_t ip;                                      /* instruction pointer */
    size_t dp;                                      /* data stack pointer (points to index of NEXT push), current top is always (dp-1) */
    ms_VMByteCode *code;                            /* byte code for current frame */
    ms_VMValue data[FRAME_DATA_STACK_LIMIT_L];      /* frame data stack */
    DSArray *blocks;                                /* stack of frame blocks */
} ms_VMFrame;

struct ms_VM {
    DSArray *fstack;                                /* call stack frame */
    ms_Error **err;                                 /* pointer to current VM error (not owned by VM) */

    DSDict *env;                                    /* global namespace */

    DSDict *float_;                                 /* float primitive prototype */
    DSDict *int_;                                   /* int primitive prototype */
    DSDict *str;                                    /* string primitive prototype */
    DSDict *bool_;                                  /* bool primitive prototype */
    DSDict *null;                                   /* null primitive prototype */
    DSDict *obj;                                    /* object primitive prototype */
};

static bool VMGeneratePrototypes(ms_VM *vm);
static ms_VMFrame *VMFrameNew(ms_VMByteCode *bc);
static void VMFrameDestroy(ms_VMFrame *f);
static ms_VMBlock *VMBlockNew(void);
static void VMBlockDestroy(ms_VMBlock *blk);
static ms_Result VMFrameExecute(ms_VM *vm, ms_VMFrame *f);
static ms_VMValue *VMPeek(const ms_VM *vm, int index);
static bool VMStackIsEmpty(const ms_VM *vm);
static inline ms_VMFrame *VMCurrentFrame(const ms_VM *vm);
static inline DSDict *VMFindIdentEnv(const ms_VM *vm, const ms_VMFrame *f, DSBuffer *ident);

static inline size_t VMPrint(ms_VM *vm);
static inline size_t VMPush(ms_VM *vm, int val);
static inline size_t VMPop(ms_VM *vm);
static inline size_t VMSwap(ms_VM *vm);
static inline size_t VMDup(ms_VM *vm);
static inline size_t VMDoBinaryOp(ms_VM *vm, const char *name);
static inline size_t VMDoUnaryOp(ms_VM *vm, const char *name);
static inline size_t VMPushBlock(ms_VM *vm);
static inline size_t VMPopBlock(ms_VM *vm);
static inline size_t VMCallFunction(ms_VM *vm);
static inline bool VMJumpIfFalse(ms_VM *vm, int arg, size_t ip, size_t *dest);
static inline size_t VMLoadName(ms_VM *vm, int arg);
static inline size_t VMNewName(ms_VM *vm, int arg);
static inline size_t VMSetName(ms_VM *vm, int arg);
static inline size_t VMDelName(ms_VM *vm, int arg);

/*
 * PUBLIC FUNCTIONS
 */

ms_VM *ms_VMNew(void) {
    ms_VM *vm = malloc(sizeof(ms_VM));
    if (!vm) {
        return NULL;
    }

    vm->fstack = dsarray_new_cap(VM_FRAME_STACK_LIMIT, NULL,
                                 (dsarray_free_fn)VMFrameDestroy);
    if (!vm->fstack) {
        free(vm);
        return NULL;
    }

    vm->env = dsdict_new((dsdict_hash_fn)dsbuf_hash,
                         (dsdict_compare_fn)dsbuf_compare,
                         (dsdict_free_fn)dsbuf_destroy, NULL);
    if (!vm->env) {
        dsarray_destroy(vm->fstack);
        free(vm);
        return NULL;
    }

    if (!VMGeneratePrototypes(vm)) {
        dsarray_destroy(vm->fstack);
        dsdict_destroy(vm->env);
        free(vm);
        return NULL;
    }

    vm->err = NULL;
    return vm;
}

ms_Result ms_VMExecute(ms_VM *vm, ms_VMByteCode *bc, ms_Error **err) {
    if ((!vm) || (!bc)) {
        return MS_RESULT_ERROR;
    }

    *err = NULL;
    vm->err = err;

    ms_VMFrame *newf = VMFrameNew(bc);
    if (!newf) {
        return MS_RESULT_ERROR;
    }
    dsarray_append(vm->fstack, newf);

    ms_Result res = VMFrameExecute(vm, newf);
    return res;
}

ms_Result ms_VMExecuteAndPrint(ms_VM *vm, ms_VMByteCode *bc, ms_Error **err) {
    if ((!vm) || (!bc)) {
        return MS_RESULT_ERROR;
    }

    *err = NULL;
    vm->err = err;

    ms_VMFrame *newf = VMFrameNew(bc);
    if (!newf) {
        return MS_RESULT_ERROR;
    }
    dsarray_append(vm->fstack, newf);

    ms_Result res = VMFrameExecute(vm, newf);
    if (res != MS_RESULT_ERROR) {
        if (!VMStackIsEmpty(vm)) {
            (void) VMPrint(vm);
        }
    }
    return res;
}

ms_VMValue *ms_VMTop(ms_VM *vm) {
    assert(vm);
    assert(vm->fstack);
    ms_VMFrame *f = dsarray_top(vm->fstack);
    assert(f);
    assert((f->dp - 1) != SIZE_MAX);
    return &f->data[f->dp - 1];
}

ms_VMValue ms_VMPop(ms_VM *vm) {
    assert(vm);
    assert(vm->fstack);
    ms_VMFrame *f = dsarray_top(vm->fstack);
    assert(f);
    assert((f->dp - 1) != SIZE_MAX);
    ms_VMValue val = f->data[f->dp - 1];
    f->data[f->dp] = EMPTY_STACK_VAL;
    f->dp--;
    return val;
}

void ms_VMErrorSet(ms_VM *vm, const char *msg, ...) {
    assert(vm);

    ms_Error **err = vm->err;
    assert(!(*err));
    *err = malloc(sizeof(ms_Error));
    if (!(*err)) {
        return;
    }
    (*err)->type = MS_ERROR_VM;

    va_list args;
    va_list argscpy;
    va_start(args, msg);
    va_copy(argscpy, args);

    int len = vsnprintf(NULL, 0, msg, args);
    if (len < 0) {
        goto vm_close_error_va_args;
    }

    (*err)->len = (size_t)len;
    (*err)->msg = malloc((size_t)len + 1);
    if ((*err)->msg) {
        vsnprintf((*err)->msg, len + 1, msg, argscpy);
    }

vm_close_error_va_args:
    va_end(args);
    va_end(argscpy);
}

void ms_VMPush(ms_VM *vm, ms_VMValue val) {
    assert(vm);
    assert(vm->fstack);
    ms_VMFrame *f = dsarray_top(vm->fstack);
    assert(f);
    assert(f->dp < FRAME_DATA_STACK_LIMIT);
    f->data[f->dp] = val;
    f->dp++;
}

void ms_VMPushFloat(ms_VM *vm, ms_ValFloat f) {
    assert(vm);
    ms_VMValue v;
    v.type = VMVAL_FLOAT;
    v.val.f = f;
    ms_VMPush(vm, v);
}

void ms_VMPushInt(ms_VM *vm, ms_ValInt i) {
    assert(vm);
    ms_VMValue v;
    v.type = VMVAL_INT;
    v.val.i = i;
    ms_VMPush(vm, v);
}

void ms_VMPushStr(ms_VM *vm, ms_ValStr *s) {
    assert(vm);
    ms_VMValue v;
    v.type = VMVAL_STR;
    v.val.s = s;
    ms_VMPush(vm, v);
}

void ms_VMPushStrL(ms_VM *vm, const char *s, size_t len) {
    assert(vm);
    DSBuffer *buf = dsbuf_new_l(s, len);
    if (!buf) { return; }
    ms_VMValue v;
    v.type = VMVAL_STR;
    v.val.s = buf;
    ms_VMPush(vm, v);
}

void ms_VMPushBool(ms_VM *vm, ms_ValBool b) {
    assert(vm);
    ms_VMValue v;
    v.type = VMVAL_BOOL;
    v.val.b = b;
    ms_VMPush(vm, v);
}

void ms_VMPushNull(ms_VM *vm) {
    assert(vm);
    ms_VMValue v;
    v.type = VMVAL_NULL;
    v.val.n = MS_VM_NULL_POINTER;
    ms_VMPush(vm, v);
}

void ms_VMSwap(ms_VM *vm) {
    (void) VMSwap(vm);
}

ms_Function ms_VMPrototypeFuncGet(ms_VM *vm, ms_VMDataType type, const char *method) {
    assert(vm);
    assert(vm->float_);
    assert(vm->int_);
    assert(vm->str);
    assert(vm->bool_);
    assert(vm->null);

    ms_FuncDef *def;
    switch (type) {
        case VMVAL_FLOAT:
            def = dsdict_get(vm->float_, (void *)method);
            return (def) ? def->func : NULL;
        case VMVAL_INT:
            def = dsdict_get(vm->int_, (void *)method);
            return (def) ? def->func : NULL;
        case VMVAL_STR:
            def = dsdict_get(vm->str, (void *)method);
            return (def) ? def->func : NULL;
        case VMVAL_BOOL:
            def = dsdict_get(vm->bool_, (void *)method);
            return (def) ? def->func : NULL;
        case VMVAL_NULL:
            def = dsdict_get(vm->null, (void *)method);
            return (def) ? def->func : NULL;
        case VMVAL_FUNC:
            break;
    }

    assert(false && "Invalid VM type given.");
    return NULL;
}

void ms_VMClear(ms_VM *vm) {
    if (!vm) { return; }

    dsarray_clear(vm->fstack);
}

void ms_VMDestroy(ms_VM *vm) {
    if (!vm) { return; }
    dsdict_destroy(vm->float_);
    vm->float_ = NULL;
    dsdict_destroy(vm->int_);
    vm->int_ = NULL;
    dsdict_destroy(vm->str);
    vm->str = NULL;
    dsdict_destroy(vm->bool_);
    vm->bool_ = NULL;
    dsdict_destroy(vm->null);
    vm->null = NULL;
    dsarray_destroy(vm->fstack);
    vm->fstack = NULL;
    vm->err = NULL;
    free(vm);
}

bool ms_VMFloatIsInt(ms_VMFloat f, ms_VMInt *l) {
    if (fmod(f, 1.0) == 0.0) {
        *l = (ms_VMInt) f;
        return true;
    }
    *l = 0;
    return false;
}

/*
 * PRIVATE FUNCTIONS
 */

// Generate prototypes for the base/primitive objects objects
static bool VMGeneratePrototypes(ms_VM *vm) {
    assert(vm);

    vm->float_ = NULL;
    vm->int_ = NULL;
    vm->str = NULL;
    vm->bool_ = NULL;
    vm->null = NULL;

    vm->float_ = dsdict_new((dsdict_hash_fn)hash_fnv1,
                            (dsdict_compare_fn)strcmp, NULL, NULL);
    vm->int_ = dsdict_new((dsdict_hash_fn)hash_fnv1,
                          (dsdict_compare_fn)strcmp, NULL, NULL);
    vm->str = dsdict_new((dsdict_hash_fn)hash_fnv1,
                         (dsdict_compare_fn)strcmp, NULL, NULL);
    vm->bool_ = dsdict_new((dsdict_hash_fn)hash_fnv1,
                           (dsdict_compare_fn)strcmp, NULL, NULL);
    vm->null = dsdict_new((dsdict_hash_fn)hash_fnv1,
                          (dsdict_compare_fn)strcmp, NULL, NULL);

    if ((!vm->float_) || (!vm->int_) || (!vm->str) || (!vm->bool_) || (!vm->null)) {
        dsdict_destroy(vm->float_);
        dsdict_destroy(vm->int_);
        dsdict_destroy(vm->str);
        dsdict_destroy(vm->bool_);
        dsdict_destroy(vm->null);
        return false;
    }

    const ms_FuncDef *f = &MS_FLOAT_PROTOTYPE[0];
    while (f->name) {
        dsdict_put(vm->float_, (void *)f->name, (void *)f);
        f++;
    }

    const ms_FuncDef *i = &MS_INT_PROTOTYPE[0];
    while (i->name) {
        dsdict_put(vm->int_, (void *)i->name, (void *)i);
        i++;
    }

    const ms_FuncDef *s = &MS_STR_PROTOTYPE[0];
    while (s->name) {
        dsdict_put(vm->str, (void *)s->name, (void *)s);
        s++;
    }

    const ms_FuncDef *b = &MS_BOOL_PROTOTYPE[0];
    while (b->name) {
        dsdict_put(vm->bool_, (void *)b->name, (void *)b);
        b++;
    }

    const ms_FuncDef *n = &MS_NULL_PROTOTYPE[0];
    while (n->name) {
        dsdict_put(vm->null, (void *)n->name, (void *)n);
        n++;
    }

    return true;
}

static ms_VMFrame *VMFrameNew(ms_VMByteCode *bc) {
    ms_VMFrame *f = calloc(1, sizeof(ms_VMFrame));
    if (!f) {
        return NULL;
    }

    f->code = bc;
    f->blocks = dsarray_new_cap(VM_FRAME_BLOCK_STACK_CAP, NULL,
                                (dsarray_free_fn)VMBlockDestroy);
    if (!f->blocks) {
        VMFrameDestroy(f);
        return NULL;
    }

    ms_VMBlock *blk = VMBlockNew();
    if (!blk) {
        VMFrameDestroy(f);
        return NULL;
    }

    dsarray_append(f->blocks, blk);
    return f;
}

static void VMFrameDestroy(ms_VMFrame *f) {
    if (!f) { return; }
    ms_VMByteCodeDestroy(f->code);  /* TODO: probably eventually cache bytecode */
    f->code = NULL;
    dsarray_destroy(f->blocks);
    f->blocks = NULL;
    free(f);
}

static ms_VMBlock *VMBlockNew(void) {
    ms_VMBlock *blk = malloc(sizeof(ms_VMBlock));
    if (!blk) {
        return NULL;
    }

    blk->env = dsdict_new((dsdict_hash_fn)dsbuf_hash,
                          (dsdict_compare_fn)dsbuf_compare,
                          NULL,      /* keys are stored in ms_VMByteCode structure and are freed by a separate function */
                          NULL);     // TODO: this should have a free function that decrements the (future) reference counter
    if (!blk->env) {
        free(blk);
        return NULL;
    }

    return blk;
}

static void VMBlockDestroy(ms_VMBlock *blk) {
    if (!blk) { return; }
    dsdict_destroy(blk->env);
    blk->env = NULL;
    free(blk);
}

static ms_Result VMFrameExecute(ms_VM *vm, ms_VMFrame *f) {
    assert(vm);
    assert(f);

    ms_VMByteCode *bc = f->code;
    for (f->ip = 0; f->ip < f->code->nops; ) {
        size_t inc = 0;
        ms_VMOpCodeType code = ms_VMOpCodeGetCode(bc->code[f->ip]);
        int arg = ms_VMOpCodeGetArg(bc->code[f->ip]);
        switch (code) {
            case OPC_PRINT:
                inc = VMPrint(vm);
                break;
            case OPC_PUSH:
                inc = VMPush(vm, arg);
                break;
            case OPC_POP:
                inc = VMPop(vm);
                break;
            case OPC_SWAP:
                inc = VMSwap(vm);
                break;
            case OPC_DUP:
                inc = VMDup(vm);
                break;
            case OPC_ADD:
                inc = VMDoBinaryOp(vm, "__add__");
                break;
            case OPC_SUBTRACT:
                inc = VMDoBinaryOp(vm, "__sub__");
                break;
            case OPC_MULTIPLY:
                inc = VMDoBinaryOp(vm, "__mult__");
                break;
            case OPC_DIVIDE:
                inc = VMDoBinaryOp(vm, "__div__");
                break;
            case OPC_IDIVIDE:
                inc = VMDoBinaryOp(vm, "__idiv__");
                break;
            case OPC_MODULO:
                inc = VMDoBinaryOp(vm, "__mod__");
                break;
            case OPC_EXPONENTIATE:
                inc = VMDoBinaryOp(vm, "__exp__");
                break;
            case OPC_NEGATE:
                inc = VMDoUnaryOp(vm, "__neg__");
                break;
            case OPC_SHIFT_LEFT:
                inc = VMDoBinaryOp(vm, "__lshift__");
                break;
            case OPC_SHIFT_RIGHT:
                inc = VMDoBinaryOp(vm, "__rshift__");
                break;
            case OPC_BITWISE_AND:
                inc = VMDoBinaryOp(vm, "__band__");
                break;
            case OPC_BITWISE_XOR:
                inc = VMDoBinaryOp(vm, "__bxor__");
                break;
            case OPC_BITWISE_OR:
                inc = VMDoBinaryOp(vm, "__bor__");
                break;
            case OPC_BITWISE_NOT:
                inc = VMDoUnaryOp(vm, "__bnot__");
                break;
            case OPC_LE:
                inc = VMDoBinaryOp(vm, "__le__");
                break;
            case OPC_LT:
                inc = VMDoBinaryOp(vm, "__lt__");
                break;
            case OPC_GE:
                inc = VMDoBinaryOp(vm, "__ge__");
                break;
            case OPC_GT:
                inc = VMDoBinaryOp(vm, "__gt__");
                break;
            case OPC_EQ:
                inc = VMDoBinaryOp(vm, "__eq__");
                break;
            case OPC_NOT_EQ:
                inc = VMDoBinaryOp(vm, "__ne__");
                break;
            case OPC_NOT:
                inc = VMDoUnaryOp(vm, "__not__");
                break;
            case OPC_AND:
                inc = VMDoBinaryOp(vm, "__and__");
                break;
            case OPC_OR:
                inc = VMDoBinaryOp(vm, "__or__");
                break;
            case OPC_CALL:
                inc = VMCallFunction(vm);
                break;
            case OPC_CALL_BUILTIN:
                ms_VMErrorSet(vm, ERR_NOT_IMPLEMENTED);
                inc = 0;
                break;
            case OPC_PUSH_BLOCK:
                inc = VMPushBlock(vm);
                break;
            case OPC_POP_BLOCK:
                inc = VMPopBlock(vm);
                break;
            case OPC_RETURN:
                ms_VMErrorSet(vm, ERR_NOT_IMPLEMENTED);
                inc = 0;
                break;
            case OPC_GET_ATTR:
                inc = VMDoBinaryOp(vm, "__getattr__");
                break;
            case OPC_SET_ATTR:
                inc = VMDoBinaryOp(vm, "__setattr__");
                break;
            case OPC_DEL_ATTR:
                inc = VMDoBinaryOp(vm, "__delattr__");
                break;
            case OPC_GET_GLO:
                ms_VMErrorSet(vm, ERR_NOT_IMPLEMENTED);
                inc = 0;
                break;
            case OPC_SET_GLO:
                ms_VMErrorSet(vm, ERR_NOT_IMPLEMENTED);
                inc = 0;
                break;
            case OPC_DEL_GLO:
                ms_VMErrorSet(vm, ERR_NOT_IMPLEMENTED);
                inc = 0;
                break;
            case OPC_GET_NAME:
                inc = VMLoadName(vm, arg);
                break;
            case OPC_NEW_NAME:
                inc = VMNewName(vm, arg);
                break;
            case OPC_SET_NAME:
                inc = VMSetName(vm, arg);
                break;
            case OPC_DEL_NAME:
                inc = VMDelName(vm, arg);
                break;
            case OPC_MAKE_LIST:
                ms_VMErrorSet(vm, ERR_NOT_IMPLEMENTED);
                inc = 0;
                break;
            case OPC_MAKE_OBJ:
                ms_VMErrorSet(vm, ERR_NOT_IMPLEMENTED);
                inc = 0;
                break;
            case OPC_NEXT:
                ms_VMErrorSet(vm, ERR_NOT_IMPLEMENTED);
                inc = 0;
                break;
            case OPC_IMPORT:
                ms_VMErrorSet(vm, ERR_NOT_IMPLEMENTED);
                inc = 0;
                break;
            case OPC_JUMP_IF_FALSE: {
                size_t dest;
                if (VMJumpIfFalse(vm, arg, f->ip, &dest)) {
                    f->ip = dest;
                    continue;
                }
                return MS_RESULT_ERROR;
            }
            case OPC_GOTO:              /* fall through */
            case OPC_BREAK:             /* fall through */
            case OPC_CONTINUE:
                f->ip = (size_t)arg;
                continue;
        }
        if (inc == 0) { return MS_RESULT_ERROR; }
        f->ip += inc;
    }

    return MS_RESULT_SUCCESS;
}

// Peek at a value a certain index of the stack without changing the pointer
// Negative values are relative to the top with (-1) indicating top, non-zero
// values indicate actual stack indices from the bottom (0)
static ms_VMValue *VMPeek(const ms_VM *vm, int index) {
    assert(vm);
    assert(vm->fstack);
    ms_VMFrame *f = dsarray_top(vm->fstack);
    assert(f);

    if (index < 0) {
        assert(((int)f->dp + index) >= 0);
        return &f->data[f->dp + index];
    } else {
        assert((size_t)index < f->dp);
        return &f->data[index];
    }
}

static bool VMStackIsEmpty(const ms_VM *vm) {
    assert(vm);
    assert(vm->fstack);
    ms_VMFrame *f = dsarray_top(vm->fstack);
    assert(f);
    return (f->dp == 0);
}

static inline ms_VMFrame *VMCurrentFrame(const ms_VM *vm) {
    assert(vm);
    assert(vm->fstack);
    return dsarray_top(vm->fstack);
}

static inline DSDict *VMFindIdentEnv(const ms_VM *vm, const ms_VMFrame *f, DSBuffer *ident) {
    assert(vm);
    assert(f);

    size_t nblocks = dsarray_len(f->blocks);
    for (size_t i = nblocks - 1; i < nblocks; i--) {        /* loop (nblocks - 1) to 0 with*/
        ms_VMBlock *blk = dsarray_get(f->blocks, i);
        ms_VMValue *v = dsdict_get(blk->env, ident);
        if (v) {
            return blk->env;
        }
    }

    return vm->env;
}

/*
 * OPCODE FUNCTIONS
 */

static inline size_t VMPrint(ms_VM *vm) {
    assert(vm);
    ms_VMValue *v = ms_VMTop(vm);

    switch (v->type) {
        case VMVAL_FLOAT:
            printf("%f\n", v->val.f);
            break;
        case VMVAL_INT:
            printf("%lld\n", v->val.i);
            break;
        case VMVAL_BOOL:
            printf("%s\n", (v->val.b) ? "true" : "false");
            break;
        case VMVAL_NULL:
            printf("null\n");
            break;
        case VMVAL_STR:
            printf("%s\n", dsbuf_char_ptr(v->val.s));
            break;
        case VMVAL_FUNC:
            printf("<func %p>\n", (void *)v->val.fn);
            break;
    }
    return 1;
}

static inline size_t VMPush(ms_VM *vm, int arg) {
    assert(vm);
    ms_VMFrame *f = VMCurrentFrame(vm);
    assert(f);
    f->data[f->dp] = f->code->values[arg];
    f->dp++;
    return 1;
}

static inline size_t VMPop(ms_VM *vm) {
    assert(vm);
    ms_VMFrame *f = VMCurrentFrame(vm);
    assert(f);
    assert((f->dp - 1) != SIZE_MAX);
    f->data[f->dp - 1] = EMPTY_STACK_VAL;
    f->dp--;
    return 1;
}

static inline size_t VMSwap(ms_VM *vm) {
    assert(vm);
    ms_VMValue v2 = ms_VMPop(vm);
    ms_VMValue v1 = ms_VMPop(vm);
    ms_VMPush(vm, v2);
    ms_VMPush(vm, v1);
    return 1;
}

static inline size_t VMDup(ms_VM *vm) {
    assert(vm);
    ms_VMFrame *f = VMCurrentFrame(vm);
    assert(f);
    ms_VMValue v = *ms_VMTop(vm);
    ms_VMPush(vm, v);
    return 1;
}

static inline size_t VMDoBinaryOp(ms_VM *vm, const char *name) {
    assert(vm);
    ms_VMValue *l = VMPeek(vm, -2);
    ms_Function op = ms_VMPrototypeFuncGet(vm, l->type, name);
    if (!op) {
        ms_VMErrorSet(vm, "Method '%s' not supported for this object.", name);
        return 0;
    }
    int res = op(vm);
    return (size_t)res;
}

static inline size_t VMDoUnaryOp(ms_VM *vm, const char *name) {
    assert(vm);
    ms_VMValue *l = VMPeek(vm, -1);
    ms_Function op = ms_VMPrototypeFuncGet(vm, l->type, name);
    if (!op) {
        ms_VMErrorSet(vm, "Method '%s' not supported for this object.", name);
        return 0;
    }
    int res = op(vm);
    return (size_t)res;
}

static inline size_t VMPushBlock(ms_VM *vm) {
    assert(vm);
    ms_VMFrame *f = VMCurrentFrame(vm);
    assert(f);
    assert(f->blocks);
    ms_VMBlock *blk = VMBlockNew();
    assert(blk);
    dsarray_append(f->blocks, blk);
    return 1;
}

static inline size_t VMPopBlock(ms_VM *vm) {
    assert(vm);
    ms_VMFrame *f = VMCurrentFrame(vm);
    assert(f);
    assert(f->blocks);
    ms_VMBlock *blk = dsarray_pop(f->blocks);
    VMBlockDestroy(blk);
    return 1;
}

static inline size_t VMCallFunction(ms_VM *vm) {
    assert(vm);
    ms_VMValue *l = VMPeek(vm, -1);
    ms_Function op = ms_VMPrototypeFuncGet(vm, l->type, "__call__");
    if (!op) {
        ms_VMErrorSet(vm, "Object is not callable.");
        return 0;
    }
    int res = op(vm);
    return (size_t)res;
}

static inline bool VMJumpIfFalse(ms_VM *vm, int arg, size_t ip, size_t *dest) {
    assert(vm);

    ms_VMValue *l = VMPeek(vm, -1);
    if (l->type != VMVAL_BOOL) {
        ms_VMErrorSet(vm, ERR_IF_EXPR_NOT_BOOL);
        return false;
    }

    if (l->val.b) {
        *dest = ip;
    } else {
        *dest = (size_t)arg;
    }

    return true;
}

static inline size_t VMLoadName(ms_VM *vm, int arg) {
    assert(vm);
    assert(arg >= 0);
    ms_VMFrame *f = VMCurrentFrame(vm);
    assert(f);
    assert(f->code);
    DSBuffer *id = f->code->idents[arg];
    assert(id);

    DSDict *env = VMFindIdentEnv(vm, f, id);
    if (!env) {
        ms_VMErrorSet(vm, ERR_NAME_NOT_DEFINED, dsbuf_char_ptr(id));
        return 0;
    }

    ms_VMValue *v = dsdict_get(env, id);
    ms_VMPush(vm, *v);
    return 1;
}

static inline size_t VMNewName(ms_VM *vm, int arg) {
    assert(vm);
    assert(arg >= 0);
    ms_VMFrame *f = VMCurrentFrame(vm);
    assert(f);
    assert(f->code);
    DSBuffer *id = f->code->idents[arg];
    assert(id);

    ms_VMValue *v = malloc(sizeof(ms_VMValue));
    if (!v) {
        ms_VMErrorSet(vm, ERR_OUT_OF_MEMORY);
        return 0;
    }
    v->type = VMVAL_NULL;
    v->val.n = MS_VM_NULL_POINTER;

    ms_VMBlock *blk = dsarray_top(f->blocks);
    dsdict_put(blk->env, id, v);
    return 1;
}

static inline size_t VMSetName(ms_VM *vm, int arg) {
    assert(vm);
    assert(arg >= 0);
    ms_VMFrame *f = VMCurrentFrame(vm);
    assert(f);
    assert(f->code);
    DSBuffer *id = f->code->idents[arg];
    assert(id);

    DSDict *env = VMFindIdentEnv(vm, f, id);
    assert(env);

    ms_VMValue *v = malloc(sizeof(ms_VMValue));
    if (!v) {
        ms_VMErrorSet(vm, ERR_OUT_OF_MEMORY);
        return 0;
    }

    *v = ms_VMPop(vm);
    // TODO: decrement reference count on the previous value
    dsdict_put(env, id, v);
    return 1;
}

static inline size_t VMDelName(ms_VM *vm, int arg) {
    assert(vm);
    assert(arg >= 0);
    ms_VMFrame *f = VMCurrentFrame(vm);
    assert(f);
    assert(f->code);
    DSBuffer *id = f->code->idents[arg];
    assert(id);

    DSDict *env = VMFindIdentEnv(vm, f, id);
    if (!env) {
        ms_VMErrorSet(vm, ERR_NAME_NOT_DEFINED, dsbuf_char_ptr(id));
        return 0;
    }

    dsdict_del(env, id);
    return 1;
}
