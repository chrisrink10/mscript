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

#define FRAME_DATA_STACK_LIMIT_L (256)
#define VM_FRAME_STACK_LIMIT_L (256)
static const int FRAME_DATA_STACK_LIMIT = FRAME_DATA_STACK_LIMIT_L;
static const int VM_FRAME_STACK_LIMIT = VM_FRAME_STACK_LIMIT_L;
static const ms_Value EMPTY_STACK_VAL;

static const int MS_VM_NULL = 0;
const void *MS_VM_NULL_POINTER = &MS_VM_NULL;

typedef struct {
    size_t ip;                                      /* instruction pointer */
    size_t dp;                                      /* data stack pointer (points to index of NEXT push), current top is always (dp-1) */
    ms_VMByteCode *code;                            /* byte code for current frame */
    ms_Value data[FRAME_DATA_STACK_LIMIT_L];      /* frame data stack */
} ms_VMFrame;

struct ms_VM {
    DSArray *fstack;                                /* call stack frame */
    ms_VMError err;                                 /* current VM error */

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
static ms_VMExecResult VMFrameExecute(ms_VM *vm, ms_VMFrame *f);
static ms_Value *VMPeek(ms_VM *vm, int index);

static inline size_t VMPrint(ms_VM *vm);
static inline size_t VMPush(ms_VM *vm, int val);
static inline size_t VMPop(ms_VM *vm);
static inline size_t VMSwap(ms_VM *vm);
static inline size_t VMDoBinaryOp(ms_VM *vm, const char *name);
static inline size_t VMDoUnaryOp(ms_VM *vm, const char *name);
static inline size_t VMCallFunction(ms_VM *vm);
static inline size_t VMLoadName(ms_VM *vm, int arg);

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

    if (!VMGeneratePrototypes(vm)) {
        dsarray_destroy(vm->fstack);
        free(vm);
        return NULL;
    }

    vm->err.msg = NULL;
    return vm;
}

ms_VMExecResult ms_VMExecute(ms_VM *vm, ms_VMByteCode *bc, const ms_VMError **err) {
    if ((!vm) || (!bc)) {
        return VMEXEC_ERROR;
    }

    ms_VMFrame *newf = VMFrameNew(bc);
    if (!newf) {
        return VMEXEC_ERROR;
    }
    dsarray_append(vm->fstack, newf);

    ms_VMExecResult res = VMFrameExecute(vm, newf);
    *err = &vm->err;
    return res;
}

ms_VMExecResult ms_VMExecuteAndPrint(ms_VM *vm, ms_VMByteCode *bc, const ms_VMError **err) {
    if ((!vm) || (!bc)) {
        return VMEXEC_ERROR;
    }

    ms_VMFrame *newf = VMFrameNew(bc);
    if (!newf) {
        return VMEXEC_ERROR;
    }
    dsarray_append(vm->fstack, newf);

    ms_VMExecResult res = VMFrameExecute(vm, newf);
    if (res != VMEXEC_ERROR) {
        (void) VMPrint(vm);
    }
    *err = &vm->err;
    return res;
}

ms_Value *ms_VMTop(ms_VM *vm) {
    assert(vm);
    assert(vm->fstack);
    ms_VMFrame *f = dsarray_top(vm->fstack);
    assert(f);
    assert((f->dp - 1) != SIZE_MAX);
    return &f->data[f->dp - 1];
}

ms_Value ms_VMPop(ms_VM *vm) {
    assert(vm);
    assert(vm->fstack);
    ms_VMFrame *f = dsarray_top(vm->fstack);
    assert(f);
    assert((f->dp - 1) != SIZE_MAX);
    ms_Value val = f->data[f->dp - 1];
    f->data[f->dp] = EMPTY_STACK_VAL;
    f->dp--;
    return val;
}

void ms_VMErrorSet(ms_VM *vm, const char *msg, ...) {
    assert(vm);
    if (vm->err.msg) { free(vm->err.msg); }

    va_list args, args2;
    va_start(args, msg);
    va_copy(args2, args);
    int len = vsnprintf(NULL, 0, msg, args);
    vm->err.msg = malloc((size_t)len + 1);
    if (vm->err.msg) {
        vsnprintf(vm->err.msg, (size_t)len + 1, msg, args2);
    }
    va_end(args2);
    va_end(args);
}

void ms_VMPush(ms_VM *vm, ms_Value val) {
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
    ms_Value v;
    v.type = MSVAL_FLOAT;
    v.val.f = f;
    ms_VMPush(vm, v);
}

void ms_VMPushInt(ms_VM *vm, ms_ValInt i) {
    assert(vm);
    ms_Value v;
    v.type = MSVAL_INT;
    v.val.i = i;
    ms_VMPush(vm, v);
}

void ms_VMPushStr(ms_VM *vm, ms_ValStr *s) {
    assert(vm);
    ms_Value v;
    v.type = MSVAL_STR;
    v.val.s = s;
    ms_VMPush(vm, v);
}

void ms_VMPushStrL(ms_VM *vm, const char *s, size_t len) {
    assert(vm);
    DSBuffer *buf = dsbuf_new_l(s, len);
    if (!buf) { return; }
    ms_Value v;
    v.type = MSVAL_STR;
    v.val.s = buf;
    ms_VMPush(vm, v);
}

void ms_VMPushBool(ms_VM *vm, ms_ValBool b) {
    assert(vm);
    ms_Value v;
    v.type = MSVAL_BOOL;
    v.val.b = b;
    ms_VMPush(vm, v);
}

void ms_VMPushNull(ms_VM *vm) {
    assert(vm);
    ms_Value v;
    v.type = MSVAL_NULL;
    v.val.n = MS_VM_NULL_POINTER;
    ms_VMPush(vm, v);
}

void ms_VMSwap(ms_VM *vm) {
    (void) VMSwap(vm);
}

ms_Function ms_VMPrototypeFuncGet(ms_VM *vm, ms_ValDataType type, const char *method) {
    assert(vm);
    assert(vm->float_);
    assert(vm->int_);
    assert(vm->str);
    assert(vm->bool_);
    assert(vm->null);

    ms_FuncDef *def;
    switch (type) {
        case MSVAL_FLOAT:
            def = dsdict_get(vm->float_, (void *)method);
            return (def) ? def->func : NULL;
        case MSVAL_INT:
            def = dsdict_get(vm->int_, (void *)method);
            return (def) ? def->func : NULL;
        case MSVAL_STR:
            def = dsdict_get(vm->str, (void *)method);
            return (def) ? def->func : NULL;
        case MSVAL_BOOL:
            def = dsdict_get(vm->bool_, (void *)method);
            return (def) ? def->func : NULL;
        case MSVAL_NULL:
            def = dsdict_get(vm->null, (void *)method);
            return (def) ? def->func : NULL;
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
    free(vm->err.msg);
    vm->err.msg = NULL;
    free(vm);
}

bool ms_ValFloatIsInt(ms_ValFloat f, ms_ValInt *l) {
    if (fmod(f, 1.0) == 0.0) {
        *l = (ms_ValInt) f;
        return true;
    }
    *l = 0;
    return false;
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

ms_VMByteCode *ms_VMByteCodeNew(const DSArray *opcodes, const DSArray *values, const DSArray *idents) {
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

// Create a new VM stack frame
static ms_VMFrame *VMFrameNew(ms_VMByteCode *bc) {
    ms_VMFrame *f = malloc(sizeof(ms_VMFrame));
    if (!f) {
        return NULL;
    }

    memset(f, 0, sizeof(ms_VMFrame));
    f->code = bc;
    return f;
}

// Destroy the memory from a VM stack frame
static void VMFrameDestroy(ms_VMFrame *f) {
    if (!f) { return; }
    ms_VMByteCodeDestroy(f->code);  /* TODO: probably eventually cache bytecode */
    free(f);
}

// Execute the code in the current VM frame
static ms_VMExecResult VMFrameExecute(ms_VM *vm, ms_VMFrame *f) {
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
            case OPC_LOAD_NAME:
                inc = VMLoadName(vm, arg);
                break;
        }
        if (inc == 0) { return VMEXEC_ERROR; }
        f->ip += inc;
    }

    return VMEXEC_SUCCESS;
}

// Peek at a value a certain index of the stack without changing the pointer
// Negative values are relative to the top with (-1) indicating top, non-zero
// values indicate actual stack indices from the bottom (0)
static ms_Value *VMPeek(ms_VM *vm, int index) {
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

/*
 * OPCODE FUNCTIONS
 */

static inline size_t VMPrint(ms_VM *vm) {
    assert(vm);
    /* CHECK_VM_STACK(vm, 1); */
    ms_Value *v = ms_VMTop(vm);

    switch (v->type) {
        case MSVAL_FLOAT:
            printf("%f\n", v->val.f);
            break;
        case MSVAL_INT:
            printf("%lld\n", v->val.i);
            break;
        case MSVAL_BOOL:
            printf("%s\n", (v->val.b) ? "true" : "false");
            break;
        case MSVAL_NULL:
            printf("null\n");
            break;
        case MSVAL_STR:
            printf("%s\n", dsbuf_char_ptr(v->val.s));
            break;
    }
    return 1;
}

static inline size_t VMPush(ms_VM *vm, int arg) {
    assert(vm);
    assert(vm->fstack);
    ms_VMFrame *f = dsarray_top(vm->fstack);
    assert(f);
    f->data[f->dp] = f->code->values[arg];
    f->dp++;
    return 1;
}

static inline size_t VMPop(ms_VM *vm) {
    assert(vm);
    assert(vm->fstack);
    ms_VMFrame *f = dsarray_top(vm->fstack);
    assert(f);
    assert((f->dp - 1) != SIZE_MAX);
    f->data[f->dp - 1] = EMPTY_STACK_VAL;
    f->dp--;
    return 1;
}

static inline size_t VMSwap(ms_VM *vm) {
    assert(vm);
    ms_Value v2 = ms_VMPop(vm);
    ms_Value v1 = ms_VMPop(vm);
    ms_VMPush(vm, v2);
    ms_VMPush(vm, v1);
    return 1;
}

static inline size_t VMDoBinaryOp(ms_VM *vm, const char *name) {
    assert(vm);
    ms_Value *l = VMPeek(vm, -2);
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
    ms_Value *l = VMPeek(vm, -1);
    ms_Function op = ms_VMPrototypeFuncGet(vm, l->type, name);
    if (!op) {
        ms_VMErrorSet(vm, "Method '%s' not supported for this object.", name);
        return 0;
    }
    int res = op(vm);
    return (size_t)res;
}

static inline size_t VMCallFunction(ms_VM *vm) {
    assert(vm);
    ms_Value *l = VMPeek(vm, -1);
    ms_Function op = ms_VMPrototypeFuncGet(vm, l->type, "__call__");
    if (!op) {
        ms_VMErrorSet(vm, "Object is not callable.");
        return 0;
    }
    int res = op(vm);
    return (size_t)res;
}

static inline size_t VMLoadName(ms_VM *vm, int arg) {
    assert(vm);
    assert(vm->fstack);
    ms_VMFrame *f = dsarray_top(vm->fstack);
    assert(f->code);
    ms_Ident *id = f->code->idents[arg];
    assert(id);
    return 1;
}
