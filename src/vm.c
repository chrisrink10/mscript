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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "vm.h"

#define MAX_STACK_HEIGHT_L (256)
static const int MAX_STACK_HEIGHT = MAX_STACK_HEIGHT_L;

struct ms_VM {
    size_t ip;
    size_t dp;
    double data[MAX_STACK_HEIGHT_L];
};

struct ms_VMByteCode{
    ms_VMOpCode *code;
    size_t len;
};

static inline ms_VMValue VMPop(ms_VM *vm);
static inline size_t VMPrint(ms_VM *vm);
static inline size_t VMPush(ms_VM *vm, double val);
static inline size_t VMPopInst(ms_VM *vm);
static inline size_t VMAdd(ms_VM *vm);
static inline size_t VMSubtract(ms_VM *vm);
static inline size_t VMMultiply(ms_VM *vm);
static inline size_t VMDivide(ms_VM *vm);
static inline size_t VMIDivide(ms_VM *vm);
static inline size_t VMModulo(ms_VM *vm);
static inline size_t VMExponentiate(ms_VM *vm);
static inline size_t VMNegate(ms_VM *vm);

/*
 * PUBLIC FUNCTIONS
 */

ms_VM *ms_VMNew(void) {
    ms_VM *vm = malloc(sizeof(ms_VM));
    if (!vm) {
        return NULL;
    }

    vm->ip = 0;
    vm->dp = 0;
    return vm;
}

void ms_VMExecute(ms_VM *vm, ms_VMByteCode *bc) {
    assert(vm);
    if (!bc) { return; }

    for (vm->ip = 0; vm->ip < bc->len; ) {
        size_t inc = 0;
        ms_VMOpCode *code = &bc->code[vm->ip];
        switch (code->type) {
            case OPC_PRINT:         inc = VMPrint(vm);            break;
            case OPC_PUSH:          inc = VMPush(vm, code->arg);  break;
            case OPC_POP:           inc = VMPopInst(vm);          break;
            case OPC_ADD:           inc = VMAdd(vm);              break;
            case OPC_SUBTRACT:      inc = VMSubtract(vm);         break;
            case OPC_MULTIPLY:      inc = VMMultiply(vm);         break;
            case OPC_DIVIDE:        inc = VMDivide(vm);           break;
            case OPC_IDIVIDE:       inc = VMIDivide(vm);          break;
            case OPC_MODULO:        inc = VMModulo(vm);           break;
            case OPC_EXPONENTIATE:  inc = VMExponentiate(vm);     break;
            case OPC_NEGATE:        inc = VMNegate(vm);           break;
        }
        vm->ip += inc;
    }
}

void ms_VMDestroy(ms_VM *vm) {
    if (!vm) { return; }
    free(vm);
}

ms_VMByteCode *ms_VMOpCodesToByteCode(DSArray *stack) {
    if (!stack) { return NULL; }

    ms_VMByteCode *bc = malloc(sizeof(ms_VMByteCode));
    if (!bc) {
        return NULL;
    }

    bc->len = dsarray_len(stack);
    bc->code = malloc(sizeof(ms_VMOpCode) * (bc->len));
    if (!bc->code) {
        free(bc);
        return NULL;
    }

    DSIter *iter = dsarray_iter(stack);
    if (!iter) {
        free(bc->code);
        free(bc);
        return NULL;
    }

    while (dsiter_next(iter)) {
        size_t i = dsiter_index(iter);
        bc->code[i] = *(ms_VMOpCode *)dsiter_value(iter);
    }

    dsiter_destroy(iter);
    return bc;
}

void ms_VMByteCodeDestroy(ms_VMByteCode *bc) {
    if (!bc) { return; }
    free(bc->code);
    free(bc);
}

/*
 * PRIVATE FUNCTIONS
 */

// Peek at the top of the VM without moving the data pointer or popping
static inline ms_VMValue VMTop(ms_VM *vm) {
    assert(vm);
    assert(vm->dp > 0);
    return vm->data[vm->dp - 1];
}

// Utility stack pop moves data pointer, but cannot be used as
// a bytecode instruction since it will not return the number of
// instruction pointer places to move
static inline ms_VMValue VMPop(ms_VM *vm) {
    assert(vm);
    assert(vm->dp >= 0);
    double val = VMTop(vm);
    vm->dp--;
    return val;
}

/*
 * OPCODE FUNCTIONS
 */

static inline size_t VMPrint(ms_VM *vm) {
    assert(vm);
    printf("%f\n", VMTop(vm));
    return 1;
}

static inline size_t VMPush(ms_VM *vm, ms_VMValue val) {
    assert(vm);
    assert(vm->dp <= MAX_STACK_HEIGHT);
    vm->data[vm->dp] = val;
    vm->dp++;
    return 1;
}

static inline size_t VMPopInst(ms_VM *vm) {
    assert(vm);
    assert(vm->dp >= 0);
    ms_VMValue val = VMPop(vm);
    vm->dp--;
    return 1;
}

static inline size_t VMAdd(ms_VM *vm) {
    assert(vm);
    assert(vm->dp >= 2);
    ms_VMValue v2 = VMPop(vm);
    ms_VMValue v1 = VMPop(vm);
    VMPush(vm, v1 + v2);
    return 1;
}

static inline size_t VMSubtract(ms_VM *vm) {
    assert(vm);
    assert(vm->dp >= 2);
    ms_VMValue v2 = VMPop(vm);
    ms_VMValue v1 = VMPop(vm);
    VMPush(vm, v1 - v2);
    return 1;
}

static inline size_t VMMultiply(ms_VM *vm) {
    assert(vm);
    assert(vm->dp >= 2);
    ms_VMValue v2 = VMPop(vm);
    ms_VMValue v1 = VMPop(vm);
    VMPush(vm, v1 * v2);
    return 1;
}

static inline size_t VMDivide(ms_VM *vm) {
    assert(vm);
    assert(vm->dp >= 2);
    ms_VMValue v2 = VMPop(vm);
    ms_VMValue v1 = VMPop(vm);
    VMPush(vm, v1 / v2);
    return 1;
}

static inline size_t VMIDivide(ms_VM *vm) {
    assert(vm);
    assert(vm->dp >= 2);
    long long v2 = (long long)VMPop(vm);
    long long v1 = (long long)VMPop(vm);
    VMPush(vm, (ms_VMValue)(v1 / v2));
    return 1;
}

static inline size_t VMModulo(ms_VM *vm) {
    assert(vm);
    assert(vm->dp >= 2);
    ms_VMValue v2 = VMPop(vm);
    ms_VMValue v1 = VMPop(vm);
    VMPush(vm, fmod(v1, v2));
    return 1;
}

static inline size_t VMExponentiate(ms_VM *vm) {
    assert(vm);
    assert(vm->dp >= 2);
    ms_VMValue v2 = VMPop(vm);
    ms_VMValue v1 = VMPop(vm);
    VMPush(vm, pow(v1, v2));
    return 1;
}

static inline size_t VMNegate(ms_VM *vm) {
    assert(vm);
    assert(vm->dp >= 2);
    ms_VMValue v = VMPop(vm);
    VMPush(vm, -v);
    return 1;
}
