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

#ifndef MSCRIPT_VM_H
#define MSCRIPT_VM_H

#include <stdbool.h>
#include "libds/array.h"
#include "libds/buffer.h"
#include "bytecode.h"
#include "error.h"
#include "lang.h"

typedef struct ms_VM ms_VM;

typedef int (*ms_Function)(ms_VM *vm);

typedef struct {
    const char *name;
    ms_Function func;
} ms_FuncDef;

extern const void *MS_VM_NULL_POINTER;

/**
* @brief Create a new mscript VM.
*
* @returns a @c ms_VM object
*/
ms_VM *ms_VMNew(void);

/**
* @brief Execute a bytecode script on the mscript VM.
*
* @param VM a @c ms_VM object
* @param bc a @c ms_VMByteCode script container
*/
ms_Result ms_VMExecute(ms_VM *vm, ms_VMByteCode *bc, ms_Error **err);

/**
* @brief Execute a bytecode script on the mscript VM and print any expression
* left on the data stack.
*/
ms_Result ms_VMExecuteAndPrint(ms_VM *vm, ms_VMByteCode *bc, ms_Error **err);

/**
* @brief Peek at the top data value on the current VM frame.
*/
ms_VMValue *ms_VMTop(ms_VM *vm);

/*
* @brief Pop the top value off the data stack on the current VM frame.
*/
ms_VMValue ms_VMPop(ms_VM *vm);

/**
* @brief Set the VM error message.
*/
void ms_VMErrorSet(ms_VM *vm, const char *msg, ...);

/*
* @brief Push a new value onto the data stack of the current VM frame.
*/
void ms_VMPush(ms_VM *vm, ms_VMValue val);

/*
* @brief Push a floating point value onto the stack.
*/
void ms_VMPushFloat(ms_VM *vm, ms_ValFloat f);

/*
* @brief Push an integer value onto the stack.
*/
void ms_VMPushInt(ms_VM *vm, ms_ValInt i);

/*
* @brief Push a string onto the stack.
*/
void ms_VMPushStr(ms_VM *vm, ms_ValStr *s);

/*
* @brief Push a C string onto the stack.
*/
void ms_VMPushStrL(ms_VM *vm, const char *s, size_t len);

/*
* @brief Push a boolean value onto the stack.
*/
void ms_VMPushBool(ms_VM *vm, ms_ValBool b);

/*
* @brief Push a null value onto the stack.
*/
void ms_VMPushNull(ms_VM *vm);

/**
* @brief Swap the ordering of the top two elements on the stack.
*/
void ms_VMSwap(ms_VM *vm);

/**
* @brief Get a function pointer for the given primitive type and method.
*/
ms_Function ms_VMPrototypeFuncGet(ms_VM *vm, ms_VMDataType type, const char *method);

/**
* @brief Clear the data stack and reset the instruction pointer.
*
* @param vm a @c ms_VM object
*/
void ms_VMClear(ms_VM *vm);

/**
* @brief Destroy the memory held by a @c ms_VM
*
* @param VM a @c ms_VM object
*/
void ms_VMDestroy(ms_VM *vm);

/*
* @brief Check if the floating point value is actually an integer.
*
* @param f a floating point value
* @param l a pointer to an integer which will be filled if @c f contains an int
* @returns true if @c f contains an integer; false otherwise
*/
bool ms_VMFloatIsInt(ms_VMFloat f, ms_VMInt *l);

#endif //MSCRIPT_VM_H
