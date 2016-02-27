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

/**
* @brief Virtual machine object for executing mscript byte code
*/
typedef struct ms_VM ms_VM;

/**
* @brief Function signature for VM C functions
*/
typedef int (*ms_Function)(ms_VM *vm);

/**
* @brief Prototype of all objects
*/
typedef struct {
    const char *name;
    ms_Function func;
} ms_FuncDef;

/*
* @brief Enumeration of VM result values
*/
typedef enum {
    VMEXEC_SUCCESS,
    VMEXEC_ERROR,
} ms_VMExecResult;

/*
* @brief Structure describing an error that occurred in the mscript VM
*/
typedef struct {
    char *msg;
} ms_VMError;

/**
* @brief Enumeration of mscript VM opcodes
*/
typedef enum {
    OPC_PRINT,
    OPC_PUSH,
    OPC_POP,
    OPC_SWAP,
    OPC_ADD,
    OPC_SUBTRACT,
    OPC_MULTIPLY,
    OPC_DIVIDE,
    OPC_IDIVIDE,
    OPC_MODULO,
    OPC_EXPONENTIATE,
    OPC_NEGATE,
    OPC_SHIFT_LEFT,
    OPC_SHIFT_RIGHT,
    OPC_BITWISE_AND,
    OPC_BITWISE_XOR,
    OPC_BITWISE_OR,
    OPC_BITWISE_NOT,
    OPC_LE,
    OPC_LT,
    OPC_GE,
    OPC_GT,
    OPC_EQ,
    OPC_NOT_EQ,
    OPC_NOT,
    OPC_AND,
    OPC_OR,
} ms_VMOpCodeType;

/**
* @brief The explicit NULL pointer for the mscript VM.
*/
extern const void *MS_VM_NULL_POINTER;

/*
* @brief Typedefs of VM values
*/
typedef double ms_VMFloat;
typedef long long ms_VMInt;
typedef DSBuffer ms_VMStr;
typedef bool ms_VMBool;
typedef const void ms_VMNull;

/**
* @brief Enumeration of primitive value types in the mscript VM
*/
typedef enum {
    VMVAL_FLOAT,
    VMVAL_INT,
    VMVAL_STR,
    VMVAL_BOOL,
    VMVAL_NULL,
} ms_VMPrimitiveType;

/**
* @brief Union of primitive types in the mscript VM
*/
typedef union {
    ms_VMFloat f;
    ms_VMInt i;
    ms_VMStr *s;
    ms_VMBool b;
    ms_VMNull *n;
} ms_VMPrimitive;

/**
* @brief Placeholder for a more sophisticated object-value in the mscript VM
*/
typedef struct {
    ms_VMPrimitiveType type;
    ms_VMPrimitive val;
} ms_VMValue;

/**
* @brief Opcode value
*/
typedef int ms_VMOpCode;

/**
* @brief Container for a full mscript bytecode script
*/
typedef struct ms_VMByteCode ms_VMByteCode;

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
ms_VMExecResult ms_VMExecute(ms_VM *vm, ms_VMByteCode *bc, const ms_VMError **err);

/**
* @brief Execute a bytecode script on the mscript VM and print any expression
* left on the data stack.
*/
ms_VMExecResult ms_VMExecuteAndPrint(ms_VM *vm, ms_VMByteCode *bc, const ms_VMError **err);

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
void ms_VMPushFloat(ms_VM *vm, ms_VMFloat f);

/*
* @brief Push an integer value onto the stack.
*/
void ms_VMPushInt(ms_VM *vm, ms_VMInt i);

/*
* @brief Push a string onto the stack.
*/
void ms_VMPushStr(ms_VM *vm, ms_VMStr *s);

/*
* @brief Push a C string onto the stack.
*/
void ms_VMPushStrL(ms_VM *vm, const char *s, size_t len);

/*
* @brief Push a boolean value onto the stack.
*/
void ms_VMPushBool(ms_VM *vm, ms_VMBool b);

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
ms_Function ms_VMPrototypeFuncGet(ms_VM *vm, ms_VMPrimitiveType type, const char *method);

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

/**
* @brief Encode an opcode with a numeric argument.
*/
ms_VMOpCode ms_VMOpCodeWithArg(ms_VMOpCodeType c, int arg);

/**
* @brief Decode the opcode argument from a full opcode
*/
int ms_VMOpCodeGetArg(ms_VMOpCode c);

/**
* @brief Decode the opcode type from a full opcode.
*/
ms_VMOpCodeType ms_VMOpCodeGetCode(ms_VMOpCode c);

/**
* @brief Convert a stack consisting of ms_VMOpCodes into a single
* @c ms_VMByteCode container.
*
* @param stack a @c DSArray with all @c ms_VMOpCode objects
* @returns an @c ms_VMByteCode container suitable for execution by the VM
*/
ms_VMByteCode *ms_VMByteCodeNew(const DSArray *opcodes, const DSArray *values);

/**
* @brief Destroy the memory held by byte code.
*
* @param bc a @c ms_VMByteCode object
*/
void ms_VMByteCodeDestroy(ms_VMByteCode *bc);

#endif //MSCRIPT_VM_H
