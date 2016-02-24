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

#include "libds/array.h"

/**
* @brief Virtual machine object for executing mscript byte code
*/
typedef struct ms_VM ms_VM;

/**
* @brief Enumeration of mscript VM opcodes
*/
typedef enum {
    OPC_PRINT,
    OPC_PUSH,
    OPC_POP,
    OPC_ADD,
    OPC_SUBTRACT,
    OPC_MULTIPLY,
    OPC_DIVIDE,
    OPC_IDIVIDE,
    OPC_MODULO,
    OPC_EXPONENTIATE,
    OPC_NEGATE,
} ms_VMOpCodeType;

/**
* @brief Placeholder for a more sophisticated object-value in the mscript VM
*/
typedef double ms_VMValue;

/**
* @brief Opcode and argument tuple for use in the VM
*/
typedef struct ms_VMOpCode {
    ms_VMOpCodeType type;
    ms_VMValue arg;
} ms_VMOpCode;

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
* @brief Convert a stack consisting of ms_VMOpCodes into a single
* @c ms_VMByteCode container.
*
* @param stack a @c DSArray with all @c ms_VMOpCode objects
* @returns an @c ms_VMByteCode container suitable for execution by the VM
*/
ms_VMByteCode *ms_VMOpCodesToByteCode(DSArray *stack);

/**
* @brief Execute a bytecode script on the mscript VM.
*
* @param VM a @c ms_VM object
* @param bc a @c ms_VMByteCode script container
*/
void ms_VMExecute(ms_VM *vm, ms_VMByteCode *bc);

/**
* @brief Destroy the memory held by a @c ms_VM
*
* @param VM a @c ms_VM object
*/
void ms_VMDestroy(ms_VM *vm);

#endif //MSCRIPT_VM_H
