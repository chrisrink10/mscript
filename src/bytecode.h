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

#ifndef MSCRIPT_BYTECODE_H
#define MSCRIPT_BYTECODE_H

#include <stddef.h>
#include "lang.h"

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
    OPC_CALL,
    OPC_LOAD_NAME,
} ms_VMOpCodeType;

/**
* @brief Opcode value
*/
typedef int ms_VMOpCode;

/**
* @brief Container for a full mscript bytecode script
*/
typedef struct {
    ms_VMOpCode *code;                              /* array of opcodes */
    ms_Value *values;                               /* array of VM values */
    ms_Ident **idents;                              /* array of identifiers */
    size_t nops;                                    /* number of opcodes */
    size_t nvals;                                   /* number of values */
    size_t nidents;                                 /* number of idents */
} ms_VMByteCode;

/**
* @brief Generate mscript VM bytecode from the given expression value.
*/
ms_VMByteCode *ms_ExprToOpCodes(ms_Expr *expr);

/**
* @brief Destroy the memory held by byte code.
*
* @param bc a @c ms_VMByteCode object
*/
void ms_VMByteCodeDestroy(ms_VMByteCode *bc);

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

#endif //MSCRIPT_BYTECODE_H
