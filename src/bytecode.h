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

typedef enum {
    /*----------------------------------------------------------------------------------
    name                    arg         action
    ------------------------------------------------------------------------------------*/
    OPC_PRINT,          /*              print $str(TOS)                                 */
    OPC_PUSH,           /*  req         push values[i] to TOS                           */
    OPC_POP,            /*              pop TOS                                         */
    OPC_SWAP,           /*              TOS, TOS1 := TOS1, TOS                          */
    OPC_DUP,            /*              duplicate TOS, push duplicate to TOS            */
    OPC_ADD,            /*              TOS := TOS1 + TOS                               */
    OPC_SUBTRACT,       /*              TOS := TOS1 - TOS                               */
    OPC_MULTIPLY,       /*              TOS := TOS1 * TOS                               */
    OPC_DIVIDE,         /*              TOS := TOS1 / TOS                               */
    OPC_IDIVIDE,        /*              TOS := TOS1 \ TOS                               */
    OPC_MODULO,         /*              TOS := TOS1 % TOS                               */
    OPC_EXPONENTIATE,   /*              TOS := TOS1 ** TOS                              */
    OPC_NEGATE,         /*              TOS := -TOS                                     */
    OPC_SHIFT_LEFT,     /*              TOS := TOS1 << TOS                              */
    OPC_SHIFT_RIGHT,    /*              TOS := TOS1 >> TOS                              */
    OPC_BITWISE_AND,    /*              TOS := TOS1 & TOS                               */
    OPC_BITWISE_XOR,    /*              TOS := TOS1 ^ TOS                               */
    OPC_BITWISE_OR,     /*              TOS := TOS1 | TOS                               */
    OPC_BITWISE_NOT,    /*              TOS := ~TOS                                     */
    OPC_LE,             /*              TOS := TOS1 <= TOS                              */
    OPC_LT,             /*              TOS := TOS1 < TOS                               */
    OPC_GE,             /*              TOS := TOS1 >= TOS                              */
    OPC_GT,             /*              TOS := TOS1 > TOS                               */
    OPC_EQ,             /*              TOS := TOS1 == TOS                              */
    OPC_NOT_EQ,         /*              TOS := TOS1 != TOS                              */
    OPC_NOT,            /*              TOS := !TOS                                     */
    OPC_AND,            /*              TOS := TOS1 && TOS                              */
    OPC_OR,             /*              TOS := TOS1 || TOS                              */
    OPC_CALL,           /*  req         call TOS with arg # arguments                   */
    OPC_PUSH_BLOCK,     /*              push a new block context onto the frame         */
    OPC_POP_BLOCK,      /*              pop the top block context from the stack        */
    OPC_RETURN,         /*              return TOS to calling context                   */
    OPC_GET_ATTR,       /*  opt         TOS := TOS[TOS1, ...]                           */
    OPC_SET_ATTR,       /*  opt         TOS[TOS1, ...] := TOS2                          */
    OPC_DEL_ATTR,       /*  opt         delete TOS[TOS1, ...]                           */
    OPC_LOAD_NAME,      /*  req         TOS := env[names[i]]                            */
    OPC_NEW_NAME,       /*  req         env[names[i]] := null                           */
    OPC_SET_NAME,       /*  req         env[names[i]] := TOS1                           */
    OPC_DEL_NAME,       /*  req         delete env[names[i]]                            */
    OPC_NEXT,           /*              TOS := $next(TOS)                               */
    OPC_MERGE,          /*              merge TOS1 := TOS                               */
    OPC_IMPORT,         /*              ???                                             */
    OPC_JUMP_IF_FALSE,  /*  req         if not TOS goto arg                             */
    OPC_GOTO,           /*  req         ip := arg                                       */
    OPC_BREAK,          /*  req         break innermost loop (e.g. goto)                */
    OPC_CONTINUE,       /*  req         continue loop from start (e.g. goto)            */
} ms_VMOpCodeType;

typedef int ms_VMOpCode;

#ifndef NDEBUG
/* LLDB Type Summary:
 * type summary add --summary-string "(${var.type}, ${var.arg})" ms_VMOpCodeDebug
 */
typedef struct {
    ms_VMOpCodeType type    : 16;
    int arg                 : 16;
} ms_VMOpCodeDebug;
#endif

typedef struct {
    ms_VMOpCode *code;                              /* array of opcodes */
    ms_Value *values;                               /* array of VM values */
    ms_Ident **idents;                              /* array of identifiers */
    size_t nops;                                    /* number of opcodes */
    size_t nvals;                                   /* number of values */
    size_t nidents;                                 /* number of idents */
} ms_VMByteCode;

/**
* @brief Generate mscript VM bytecode from the given abstract syntax tree.
*/
ms_VMByteCode *ms_ASTToOpCodes(ms_AST *ast);

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
