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
    /*--------------------------------------------------------------------------------------------------------------------------
    name                    arg         action                                          detail
    ----------------------------------------------------------------------------------------------------------------------------*/
    OPC_PRINT,          /*              print $str(TOS)                                                                         */
    OPC_PUSH,           /*  req         push values[i] to TOS                                                                   */
    OPC_POP,            /*              pop TOS                                                                                 */
    OPC_SWAP,           /*              TOS, TOS1 := TOS1, TOS                                                                  */
    OPC_DUP,            /*              duplicate TOS, push duplicate to TOS                                                    */
    OPC_ADD,            /*              TOS := TOS1 + TOS                                                                       */
    OPC_SUBTRACT,       /*              TOS := TOS1 - TOS                                                                       */
    OPC_MULTIPLY,       /*              TOS := TOS1 * TOS                                                                       */
    OPC_DIVIDE,         /*              TOS := TOS1 / TOS                                                                       */
    OPC_IDIVIDE,        /*              TOS := TOS1 \ TOS                                                                       */
    OPC_MODULO,         /*              TOS := TOS1 % TOS                                                                       */
    OPC_EXPONENTIATE,   /*              TOS := TOS1 ** TOS                                                                      */
    OPC_NEGATE,         /*              TOS := -TOS                                                                             */
    OPC_SHIFT_LEFT,     /*              TOS := TOS1 << TOS                                                                      */
    OPC_SHIFT_RIGHT,    /*              TOS := TOS1 >> TOS                                                                      */
    OPC_BITWISE_AND,    /*              TOS := TOS1 & TOS                                                                       */
    OPC_BITWISE_XOR,    /*              TOS := TOS1 ^ TOS                                                                       */
    OPC_BITWISE_OR,     /*              TOS := TOS1 | TOS                                                                       */
    OPC_BITWISE_NOT,    /*              TOS := ~TOS                                                                             */
    OPC_LE,             /*              TOS := TOS1 <= TOS                                                                      */
    OPC_LT,             /*              TOS := TOS1 < TOS                                                                       */
    OPC_GE,             /*              TOS := TOS1 >= TOS                                                                      */
    OPC_GT,             /*              TOS := TOS1 > TOS                                                                       */
    OPC_EQ,             /*              TOS := TOS1 == TOS                                                                      */
    OPC_NOT_EQ,         /*              TOS := TOS1 != TOS                                                                      */
    OPC_NOT,            /*              TOS := !TOS                                                                             */
    OPC_AND,            /*              TOS := TOS1 && TOS                                                                      */
    OPC_OR,             /*              TOS := TOS1 || TOS                                                                      */
    OPC_CALL,           /*  req         call TOS with arg # arguments                                                           */
    OPC_CALL_BUILTIN,   /*  req         call builtin names[i]                                                                   */
    OPC_PUSH_BLOCK,     /*              push a new block context onto the frame                                                 */
    OPC_POP_BLOCK,      /*              pop the top block context from the stack                                                */
    OPC_RETURN,         /*              return TOS to calling context                                                           */
    OPC_GET_ATTR,       /*  opt         TOS := TOS[TOS1, ...]                                                                   */
    OPC_SET_ATTR,       /*  opt         TOS[TOS1, ...] := TOSN                                                                  */
    OPC_DEL_ATTR,       /*  opt         delete TOS[TOS1, ...]                                                                   */
    OPC_GET_GLO,        /*  req         TOS := db[..., TOS1, TOS]                                                               */
    OPC_SET_GLO,        /*  req         db[..., TOS1, TOS] := TOSN                                                              */
    OPC_DEL_GLO,        /*  req         delete db[..., TOS1, TOS]                                                               */
    OPC_NEW_NAME,       /*  req         env[names[i]] := null                                                                   */
    OPC_GET_NAME,       /*  req         TOS := env[names[i]]                                                                    */
    OPC_SET_NAME,       /*  req         env[names[i]] := TOS                                                                    */
    OPC_DEL_NAME,       /*  req         delete env[names[i]]                                                                    */
    OPC_NEXT,           /*              TOS := $next(TOS)                                                                       */
    OPC_MERGE,          /*              merge TOS1 := TOS                                                                       */
    OPC_IMPORT,         /*  opt         TOS := import TOS.TOS1...                                                               */
    OPC_JUMP_IF_FALSE,  /*  req         if not TOS goto arg                                                                     */
    OPC_GOTO,           /*  req         ip := arg                                                                               */
    OPC_BREAK,          /*  req         break innermost loop (e.g. goto)                converted to GOTO w/ arg                */
    OPC_CONTINUE,       /*  req         continue loop from start (e.g. goto)            converted to GOTO w/ arg                */
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

typedef struct ms_VMByteCode ms_VMByteCode;
typedef double ms_VMFloat;
typedef long long ms_VMInt;
typedef DSBuffer ms_VMStr;
typedef bool ms_VMBool;
typedef const void ms_VMNull;

typedef struct {
    DSArray *args;
    ms_VMByteCode *code;
} ms_VMFunc;

typedef enum {
    VMVAL_FLOAT,
    VMVAL_INT,
    VMVAL_STR,
    VMVAL_BOOL,
    VMVAL_NULL,
    VMVAL_FUNC,
} ms_VMDataType;

typedef union {
    ms_VMFloat f;
    ms_VMInt i;
    ms_VMStr *s;
    ms_VMBool b;
    ms_VMNull *n;
    ms_VMFunc *fn;
} ms_VMData;

typedef struct {
    ms_VMDataType type;
    ms_VMData val;
} ms_VMValue;

struct ms_VMByteCode {
    ms_VMOpCode *code;                              /* array of opcodes */
    ms_VMValue *values;                             /* array of VM values */
    DSBuffer **idents;                              /* array of identifiers */
    size_t nops;                                    /* number of opcodes */
    size_t nvals;                                   /* number of values */
    size_t nidents;                                 /* number of idents */
};

/**
* @brief Generate mscript VM bytecode from the given abstract syntax tree.
*/
ms_VMByteCode *ms_ASTToOpCodes(ms_AST *ast);

/**
* @brief Print a representation of the bytecode format to the stdout.
*/
void ms_VMByteCodePrint(const ms_VMByteCode *bc);

/**
* @brief Destroy the memory held by byte code.
*
* @param bc a @c ms_VMByteCode object
*/
void ms_VMByteCodeDestroy(ms_VMByteCode *bc);

/**
* @brief Destroy an ms_VMValue.
*/
void ms_VMValueDestroy(ms_VMValue *v);

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
* @brief Get the string constant name for a given VM opcode.
*/
const char *ms_VMOpCodeToString(ms_VMOpCode c);

#endif //MSCRIPT_BYTECODE_H
