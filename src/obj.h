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

#ifndef MSCRIPT_OBJ_H
#define MSCRIPT_OBJ_H

#include "libds/dict.h"
#include "vm.h"

/**
* @
*/
typedef int (*ms_Function)(ms_VM *vm);

/**
* @brief Prototype of all objects
*/
typedef struct {
    const char *name;
    ms_Function func;
} ms_FuncDef;

extern const ms_FuncDef MS_OBJECT_PROTOTYPE[];
extern const ms_FuncDef MS_FLOAT_PROTOTYPE[];
extern const ms_FuncDef MS_INT_PROTOTYPE[];
extern const ms_FuncDef MS_STR_PROTOTYPE[];
extern const ms_FuncDef MS_BOOL_PROTOTYPE[];
extern const ms_FuncDef MS_NULL_PROTOTYPE[];

#endif //MSCRIPT_OBJ_H
