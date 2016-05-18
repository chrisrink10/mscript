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

#ifndef MSCRIPT_MSCRIPT_H
#define MSCRIPT_MSCRIPT_H

#include "error.h"

typedef struct ms_State ms_State;

typedef struct {
    bool print_bytecode;
} ms_StateOptions;

ms_State *ms_StateNew(void);
ms_State *ms_StateNewOptions(ms_StateOptions opts);
ms_Result ms_StateExecuteString(ms_State *state, const char *str, const ms_Error **err);
ms_Result ms_StateExecuteStringL(ms_State *state, const char *str, size_t len, const ms_Error **err);
ms_Result ms_StateExecuteFile(ms_State *state, const char *fname, const ms_Error **err);
void ms_StateErrorClear(ms_State *state);
void ms_StateDestroy(ms_State *state);

#endif //MSCRIPT_MSCRIPT_H
