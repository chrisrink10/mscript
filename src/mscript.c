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
#include <string.h>
#include "mscript.h"
#include "parser.h"
#include "vm.h"

/*
 * FORWARD DECLARATIONS
 */

struct ms_State {
    ms_Parser *prs;
    ms_VM *vm;
    ms_StateOptions opts;
    ms_Error *err;
};

static ms_Result StateParseAndExecute(ms_State *state, const ms_Error **err);

/*
 * PUBLIC FUNCTIONS
 */

ms_State *ms_StateNew(void) {
    ms_StateOptions opts = {
        .print_bytecode = false,
    };
    return ms_StateNewOptions(opts);
}

ms_State *ms_StateNewOptions(ms_StateOptions opts) {
    ms_State *state = malloc(sizeof(ms_State));
    if (!state) {
        return NULL;
    }

    state->prs = ms_ParserNew();
    if (!state->prs) {
        free(state);
        return NULL;
    }

    state->vm = ms_VMNew();
    if (!state->vm) {
        ms_ParserDestroy(state->prs);
        free(state);
        return NULL;
    }

    state->opts = opts;
    state->err = NULL;
    return state;
}

ms_Result ms_StateExecuteString(ms_State *state, const char *str, const ms_Error **err) {
    return ms_StateExecuteStringL(state, str, strlen(str), err);
}

ms_Result ms_StateExecuteStringL(ms_State *state, const char *str, size_t len, const ms_Error **err) {
    if (!state) {
        return MS_RESULT_ERROR;
    }

    if (!ms_ParserInitStringL(state->prs, str, len)) {
        return MS_RESULT_ERROR;
    }

    return StateParseAndExecute(state, err);
}

ms_Result ms_StateExecuteFile(ms_State *state, const char *fname, const ms_Error **err) {
    if (!state) {
        return MS_RESULT_ERROR;
    }

    if (!ms_ParserInitFile(state->prs, fname)) {
        return MS_RESULT_ERROR;
    }

    return StateParseAndExecute(state, err);
}

void ms_StateErrorClear(ms_State *state) {
    if (!state) { return; }
    ms_ErrorDestroy(state->err);
    state->err = NULL;
}

void ms_StateDestroy(ms_State *state) {
    if (!state) { return; }
    ms_ParserDestroy(state->prs);
    state->prs = NULL;
    ms_VMDestroy(state->vm);
    state->vm = NULL;
    ms_StateErrorClear(state);
    free(state);
}

/*
 * PRIVATE FUNCTIONS
 */

static ms_Result StateParseAndExecute(ms_State *state, const ms_Error **err) {
    assert(state);
    assert(err);

    ms_StateErrorClear(state);

    const ms_AST *ast;
    if (ms_ParserParse(state->prs, &ast, &state->err) == MS_RESULT_ERROR) {
        *err = state->err;
        return MS_RESULT_ERROR;
    }

    assert(!state->err);
    ms_VMByteCode *code;    /* freed by the VM */
    if (ms_VMByteCodeGenerateFromAST(ast, &code, &state->err) == MS_RESULT_ERROR) {
        *err = state->err;
        return MS_RESULT_ERROR;
    }

    assert(!state->err);
    if (state->opts.print_bytecode) {
        ms_VMByteCodePrint(code);
    }

    const ms_VMError *vmerr;
    if (ms_VMExecuteAndPrint(state->vm, code, &vmerr) != VMEXEC_SUCCESS) {
        return MS_RESULT_ERROR;
    }

    ms_VMClear(state->vm);
    return MS_RESULT_SUCCESS;
}
