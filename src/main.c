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
#include <stdlib.h>
#include <string.h>
#include <readline/readline.h>
#include "parser.h"
#include "vm.h"

static int StartREPL(const char *prog) {
    ms_Parser *prs = ms_ParserNew();
    assert(prs);
    ms_VM *vm = ms_VMNew();
    assert(vm);

    char *input;
    printf("mscript v0.1\n");
    while ((input = readline("> ")) != NULL) {
        if (strlen(input) == 0) {
            ms_VMClear(vm);
            free(input);
            break;
        }

        ms_ParserInitString(prs, input);

        ms_ParseError *err;
        ms_VMByteCode *code;    /* freed by the VM */
        if (ms_ParserParse(prs, &code, &err) == PARSE_ERROR) {
            printf("%s: \nLine %zu, Col %zu :: %s\n",
                   prog, err->tok->line, err->tok->col, err->msg);
            free(input);
            continue;
        }

        assert(code);
        assert(!err);

        const ms_VMError *vmerr;
        if (ms_VMExecuteAndPrint(vm, code, &vmerr) != VMEXEC_SUCCESS) {
            printf("%s: \n%s\n", prog, vmerr->msg);
        }

        ms_VMClear(vm);
        free(input);
    }

    ms_ParserDestroy(prs);
    ms_VMDestroy(vm);
    return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
    return StartREPL(argv[0]);
}
