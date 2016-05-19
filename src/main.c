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
#include "mscript.h"

static int StartREPL(const char *prog) {
    ms_StateOptions opts = {
        .interactive_mode = true,
        .print_bytecode = true,
    };
    ms_State *ms = ms_StateNewOptions(&opts);
    assert(ms);

    char *input;
    printf("mscript v0.1\n");
    while ((input = readline("> ")) != NULL) {
        if (strlen(input) == 0) {
            free(input);
            break;
        }

        const ms_Error *err;
        if (ms_StateExecuteString(ms, input, &err) == MS_RESULT_ERROR) {
            printf("%s: \n%s\n", prog, err->msg);
        }

        free(input);
    }

    ms_StateDestroy(ms);
    return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
    return StartREPL(argv[0]);
}
