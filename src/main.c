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
#include <stdio.h>
#include <string.h>
#include "../deps/linenoise/linenoise.h"
#include "mscript.h"

static int MS_LINENOISE_HISTORY_LEN = 50;

typedef struct {
    bool show_help;
    bool show_version;
    bool print_bytecode;
    bool execute_string;
    char *code;
    bool execute_script;
    char *script;
    size_t nargs;
    char **script_args;
} CommandLineArgs;

static void PrintHelp(const char *prog) {
    printf("usage: %s -h -v -a -s [code] [script [args]]\n", prog);
    puts("Options:");
    puts("  -h        show this help text and exit");
    puts("  -v        show the version and exit");
    puts("  -a        print bytecode for all inputs");
    puts("  -s [code] execute string `code`");
}

static void PrintVersion(void) {
    puts("mscript v1.0");
    puts("Copyright (c) 2016 Chris Rink");
}

static int ParseCommandLineOpts(CommandLineArgs *opts, int argc, char *argv[]) {
    assert(opts);

    for (int i = 1; i < argc; ) {
        char *arg = argv[i];

        if (arg[0] == '-') {
            switch (arg[1]) {
                case 'h':
                    opts->show_help = true;
                    i += 1;
                    break;
                case 'v':
                    opts->show_version = true;
                    i += 1;
                    break;
                case 'a':
                    opts->print_bytecode = true;
                    i += 1;
                    break;
                case 's':
                    opts->execute_string = true;
                    i += 1;
                    if (i < argc) {
                        opts->code = argv[i];
                        i += 1;
                    } else {
                        printf("%s: expected argument `code`", argv[0]);
                        PrintHelp(argv[0]);
                        return EXIT_FAILURE;
                    }
                    break;
                default:
                    printf("%s: unrecognized option '-%c'\n", argv[0], arg[1]);
                    PrintHelp(argv[0]);
                    return EXIT_FAILURE;
            }
        } else {
            opts->execute_script = true;
            opts->script = argv[i];
            i += 1;
            opts->script_args = &argv[i];
            opts->nargs = (size_t)(argc - i);
            break;
        }
    }

    return EXIT_SUCCESS;
}

static int ExecuteScript(const char *prog, CommandLineArgs *args) {
    ms_StateOptions opts = {
        .interactive_mode = false,
        .print_bytecode = args->print_bytecode,
    };
    ms_State *ms = ms_StateNewOptions(&opts);
    if (!ms) {
        printf("%s: could not create the state object\n", prog);
        return EXIT_FAILURE;
    }

    const ms_Error *err;
    if (ms_StateExecuteFile(ms, args->script, &err) == MS_RESULT_ERROR) {
        printf("%s: \n%s\n", prog, err->msg);
    }

    ms_StateDestroy(ms);
    return EXIT_SUCCESS;
}

static int ExecuteString(const char *prog, CommandLineArgs *args) {
    ms_StateOptions opts = {
        .interactive_mode = false,
        .print_bytecode = args->print_bytecode,
    };
    ms_State *ms = ms_StateNewOptions(&opts);
    if (!ms) {
        printf("%s: could not create the state object\n", prog);
        return EXIT_FAILURE;
    }

    const ms_Error *err;
    if (ms_StateExecuteString(ms, args->code, &err) == MS_RESULT_ERROR) {
        printf("%s: \n%s\n", prog, err->msg);
    }

    ms_StateDestroy(ms);
    return EXIT_SUCCESS;
}

static int StartREPL(const char *prog, CommandLineArgs *args) {
    ms_StateOptions opts = {
        .interactive_mode = true,
        .print_bytecode = args->print_bytecode,
    };
    ms_State *ms = ms_StateNewOptions(&opts);
    if (!ms) {
        printf("%s: could not create the state object\n", prog);
        return EXIT_FAILURE;
    }

    linenoiseSetMultiLine(1);
    linenoiseHistorySetMaxLen(MS_LINENOISE_HISTORY_LEN);

    char *input;
    puts("mscript v0.1");
    while ((input = linenoise("> ")) != NULL) {
        if (strlen(input) == 0) {
            linenoiseFree(input);
            continue;
        }
        linenoiseHistoryAdd(input);

        const ms_Error *err;
        if (ms_StateExecuteString(ms, input, &err) == MS_RESULT_ERROR) {
            printf("%s: \n%s\n", prog, err->msg);
        }

        linenoiseFree(input);
    }

    ms_StateDestroy(ms);
    return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
    CommandLineArgs args = { 0 };
    if (ParseCommandLineOpts(&args, argc, argv) == EXIT_FAILURE) {
        return EXIT_FAILURE;
    }

    if (args.show_help) {
        PrintHelp(argv[0]);
        return EXIT_SUCCESS;
    }

    if (args.show_version) {
        PrintVersion();
        return EXIT_SUCCESS;
    }

    if (args.execute_string) {
        return ExecuteString(argv[0], &args);
    }

    if (args.execute_script) {
        return ExecuteScript(argv[0], &args);
    }

    return StartREPL(argv[0], &args);
}
