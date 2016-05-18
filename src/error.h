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

#ifndef MSCRIPT_ERROR_H
#define MSCRIPT_ERROR_H

#include "lexer.h"

typedef enum {
    MS_RESULT_SUCCESS,              /** Operation succeeded without error */
    MS_RESULT_WARNINGS,             /** Operation succeeded with non-fatal errors */
    MS_RESULT_ERROR,               /** Operation could not be completed */
} ms_Result;

typedef enum {
    MS_ERROR_PARSER,                /** Error occurred during parsing */
    MS_ERROR_CODEGEN,               /** Error occurred during code generation */
    MS_ERROR_VM,                    /** Error occurred during runtime in the VM */
} ms_ErrorType;

typedef struct {
    ms_Token *tok;                  /** Token potentially associated with the error */
} ms_ParseError;

typedef union {
    ms_ParseError parse;
} ms_ErrorDetail;

typedef struct {
    ms_ErrorType type;
    ms_ErrorDetail detail;
    size_t len;                     /** Length of the error message */
    char *msg;                      /** Error message associated with the error */
} ms_Error;

void ms_ErrorDestroy(ms_Error *err);

#endif //MSCRIPT_ERROR_H
