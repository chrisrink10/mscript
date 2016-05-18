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

#include "error.h"

void ms_ErrorDestroy(ms_Error *err) {
    if (!err) { return; }

    switch (err->type) {
        case MS_ERROR_PARSER:
            ms_TokenDestroy(err->detail.parse.tok);
            err->detail.parse.tok = NULL;
            break;
        case MS_ERROR_CODEGEN:
            break;
        case MS_ERROR_VM:
            break;
    }

    free(err->msg);
    err->msg = NULL;
    free(err);
}
