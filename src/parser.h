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

#ifndef MSCRIPT_PARSER_H
#define MSCRIPT_PARSER_H

#include <stdbool.h>
#include "lang.h"

/**
* @brief Parser object
*/
typedef struct ms_Parser ms_Parser;

/**
*
*/
typedef enum ms_ParseResult {
    PARSE_SUCCESS,
    PARSE_WARNINGS,
    PARSE_ERROR
} ms_ParseResult;

/**
* @brief Error type returned from @c ms_Parser objects when
* there is an error parsing the code.
*/
typedef struct ms_ParseError {
    char *msg;
    ms_Token *tok;
} ms_ParseError;

/**
* @brief Create a new @c ms_Parser object.
*/
ms_Parser *ms_ParserNew(void);

/**
* @brief Initialize a @c ms_Parser from a file at the given path.
*/
bool ms_ParserInitFile(ms_Parser *prs, const char *fname);

/**
* @brief Initialize a @c ms_Parser from a string.
*/
bool ms_ParserInitString(ms_Parser *prs, const char *str);

/**
* @brief Initialize a @c ms_Parser from a string with known length.
*/
bool ms_ParserInitStringL(ms_Parser *prs, const char *str, size_t len);

/**
* @brief Destroy a @c ms_Parser object.
*/
void ms_ParserDestroy(ms_Parser *prs);

/**
* @brief Parse the mscript string or file associated with this @c ms_Parser .
*/
ms_ParseResult ms_ParserParse(ms_Parser *prs, ms_ParseError **err);

/**
* @brief Return a reference to the parsed AST node.
*/
ms_AST *ms_ParserGetAST(ms_Parser *prs);

/**
* @brief Destroy a @c ms_ParseError object.
*/
void ms_ParseErrorDestroy(ms_ParseError *err);

#endif //MSCRIPT_PARSER_H
