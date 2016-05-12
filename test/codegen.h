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

#ifndef MSCRIPT_CODEGEN_H
#define MSCRIPT_CODEGEN_H

#include "munit/munit.h"

/*
 * FUNCTION DECLARATIONS
 */

MunitResult prs_TestCodeGenLiterals(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenUnaryExprs(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenBinaryExprs(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenExprPrecedence(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenFunctionCalls(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenDeleteStatement(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenForIncStatements(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenForIterStatements(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenForExprStatements(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenIfStatements(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenImportStatement(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenMergeStatement(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenReturnStatement(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenFuncDeclaration(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenDeclaration(const MunitParameter params[], void *user_data);
MunitResult prs_TestCodeGenAssignment(const MunitParameter params[], void *user_data);

/*
 * TEST DEFINITIONS
 */

extern MunitTest codegen_tests[];

#endif //MSCRIPT_CODEGEN_H
