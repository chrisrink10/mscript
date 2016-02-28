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

#include <stdio.h>
#include "lexer.h"
#include "../src/lexer.h"

typedef struct LexResultTuple {
    const char *val;        /** Value of the input token */
    ms_TokenType type;      /** Type of token value should be lexed into */
} LexResultTuple;

/*
 * FORWARD DECLARATIONS
 */

MunitResult LexExpect(const char *param, ms_TokenType type);
MunitResult TestLexResultTuple(LexResultTuple *tokens, size_t len);

/*
 * UNIT TEST FUNCTIONS
 */

MunitResult lex_TestLexIntNumerics(const MunitParameter params[], void *user_data) {
    const char *num = munit_parameters_get(params, "num");
    return LexExpect(num, INT_NUMBER);
}

MunitResult lex_TestLexFloatNumerics(const MunitParameter params[], void *user_data) {
    const char *num = munit_parameters_get(params, "num");
    return LexExpect(num, FLOAT_NUMBER);
}

MunitResult lex_TestLexHexNumerics(const MunitParameter params[], void *user_data) {
    const char *num = munit_parameters_get(params, "num");
    return LexExpect(num, HEX_NUMBER);
}

MunitResult lex_TestLexBadNumerics(const MunitParameter params[], void *user_data) {
    const char *num = munit_parameters_get(params, "num");
    return LexExpect(num, ERROR);
}

MunitResult lex_TestLexKeywords(const MunitParameter params[], void *user_data) {
    static LexResultTuple tokens[] = {
        { "if", KW_IF },
        { "else", KW_ELSE },
        { "return", KW_RETURN },
        { "true", KW_TRUE },
        { "false", KW_FALSE },
        { "null", KW_NULL },
        { "func", KW_FUNC },
        { "del", KW_DEL },
        { "continue", KW_CONTINUE },
        { "break", KW_BREAK },
        { "import", KW_IMPORT },
        { "package", KW_PACKAGE },
        { "merge", KW_MERGE },
        { "var", KW_VAR },
        { "num", KW_NUM },
        { "str", KW_STR },
        { "bool", KW_BOOL },
        { "datetime", KW_DATETIME },
        { "obj", KW_OBJ },
        { "as", KW_AS },
        { "in", KW_IN },
        { "is", KW_IS },
        { "for", KW_FOR },
    };

    size_t len = sizeof(tokens) / sizeof(tokens[0]);
    return TestLexResultTuple(tokens, len);
}

MunitResult lex_TestLexNonKeywords(const MunitParameter params[], void *user_data) {
    const char *kw = munit_parameters_get(params, "kw");
    return LexExpect(kw, IDENTIFIER);
}

MunitResult lex_TestLexReservedKeywords(const MunitParameter params[], void *user_data) {
    const char *kw = munit_parameters_get(params, "kw");
    return LexExpect(kw, RESERVED_KW);
}

MunitResult lex_TestLexGlobals(const MunitParameter params[], void *user_data) {
    const char *global = munit_parameters_get(params, "global");
    return LexExpect(global, GLOBAL);
}

MunitResult lex_TestLexBuiltins(const MunitParameter params[], void *user_data) {
    const char *builtin = munit_parameters_get(params, "builtin");
    return LexExpect(builtin, BUILTIN_FUNC);
}

MunitResult lex_TestLexInvalidIdentifiers(const MunitParameter params[], void *user_data) {
    const char *ident = munit_parameters_get(params, "ident");
    return LexExpect(ident, ERROR);
}

MunitResult lex_TestLexOperators(const MunitParameter params[], void *user_data) {
    static LexResultTuple tokens[] = {
        { "+", OP_PLUS },
        { "+=", OP_PLUS_EQUALS },
        { "-", OP_MINUS },
        { "-=", OP_MINUS_EQUALS },
        { "*", OP_TIMES },
        { "*=", OP_TIMES_EQUALS },
        { "/", OP_DIVIDE },
        { "/=", OP_DIVIDE_EQUALS },
        { "\\", OP_IDIVIDE },
        { "\\=", OP_IDIVIDE_EQUALS },
        { "%", OP_MODULO },
        { "%=", OP_MODULO_EQUALS },
        { "**", OP_EXPONENTIATE },
        { "&&", OP_AND },
        { "||", OP_OR },
        { "!", OP_NOT },
        { "==", OP_DOUBLE_EQ },
        { "!=", OP_NOT_EQ },
        { ">", OP_GT },
        { ">=", OP_GE },
        { "<", OP_LT },
        { "<=", OP_LE },
        { "++", OP_INCREMENT },
        { "--", OP_DECREMENT },
        { "=", OP_EQ },
        { "&", OP_BITWISE_AND },
        { "|", OP_BITWISE_OR },
        { "~", OP_BITWISE_NOT },
        { "@", OP_BITWISE_XOR },
        { ">>", OP_SHIFT_RIGHT },
        { ">>", OP_SHIFT_LEFT },
    };

    size_t len = sizeof(tokens) / sizeof(tokens[0]);
    return TestLexResultTuple(tokens, len);
}

MunitResult lex_TestLexPunctuation(const MunitParameter params[], void *user_data) {
    static LexResultTuple tokens[] = {
        { "[", LBRACKET },
        { "]", RBRACKET },
        { "(", LPAREN },
        { ")", RPAREN },
        { "{", LBRACE },
        { "}", RBRACE },
        { ":", COLON },
        { ".", PERIOD },
        { ",", COMMA }
    };

    size_t len = sizeof(tokens) / sizeof(tokens[0]);
    return TestLexResultTuple(tokens, len);
}

MunitResult lex_TestLexNewlines(const MunitParameter params[], void *user_data) {
    const char *newline = munit_parameters_get(params, "newline");
    return LexExpect(newline, NEWLINE);
}

MunitResult lex_TestLexStrings(const MunitParameter params[], void *user_data) {
    const char *str = munit_parameters_get(params, "str");
    return LexExpect(str, STRING);
}

MunitResult lex_TestLexInvalidStrings(const MunitParameter params[], void *user_data) {
    const char *str = munit_parameters_get(params, "str");
    return LexExpect(str, ERROR);
}

/*
 * PRIVATE FUNCTIONS
 */

MunitResult LexExpect(const char *param, ms_TokenType type) {
    ms_Lexer *lex = ms_LexerNew();
    munit_assert_non_null(lex);
    munit_assert(ms_LexerInitString(lex, param));

    ms_Token *tok = ms_LexerNextToken(lex);
    munit_assert_non_null(tok);
    munit_assert_cmp_int(tok->type, ==, type);
    if (type != ERROR) {
        munit_assert_string_equal(dsbuf_char_ptr(tok->value), param);
    }
    munit_assert_string_equal(ms_TokenName(tok), ms_TokenTypeName(type));

    ms_LexerDestroy(lex);
    ms_TokenDestroy(tok);
    return MUNIT_OK;
}

MunitResult TestLexResultTuple(LexResultTuple *tokens, size_t len) {
    ms_Lexer *lex = ms_LexerNew();
    munit_assert_non_null(lex);

    for (size_t i = 0; i < len; i++) {
        LexResultTuple *tuple = &tokens[0];
        (void)LexExpect(tuple->val, tuple->type);
    }
    return MUNIT_OK;
}
