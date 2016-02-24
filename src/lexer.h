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

#ifndef MSCRIPT_LEXER_H
#define MSCRIPT_LEXER_H

#include <stdbool.h>
#include <stdlib.h>
#include "libds/buffer.h"

static const char *const TOK_ERROR = "ERROR";
static const char *const TOK_RESERVED_KW = "RESERVED_KW";
static const char *const TOK_IDENTIFIER = "IDENTIFIER";
static const char *const TOK_BUILTIN_FUNC = "BUILTIN_FUNC";
static const char *const TOK_GLOBAL = "GLOBAL";
static const char *const TOK_NEWLINE = "NEWLINE";
static const char *const TOK_COMMA = "COMMA";
static const char *const TOK_PERIOD = "PERIOD";
static const char *const TOK_RBRACE = "RBRACE";
static const char *const TOK_LBRACE = "LBRACE";
static const char *const TOK_RBRACKET = "RBRACKET";
static const char *const TOK_LBRACKET = "LBRACKET";
static const char *const TOK_RPAREN = "RPAREN";
static const char *const TOK_LPAREN = "LPAREN";
static const char *const TOK_COLON = "COLON";
static const char *const TOK_OP_BITWISE_AND = "OP_BITWISE_AND";
static const char *const TOK_OP_BITWISE_OR = "OP_BITWISE_OR";
static const char *const TOK_OP_BITWISE_XOR = "OP_BITWISE_XOR";
static const char *const TOK_OP_BITWISE_NOT = "OP_BITWISE_NOT";
static const char *const TOK_OP_SHIFT_LEFT = "OP_SHIFT_LEFT";
static const char *const TOK_OP_SHIFT_RIGHT = "OP_SHIFT_RIGHT";
static const char *const TOK_OP_LE = "OP_LE";
static const char *const TOK_OP_GE = "OP_GE";
static const char *const TOK_NOT_EQ = "OP_NOT_EQ";
static const char *const TOK_OP_NOT = "OP_NOT";
static const char *const TOK_OP_EQ = "OP_EQ";
static const char *const TOK_OP_LT = "OP_LT";
static const char *const TOK_OP_GT = "OP_GT";
static const char *const TOK_OP_DOUBLE_EQ = "OP_DOUBLE_EQ";
static const char *const TOK_OP_EXPONENTIATE = "OP_EXPONENTIATE";
static const char *const TOK_OP_DECREMENT = "OP_DECREMENT";
static const char *const TOK_OP_INCREMENT = "OP_INCREMENT";
static const char *const TOK_OP_OR = "OP_OR";
static const char *const TOK_OP_AND = "OP_AND";
static const char *const TOK_OP_MODULO_EQUALS = "OP_MODULO_EQUALS";
static const char *const TOK_OP_IDIVIDE_EQUALS = "OP_IDIVIDE_EQUALS";
static const char *const TOK_OP_DIVIDE_EQUALS = "OP_DIVIDE_EQUALS";
static const char *const TOK_OP_TIMES_EQUALS = "OP_TIMES_EQUALS";
static const char *const TOK_OP_MINUS_EQUALS = "OP_MINUS_EQUALS";
static const char *const TOK_OP_PLUS_EQUALS = "OP_PLUS_EQUALS";
static const char *const TOK_OP_MODULO = "OP_MODULO";
static const char *const TOK_OP_IDIVIDE = "OP_IDIVIDE";
static const char *const TOK_OP_DIVIDE = "OP_DIVIDE";
static const char *const TOK_OP_TIMES = "OP_TIMES";
static const char *const TOK_OP_MINUS = "OP_MINUS";
static const char *const TOK_OP_PLUS = "OP_PLUS";
static const char *const TOK_OP_UMINUS = "OP_UMINUS";
static const char *const TOK_KW_MERGE = "KW_MERGE";
static const char *const TOK_KW_NULL = "KW_NULL";
static const char *const TOK_KW_FALSE = "KW_FALSE";
static const char *const TOK_KW_TRUE = "KW_TRUE";
static const char *const TOK_KW_DEL = "KW_DEL";
static const char *const TOK_KW_VAR = "KW_VAR";
static const char *const TOK_KW_BREAK = "KW_BREAK";
static const char *const TOK_KW_CONTINUE = "KW_CONTINUE";
static const char *const TOK_KW_PACKAGE = "KW_PACKAGE";
static const char *const TOK_KW_IMPORT = "KW_IMPORT";
static const char *const TOK_KW_FOR = "KW_FOR";
static const char *const TOK_KW_RETURN = "KW_RETURN";
static const char *const TOK_KW_ELSE = "KW_ELSE";
static const char *const TOK_KW_IF = "KW_IF";
static const char *const TOK_KW_FUNC = "KW_FUNC";
static const char *const TOK_KW_NUM = "KW_NUM";
static const char *const TOK_KW_STR = "KW_STR";
static const char *const TOK_KW_BOOL = "KW_BOOL";
static const char *const TOK_KW_DATETIME = "KW_DATETIME";
static const char *const TOK_KW_OBJ = "KW_OBJ";
static const char *const TOK_KW_IS = "KW_IS";
static const char *const TOK_KW_AS = "KW_AS";
static const char *const TOK_KW_IN = "KW_IN";
static const char *const TOK_STRING = "STRING";
static const char *const TOK_NUMBER = "NUMBER";
static const char *const TOK_HEX_NUMBER = "HEX_NUMBER";

/**
 * @brief Enumeration of mscript token types.
 */
typedef enum ms_TokenType {
    ERROR,
    RESERVED_KW,
    IDENTIFIER,
    BUILTIN_FUNC,
    GLOBAL,
    NUMBER,
    HEX_NUMBER,
    STRING,
    KW_FUNC,
    KW_IF,
    KW_ELSE,
    KW_RETURN,
    KW_FOR,
    KW_IMPORT,
    KW_PACKAGE,
    KW_CONTINUE,
    KW_BREAK,
    KW_VAR,
    KW_DEL,
    KW_TRUE,
    KW_FALSE,
    KW_NULL,
    KW_MERGE,
    KW_NUM,
    KW_STR,
    KW_BOOL,
    KW_DATETIME,
    KW_OBJ,
    KW_IS,
    KW_AS,
    KW_IN,
    OP_UMINUS,
    OP_PLUS,
    OP_MINUS,
    OP_TIMES,
    OP_DIVIDE,
    OP_IDIVIDE,
    OP_MODULO,
    OP_PLUS_EQUALS,
    OP_MINUS_EQUALS,
    OP_TIMES_EQUALS,
    OP_DIVIDE_EQUALS,
    OP_IDIVIDE_EQUALS,
    OP_MODULO_EQUALS,
    OP_AND,
    OP_OR,
    OP_INCREMENT,
    OP_DECREMENT,
    OP_EXPONENTIATE,
    OP_DOUBLE_EQ,
    OP_GT,
    OP_LT,
    OP_EQ,
    OP_NOT,
    OP_NOT_EQ,
    OP_GE,
    OP_LE,
    OP_BITWISE_AND,
    OP_BITWISE_OR,
    OP_BITWISE_XOR,
    OP_BITWISE_NOT,
    OP_SHIFT_LEFT,
    OP_SHIFT_RIGHT,
    COLON,
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACE,
    RBRACE,
    PERIOD,
    COMMA,
    NEWLINE
} ms_TokenType;

/**
 * @brief Lexer token value
 */
typedef struct ms_Token {
    ms_TokenType type;
    DSBuffer *value;
    size_t line;
    size_t col;
} ms_Token;

/**
* @brief Lexer object
*/
typedef struct ms_Lexer ms_Lexer;

/**
* @brief Create a new lexer object.
*
* @returns a heap-allocated @c ms_Lexer object
*/
ms_Lexer *ms_LexerNew(void);

/**
* @brief Initialize a lexer object with a file.
*
* This is safe to call on an existing lexer. The old stream will
* be disposed of prior to opening the new stream.
*
* @param lex the lexer object
* @param fname the name of the file to open
* @returns true if the lexer was initialized with the given file;
*          false otherwise
*/
bool ms_LexerInitFile(ms_Lexer *lex, const char *fname);

/**
* @brief Initialize a lexer object with a string.
*
* This is safe to call on an existing lexer. The old stream will
* be disposed of prior to opening the new stream.
*
* @param lex the lexer object
* @param str the string to lex
* @returns true if the lexer was initialized with the given file;
*          false otherwise
*/
bool ms_LexerInitString(ms_Lexer *lex, const char *str);

/**
* @brief Initialize a lexer object with a string of the given length.
*
* This is safe to call on an existing lexer. The old stream will
* be disposed of prior to opening the new stream.
*
* @param lex the lexer object
* @param str the string to lex
* @param len the length of the input string
* @returns true if the lexer was initialized with the given file;
*          false otherwise
*/
bool ms_LexerInitStringL(ms_Lexer *lex, const char *str, size_t len);

/**
* @brief Dispose of the lexer object and free memory it is holding.
*
* If this object was initialized with a file, the file handle will be closed
* when this function is called.
*
* @param lex the lexer object
*/
void ms_LexerDestroy(ms_Lexer *lex);

/**
* @brief Return the next available token in the stream.
*
* @param lex the lexer object
* @returns the next token in the input stream or NULL once they've reached
*          the EOF
*/
ms_Token *ms_LexerNextToken(ms_Lexer *lex);

/**
* @brief Create a new lexer token of the given type with the value and
* positional information provided.
*
* @param type the type of token
* @param value the string value of the token
* @param len the length of the string value of the token
* @param line the line the token was found on
* @param col the starting column position for the token
* @returns a new @c ms_Token value unless an error occurs; NULL if
*          there was an error in creating the structure
*/
ms_Token *ms_TokenNew(ms_TokenType type, const char *value, size_t len, size_t line, size_t col);

/**
* @brief Return a string representation of a Gryphon token.
*
* You must call @c free on the return value from this function.
*
* @param tok a token object
* @returns a string representation of the input token
*/
char *ms_TokenToString(ms_Token *tok);

/**
* @brief Indicate if the given token qualifies as an operator.
*
* @param tok a token object
* @returns true if the given token is an operator; false otherwise
*/
bool ms_TokenIsOp(ms_Token *tok);

/**
* @brief Destroy a token.
*
* @param tok a token object
*/
void ms_TokenDestroy(ms_Token *tok);

/**
* @brief Return the constant name corresponding to the given token.
*
* @param tok a token object
* @returns the string name of the given token type
*/
const char *ms_TokenName(ms_Token *tok);

/**
* @brief Return the constant name corresponding to the given token type.
*
* @param type a type of token
* @returns the string name of the given token type
*/
const char *ms_TokenTypeName(ms_TokenType type);

/**
* @brief Indicate if the given type of token qualifies as an operator.
*
* @param type a type of token
* @returns true if the given token type is an operator; false otherwise
*/
bool ms_TokenTypeIsOp(ms_TokenType type);

#endif //MSCRIPT_LEXER_H
