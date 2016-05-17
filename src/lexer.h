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

extern const char *const TOK_ERROR;
extern const char *const TOK_RESERVED_KW;
extern const char *const TOK_IDENTIFIER;
extern const char *const TOK_BUILTIN_FUNC;
extern const char *const TOK_GLOBAL;
extern const char *const TOK_NEWLINE;
extern const char *const TOK_QUESTION_MARK;
extern const char *const TOK_SEMICOLON;
extern const char *const TOK_COMMA;
extern const char *const TOK_PERIOD;
extern const char *const TOK_RBRACE;
extern const char *const TOK_LBRACE;
extern const char *const TOK_RBRACKET;
extern const char *const TOK_LBRACKET;
extern const char *const TOK_RPAREN;
extern const char *const TOK_LPAREN;
extern const char *const TOK_COLON;
extern const char *const TOK_OP_SAFE_GETATTR;
extern const char *const TOK_OP_SAFE_REFERENCE;
extern const char *const TOK_OP_BITWISE_AND_EQUALS;
extern const char *const TOK_OP_BITWISE_OR_EQUALS;
extern const char *const TOK_OP_BITWISE_XOR_EQUALS;
extern const char *const TOK_OP_SHIFT_LEFT_EQUALS;
extern const char *const TOK_OP_SHIFT_RIGHT_EQUALS;
extern const char *const TOK_OP_BITWISE_AND;
extern const char *const TOK_OP_BITWISE_OR;
extern const char *const TOK_OP_BITWISE_XOR;
extern const char *const TOK_OP_BITWISE_NOT;
extern const char *const TOK_OP_SHIFT_LEFT;
extern const char *const TOK_OP_SHIFT_RIGHT;
extern const char *const TOK_OP_LE;
extern const char *const TOK_OP_GE;
extern const char *const TOK_NOT_EQ;
extern const char *const TOK_OP_NOT;
extern const char *const TOK_OP_EQ;
extern const char *const TOK_OP_LT;
extern const char *const TOK_OP_GT;
extern const char *const TOK_OP_DOUBLE_EQ;
extern const char *const TOK_OP_EXPONENTIATE;
extern const char *const TOK_OP_OR;
extern const char *const TOK_OP_AND;
extern const char *const TOK_OP_MODULO_EQUALS;
extern const char *const TOK_OP_IDIVIDE_EQUALS;
extern const char *const TOK_OP_DIVIDE_EQUALS;
extern const char *const TOK_OP_TIMES_EQUALS;
extern const char *const TOK_OP_MINUS_EQUALS;
extern const char *const TOK_OP_PLUS_EQUALS;
extern const char *const TOK_OP_MODULO;
extern const char *const TOK_OP_IDIVIDE;
extern const char *const TOK_OP_DIVIDE;
extern const char *const TOK_OP_TIMES;
extern const char *const TOK_OP_MINUS;
extern const char *const TOK_OP_PLUS;
extern const char *const TOK_OP_UMINUS;
extern const char *const TOK_KW_MERGE;
extern const char *const TOK_KW_NULL;
extern const char *const TOK_KW_FALSE;
extern const char *const TOK_KW_TRUE;
extern const char *const TOK_KW_DEL;
extern const char *const TOK_KW_VAR;
extern const char *const TOK_KW_BREAK;
extern const char *const TOK_KW_CONTINUE;
extern const char *const TOK_KW_IMPORT;
extern const char *const TOK_KW_FOR;
extern const char *const TOK_KW_RETURN;
extern const char *const TOK_KW_ELSE;
extern const char *const TOK_KW_IF;
extern const char *const TOK_KW_FUNC;
extern const char *const TOK_KW_IS;
extern const char *const TOK_KW_IN;
extern const char *const TOK_KW_SELECT;
extern const char *const TOK_STRING;
extern const char *const TOK_INT_NUMBER;
extern const char *const TOK_FLOAT_NUMBER;
extern const char *const TOK_HEX_NUMBER;

/**
 * @brief Enumeration of mscript token types.
 */
typedef enum ms_TokenType {
    ERROR,
    RESERVED_KW,
    IDENTIFIER,
    BUILTIN_FUNC,
    GLOBAL,
    FLOAT_NUMBER,
    INT_NUMBER,
    HEX_NUMBER,
    STRING,
    KW_FUNC,
    KW_IF,
    KW_ELSE,
    KW_RETURN,
    KW_FOR,
    KW_IMPORT,
    KW_CONTINUE,
    KW_BREAK,
    KW_VAR,
    KW_DEL,
    KW_TRUE,
    KW_FALSE,
    KW_NULL,
    KW_MERGE,
    KW_IS,
    KW_IN,
    KW_SELECT,
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
    OP_BITWISE_AND_EQUALS,
    OP_BITWISE_OR_EQUALS,
    OP_BITWISE_XOR_EQUALS,
    OP_SHIFT_LEFT_EQUALS,
    OP_SHIFT_RIGHT_EQUALS,
    OP_SAFE_REFERENCE,
    OP_SAFE_GETATTR,
    COLON,
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACE,
    RBRACE,
    PERIOD,
    COMMA,
    SEMICOLON,
    QUESTION_MARK,
    NEWLINE_TOK
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

#endif //MSCRIPT_LEXER_H
