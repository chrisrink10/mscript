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
#include <string.h>
#include "libds/dict.h"
#include "libds/hash.h"
#include "stream/streamreader.h"
#include "lexer.h"

/*
 * MACROS AND CONSTANTS
 */

// Token name constants
const char *const TOK_ERROR = "ERROR";
const char *const TOK_RESERVED_KW = "RESERVED_KW";
const char *const TOK_IDENTIFIER = "IDENTIFIER";
const char *const TOK_BUILTIN_FUNC = "BUILTIN_FUNC";
const char *const TOK_GLOBAL = "GLOBAL";
const char *const TOK_NEWLINE = "NEWLINE";
const char *const TOK_QUESTION_MARK = "QUESTION_MARK";
const char *const TOK_SEMICOLON = "SEMICOLON";
const char *const TOK_COMMA = "COMMA";
const char *const TOK_PERIOD = "PERIOD";
const char *const TOK_RBRACE = "RBRACE";
const char *const TOK_LBRACE = "LBRACE";
const char *const TOK_RBRACKET = "RBRACKET";
const char *const TOK_LBRACKET = "LBRACKET";
const char *const TOK_RPAREN = "RPAREN";
const char *const TOK_LPAREN = "LPAREN";
const char *const TOK_COLON = "COLON";
const char *const TOK_OP_SAFE_GETATTR = "OP_SAFE_GETATTR";
const char *const TOK_OP_SAFE_REFERENCE = "OP_SAFE_REFERENCE";
const char *const TOK_OP_BITWISE_AND_EQUALS = "OP_BITWISE_AND_EQUALS";
const char *const TOK_OP_BITWISE_OR_EQUALS = "OP_BITWISE_OR_EQUALS";
const char *const TOK_OP_BITWISE_XOR_EQUALS = "OP_BITWISE_XOR_EQUALS";
const char *const TOK_OP_SHIFT_LEFT_EQUALS = "OP_SHIFT_LEFT_EQUALS";
const char *const TOK_OP_SHIFT_RIGHT_EQUALS = "OP_SHIFT_RIGHT_EQUALS";
const char *const TOK_OP_BITWISE_AND = "OP_BITWISE_AND";
const char *const TOK_OP_BITWISE_OR = "OP_BITWISE_OR";
const char *const TOK_OP_BITWISE_XOR = "OP_BITWISE_XOR";
const char *const TOK_OP_BITWISE_NOT = "OP_BITWISE_NOT";
const char *const TOK_OP_SHIFT_LEFT = "OP_SHIFT_LEFT";
const char *const TOK_OP_SHIFT_RIGHT = "OP_SHIFT_RIGHT";
const char *const TOK_OP_LE = "OP_LE";
const char *const TOK_OP_GE = "OP_GE";
const char *const TOK_NOT_EQ = "OP_NOT_EQ";
const char *const TOK_OP_NOT = "OP_NOT";
const char *const TOK_OP_EQ = "OP_EQ";
const char *const TOK_OP_LT = "OP_LT";
const char *const TOK_OP_GT = "OP_GT";
const char *const TOK_OP_DOUBLE_EQ = "OP_DOUBLE_EQ";
const char *const TOK_OP_EXPONENTIATE = "OP_EXPONENTIATE";
const char *const TOK_OP_OR = "OP_OR";
const char *const TOK_OP_AND = "OP_AND";
const char *const TOK_OP_MODULO_EQUALS = "OP_MODULO_EQUALS";
const char *const TOK_OP_IDIVIDE_EQUALS = "OP_IDIVIDE_EQUALS";
const char *const TOK_OP_DIVIDE_EQUALS = "OP_DIVIDE_EQUALS";
const char *const TOK_OP_TIMES_EQUALS = "OP_TIMES_EQUALS";
const char *const TOK_OP_MINUS_EQUALS = "OP_MINUS_EQUALS";
const char *const TOK_OP_PLUS_EQUALS = "OP_PLUS_EQUALS";
const char *const TOK_OP_MODULO = "OP_MODULO";
const char *const TOK_OP_IDIVIDE = "OP_IDIVIDE";
const char *const TOK_OP_DIVIDE = "OP_DIVIDE";
const char *const TOK_OP_TIMES = "OP_TIMES";
const char *const TOK_OP_MINUS = "OP_MINUS";
const char *const TOK_OP_PLUS = "OP_PLUS";
const char *const TOK_OP_UMINUS = "OP_UMINUS";
const char *const TOK_KW_NULL = "KW_NULL";
const char *const TOK_KW_FALSE = "KW_FALSE";
const char *const TOK_KW_TRUE = "KW_TRUE";
const char *const TOK_KW_DEL = "KW_DEL";
const char *const TOK_KW_VAR = "KW_VAR";
const char *const TOK_KW_BREAK = "KW_BREAK";
const char *const TOK_KW_CONTINUE = "KW_CONTINUE";
const char *const TOK_KW_IMPORT = "KW_IMPORT";
const char *const TOK_KW_FOR = "KW_FOR";
const char *const TOK_KW_RETURN = "KW_RETURN";
const char *const TOK_KW_ELSE = "KW_ELSE";
const char *const TOK_KW_IF = "KW_IF";
const char *const TOK_KW_FUNC = "KW_FUNC";
const char *const TOK_KW_IS = "KW_IS";
const char *const TOK_KW_IN = "KW_IN";
const char *const TOK_KW_SELECT = "KW_SELECT";
const char *const TOK_STRING = "STRING";
const char *const TOK_INT_NUMBER = "INT_NUMBER";
const char *const TOK_FLOAT_NUMBER = "FLOAT_NUMBER";
const char *const TOK_HEX_NUMBER = "HEX_NUMBER";

// Default token buffer size
static const int DEFAULT_TOKEN_BUFFER = 10;

// Important private constants
static const char *const BASE_10_DIGITS = "0123456789";
static const char *const BASE_16_DIGITS = "0123456789abcdefABCDEF";
static const char *const ALPHANUMERIC = "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
static const char *const SYMBOLS = "/*+-=)(&^%$#@![]{}\\|'\":;,.<>?`~ \f\t\n\r;?`#";    // Includes invalid symbols:  ;?`#

// mscript language keywords
typedef struct KeywordTuple {
    const char *kw;
    ms_TokenType type;
} KeywordTuple;
static KeywordTuple KEYWORDS[] = {
    // Current/valid keywords
    { "if", KW_IF }, { "else", KW_ELSE }, { "return", KW_RETURN },
    { "true", KW_TRUE }, { "false", KW_FALSE }, { "null", KW_NULL },
    { "func", KW_FUNC }, { "del", KW_DEL }, { "continue", KW_CONTINUE },
    { "break", KW_BREAK }, { "import", KW_IMPORT }, { "var", KW_VAR },
    { "in", KW_IN }, { "is", KW_IS }, { "for", KW_FOR }, { "select", KW_SELECT },

    // Unused and reserved keywords
    { "switch", RESERVED_KW }, { "error", RESERVED_KW }, { "goto", RESERVED_KW },
    { "class", RESERVED_KW }, { "private", RESERVED_KW },
    { "public", RESERVED_KW }, { "protected", RESERVED_KW },
    { "yield", RESERVED_KW }, { "from", RESERVED_KW }, { "try", RESERVED_KW },
    { "finally", RESERVED_KW }, { "do", RESERVED_KW }, { "and", RESERVED_KW },
    { "or", RESERVED_KW }, { "mut", RESERVED_KW }, { "const", RESERVED_KW },
    { "async", RESERVED_KW }, { "await", RESERVED_KW },
    { "repeat", RESERVED_KW }, { "until", RESERVED_KW },
    { "while", RESERVED_KW }, { "except", RESERVED_KW }, { "num", RESERVED_KW },
    { "str", RESERVED_KW }, { "bool", RESERVED_KW }, { "datetime", RESERVED_KW },
    { "obj", RESERVED_KW }, { "as", RESERVED_KW }, { "package", RESERVED_KW },
    { "with", RESERVED_KW }, { "using", RESERVED_KW }, { "spawn", RESERVED_KW },
    { "val", RESERVED_KW }, { "merge", RESERVED_KW }
};

/*
 * FORWARD DECLARATIONS
 */

// Lexer object
struct ms_Lexer {
    size_t line;                /** Current line in the input stream */
    size_t col;                 /** Current column of the current line */
    ms_StreamReader *reader;    /** Input stream (either string or file) */
    DSBuffer *buffer;           /** Current token value buffer */
    DSDict *kwcache;            /** Cache of keyword tokens for fast lookup */
};

// Forward declarations used by the public API
static inline ms_Token *LexerTokenNew(ms_Lexer *lex, enum ms_TokenType type, const char *value, size_t len);
static inline ms_Token *LexerTokenError(ms_Lexer *lex, const char *msg);
static inline ms_Token *LexerTokenFromBuffer(ms_Lexer *lex, ms_TokenType type);
static inline int LexerNextChar(ms_Lexer *lex);
static inline int LexerPeek(ms_Lexer *lex);
static ms_Token *LexerLexNumber(ms_Lexer *lex, int prev);
static ms_Token *LexerLexWord(ms_Lexer *lex, int prev);
static ms_Token *LexerLexString(ms_Lexer *lex, int first);
static bool LexerAcceptOne(ms_Lexer *lex, const char *valid);
static int LexerAcceptRun(ms_Lexer *lex, const char *valid);
static int LexerAcceptExcept(ms_Lexer *lex, const char *invalid);
static inline void LexerIncrementLine(ms_Lexer *lex);
static inline void LexerBackup(ms_Lexer *lex);
static inline void LexerAddToBuffer(ms_Lexer *lex, int c);
static inline bool LexerResetBuffer(ms_Lexer *lex);
static ms_TokenType LexerGetWordTokenType(ms_Lexer *lex);
static bool LexerConstructKeywordCache(ms_Lexer *lex);
static inline bool InStr(int needle, const char *haystack);

/*
 * PUBLIC FUNCTIONS
 */

ms_Lexer *ms_LexerNew(void) {
    ms_Lexer* lex = calloc(sizeof(ms_Lexer), sizeof(ms_Lexer));
    if (!lex) {
        return NULL;
    }

    lex->reader = NULL;
    lex->buffer = NULL;
    lex->kwcache = NULL;
    if (!LexerConstructKeywordCache(lex)) {
        free(lex);
        return NULL;
    }
    return lex;
}

bool ms_LexerInitFile(ms_Lexer *lex, const char *fname) {
    assert(lex);

    dsbuf_destroy(lex->buffer);
    ms_StreamDestroy(lex->reader);
    lex->reader = ms_StreamNewFile(fname);
    if (!lex->reader) {
        return false;
    }

    lex->line = 1;
    lex->col = 0;
    lex->buffer = NULL;
    return LexerResetBuffer(lex);;
}

bool ms_LexerInitString(ms_Lexer *lex, const char *str) {
    return ms_LexerInitStringL(lex, str, strlen(str));
}

bool ms_LexerInitStringL(ms_Lexer *lex, const char *str, size_t len) {
    assert(lex);

    dsbuf_destroy(lex->buffer);
    ms_StreamDestroy(lex->reader);
    lex->reader = ms_StreamNewStringL(str, len);
    if (!lex->reader) {
        return false;
    }

    lex->line = 1;
    lex->col = 1;
    lex->buffer = NULL;
    return LexerResetBuffer(lex);
}

void ms_LexerDestroy(ms_Lexer *lex) {
    if (!lex) { return; }
    ms_StreamDestroy(lex->reader);
    lex->reader = NULL;
    dsbuf_destroy(lex->buffer);
    lex->buffer = NULL;
    dsdict_destroy(lex->kwcache);
    lex->kwcache = NULL;
    free(lex);
}

ms_Token *ms_LexerNextToken(ms_Lexer *lex) {
    assert(lex);
    int n = LexerNextChar(lex);
    if (n == EOF) { return NULL; }

begin_lex:              // Jump label for ignored input
    switch(n) {
        // Handle a potential EOF
        case EOF:
            return NULL;

            // Whitespace
        case '\r':
        case '\n':
        case ' ':
        case '\t':
        case '\f':
            n = LexerNextChar(lex);
            goto begin_lex;

            // Comments and standard division
        case '/':
            n = LexerNextChar(lex);
            if (n == '=') {
                return LexerTokenNew(lex, OP_DIVIDE_EQUALS, "/=", 2);
            } else if (n != '/') {
                return LexerTokenNew(lex, OP_DIVIDE, "/", 1);
            }
            n = LexerNextChar(lex);    // Ignore to the end of the line
            while (n != '\n' && n != '\r') { n = LexerNextChar(lex); }
            goto begin_lex;

            // Equality check
        case '=':
            n = LexerPeek(lex);
            if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_DOUBLE_EQ, "==", 2);
            }
            return LexerTokenError(lex, "=");

            // Assignment operator or colon separator
        case ':':
            n = LexerPeek(lex);
            if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_EQ, ":=", 2);
            }
            return LexerTokenNew(lex, COLON, ":", 1);

            // Addition and increment
        case '+':
            n = LexerPeek(lex);
            if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_PLUS_EQUALS, "+=", 2);
            }
            return LexerTokenNew(lex, OP_PLUS, "+", 1);

            // Subtract and decrement
        case '-':
            n = LexerPeek(lex);
            if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_MINUS_EQUALS, "-=", 2);
            }
            return LexerTokenNew(lex, OP_MINUS, "-", 1);

            // Multiplication and exponentiation
        case '*':
            n = LexerPeek(lex);
            if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_TIMES_EQUALS, "*=", 2);
            } else if (n == '*') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_EXPONENTIATE, "**", 2);
            }
            return LexerTokenNew(lex, OP_TIMES, "*", 1);

            // Integer division
        case '\\':
            n = LexerPeek(lex);
            if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_IDIVIDE_EQUALS, "\\=", 2);
            }
            return LexerTokenNew(lex, OP_IDIVIDE, "\\", 1);

            // Modulo
        case '%':
            n = LexerPeek(lex);
            if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_MODULO_EQUALS, "%=", 2);
            }
            return LexerTokenNew(lex, OP_MODULO, "%", 1);

            // Logical and bitwise AND
        case '&':
            n = LexerPeek(lex);
            if (n == '&') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_AND, "&&", 2);
            } else if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_BITWISE_AND_EQUALS, "&=", 2);
            }
            return LexerTokenNew(lex, OP_BITWISE_AND, "&", 1);

            // Logical and bitwise OR
        case '|':
            n = LexerPeek(lex);
            if (n == '|') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_OR, "||", 2);
            } else if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_BITWISE_OR_EQUALS, "|=", 2);
            }
            return LexerTokenNew(lex, OP_BITWISE_OR, "|", 1);

            // Bitwise NOT
        case '~':
            return LexerTokenNew(lex, OP_BITWISE_NOT, "~", 1);

            // Bitwise XOR
        case '^':
            n = LexerPeek(lex);
            if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_BITWISE_XOR_EQUALS, "^=", 2);
            }
            return LexerTokenNew(lex, OP_BITWISE_XOR, "^", 1);

            // Less than and less equal
        case '<':
            n = LexerPeek(lex);
            if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_LE, "<=", 2);
            } else if (n == '<') {
                (void)LexerNextChar(lex);
                n = LexerPeek(lex);
                if (n == '=') {
                    (void)LexerNextChar(lex);
                    return LexerTokenNew(lex, OP_SHIFT_LEFT_EQUALS, "<<=", 3);
                }
                return LexerTokenNew(lex, OP_SHIFT_LEFT, "<<", 2);
            }
            return LexerTokenNew(lex, OP_LT, "<", 1);

            // Greater than and greater equal
        case '>':
            n = LexerPeek(lex);
            if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_GE, ">=", 2);
            } else if (n == '>') {
                (void)LexerNextChar(lex);
                n = LexerPeek(lex);
                if (n == '=') {
                    (void)LexerNextChar(lex);
                    return LexerTokenNew(lex, OP_SHIFT_RIGHT_EQUALS, ">>=", 3);
                }
                return LexerTokenNew(lex, OP_SHIFT_RIGHT, ">>", 2);
            }
            return LexerTokenNew(lex, OP_GT, ">", 1);

            // Logical NOT and not equals
        case '!':
            n = LexerPeek(lex);
            if (n == '=') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_NOT_EQ, "!=", 2);
            }
            return LexerTokenNew(lex, OP_NOT, "!", 1);

            // Conditional operator
        case '?':
            n = LexerPeek(lex);
            if (n == '.') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_SAFE_REFERENCE, "?.", 2);
            } else if (n == '[') {
                (void)LexerNextChar(lex);
                return LexerTokenNew(lex, OP_SAFE_GETATTR, "?[", 2);
            }
            return LexerTokenNew(lex, QUESTION_MARK, "?", 1);

            // Various unambiguous symbols
        case '(':   return LexerTokenNew(lex, LPAREN, "(", 1);
        case ')':   return LexerTokenNew(lex, RPAREN, ")", 1);
        case '[':   return LexerTokenNew(lex, LBRACKET, "[", 1);
        case ']':   return LexerTokenNew(lex, RBRACKET, "]", 1);
        case '{':   return LexerTokenNew(lex, LBRACE, "{", 1);
        case '}':   return LexerTokenNew(lex, RBRACE, "}", 1);
        case ',':   return LexerTokenNew(lex, COMMA, ",", 1);
        case ';':   return LexerTokenNew(lex, SEMICOLON, ";", 1);

            // Potential numeric
        case '.':
            n = LexerPeek(lex);
            if (InStr(n, BASE_10_DIGITS)) {
                if (!LexerResetBuffer(lex)) { return NULL; }
                return LexerLexNumber(lex, '.');
            }
            return LexerTokenNew(lex, PERIOD, ".", 1);

            // Built-in functions
        case '$':
            if (!LexerResetBuffer(lex)) { return NULL; }
            return LexerLexWord(lex, n);

            // Database (global) reference
        case '@':
            if (!LexerResetBuffer(lex)) { return NULL; }
            return LexerLexWord(lex, n);

            // Strings
        case '"':
        case '\'':
            if (!LexerResetBuffer(lex)) { return NULL; }
            return LexerLexString(lex, n);

            // Numeric value
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            if (!LexerResetBuffer(lex)) { return NULL; }
            return LexerLexNumber(lex, n);

            // Unused (but invalid) symbols
        case '`':
            return LexerTokenError(lex, "`");
        case '#':
            return LexerTokenError(lex, "#");

            // Word
        default:
            if (!LexerResetBuffer(lex)) { return NULL; }
            return LexerLexWord(lex, n);
    }

    return NULL;
}

ms_Token *ms_TokenNew(ms_TokenType type, const char *value, size_t len, size_t line, size_t col) {
    ms_Token *tok = malloc(sizeof(ms_Token));
    if (!tok) {
        goto cleanup_token;
    }

    len = (len == 0) ? 1 : len;             /* guarantee that empty strings have a length since len must be >= 1 */
    tok->value = dsbuf_new_l(value, len);
    if (!tok->value) {
        goto cleanup_token_value;
    }

    tok->type = type;
    tok->line = line;
    tok->col = col;
    return tok;

cleanup_token_value:
    dsbuf_destroy(tok->value);
cleanup_token:
    free(tok);
    return NULL;
}

char *ms_TokenToString(ms_Token *tok) {
    if (!tok) { return 0; }
    const char* tok_name = ms_TokenName(tok);

    size_t len = 0;
    char *buf = NULL;

gen_token_string:
    len = snprintf(buf, len, "Token(%s, \"%s\", %zu, %zu)", tok_name,
                   dsbuf_char_ptr(tok->value), tok->line, tok->col);

    if (buf) {
        return buf;
    }

    buf = malloc(len + 1);
    if (!buf) {
        return NULL;
    }

    goto gen_token_string;
}

void ms_TokenDestroy(ms_Token *tok) {
    if (!tok) { return; }
    dsbuf_destroy(tok->value);
    tok->value = NULL;
    free(tok);
}

const char *ms_TokenName(ms_Token *tok) {
    assert(tok);
    return ms_TokenTypeName(tok->type);
}

const char *ms_TokenTypeName(ms_TokenType type) {
    switch(type) {
        case ERROR:                     return TOK_ERROR;
        case RESERVED_KW:               return TOK_RESERVED_KW;
        case IDENTIFIER:                return TOK_IDENTIFIER;
        case BUILTIN_FUNC:              return TOK_BUILTIN_FUNC;
        case GLOBAL:                    return TOK_GLOBAL;
        case FLOAT_NUMBER:              return TOK_FLOAT_NUMBER;
        case INT_NUMBER:                return TOK_INT_NUMBER;
        case HEX_NUMBER:                return TOK_HEX_NUMBER;
        case STRING:                    return TOK_STRING;
        case KW_FUNC:                   return TOK_KW_FUNC;
        case KW_IF:                     return TOK_KW_IF;
        case KW_ELSE:                   return TOK_KW_ELSE;
        case KW_RETURN:                 return TOK_KW_RETURN;
        case KW_FOR:                    return TOK_KW_FOR;
        case KW_IMPORT:                 return TOK_KW_IMPORT;
        case KW_CONTINUE:               return TOK_KW_CONTINUE;
        case KW_BREAK:                  return TOK_KW_BREAK;
        case KW_VAR:                    return TOK_KW_VAR;
        case KW_DEL:                    return TOK_KW_DEL;
        case KW_TRUE:                   return TOK_KW_TRUE;
        case KW_FALSE:                  return TOK_KW_FALSE;
        case KW_NULL:                   return TOK_KW_NULL;
        case KW_IS:                     return TOK_KW_IS;
        case KW_IN:                     return TOK_KW_IN;
        case KW_SELECT:                 return TOK_KW_SELECT;
        case OP_UMINUS:                 return TOK_OP_UMINUS;
        case OP_PLUS:                   return TOK_OP_PLUS;
        case OP_MINUS:                  return TOK_OP_MINUS;
        case OP_TIMES:                  return TOK_OP_TIMES;
        case OP_DIVIDE:                 return TOK_OP_DIVIDE;
        case OP_IDIVIDE:                return TOK_OP_IDIVIDE;
        case OP_MODULO:                 return TOK_OP_MODULO;
        case OP_PLUS_EQUALS:            return TOK_OP_PLUS_EQUALS;
        case OP_MINUS_EQUALS:           return TOK_OP_MINUS_EQUALS;
        case OP_TIMES_EQUALS:           return TOK_OP_TIMES_EQUALS;
        case OP_DIVIDE_EQUALS:          return TOK_OP_DIVIDE_EQUALS;
        case OP_IDIVIDE_EQUALS:         return TOK_OP_IDIVIDE_EQUALS;
        case OP_MODULO_EQUALS:          return TOK_OP_MODULO_EQUALS;
        case OP_AND:                    return TOK_OP_AND;
        case OP_OR:                     return TOK_OP_OR;
        case OP_EXPONENTIATE:           return TOK_OP_EXPONENTIATE;
        case OP_DOUBLE_EQ:              return TOK_OP_DOUBLE_EQ;
        case OP_GT:                     return TOK_OP_GT;
        case OP_LT:                     return TOK_OP_LT;
        case OP_EQ:                     return TOK_OP_EQ;
        case OP_NOT:                    return TOK_OP_NOT;
        case OP_NOT_EQ:                 return TOK_NOT_EQ;
        case OP_GE:                     return TOK_OP_GE;
        case OP_LE:                     return TOK_OP_LE;
        case OP_BITWISE_AND:            return TOK_OP_BITWISE_AND;
        case OP_BITWISE_OR:             return TOK_OP_BITWISE_OR;
        case OP_BITWISE_XOR:            return TOK_OP_BITWISE_XOR;
        case OP_BITWISE_NOT:            return TOK_OP_BITWISE_NOT;
        case OP_SHIFT_LEFT:             return TOK_OP_SHIFT_LEFT;
        case OP_SHIFT_RIGHT:            return TOK_OP_SHIFT_RIGHT;
        case OP_BITWISE_AND_EQUALS:     return TOK_OP_BITWISE_AND_EQUALS;
        case OP_BITWISE_OR_EQUALS:      return TOK_OP_BITWISE_OR_EQUALS;
        case OP_BITWISE_XOR_EQUALS:     return TOK_OP_BITWISE_XOR_EQUALS;
        case OP_SHIFT_LEFT_EQUALS:      return TOK_OP_SHIFT_LEFT_EQUALS;
        case OP_SHIFT_RIGHT_EQUALS:     return TOK_OP_SHIFT_RIGHT_EQUALS;
        case OP_SAFE_REFERENCE:         return TOK_OP_SAFE_REFERENCE;
        case OP_SAFE_GETATTR:           return TOK_OP_SAFE_GETATTR;
        case COLON:                     return TOK_COLON;
        case LPAREN:                    return TOK_LPAREN;
        case RPAREN:                    return TOK_RPAREN;
        case LBRACKET:                  return TOK_LBRACKET;
        case RBRACKET:                  return TOK_RBRACKET;
        case LBRACE:                    return TOK_LBRACE;
        case RBRACE:                    return TOK_RBRACE;
        case PERIOD:                    return TOK_PERIOD;
        case COMMA:                     return TOK_COMMA;
        case SEMICOLON:                 return TOK_SEMICOLON;
        case QUESTION_MARK:             return TOK_QUESTION_MARK;
        case NEWLINE_TOK:               return TOK_NEWLINE;
    }

    assert(false);
    return NULL;
}

/*
 * PRIVATE FUNCTIONS
 */

// Create a new token with the given type and value.
static inline ms_Token *LexerTokenNew(ms_Lexer *lex, ms_TokenType type, const char *value, size_t len) {
    assert(lex);
    return ms_TokenNew(type, value, len, lex->line, lex->col);
}

// Create a new Error token with the given message
static inline ms_Token *LexerTokenError(ms_Lexer *lex, const char *msg) {
    assert(lex);
    assert(msg);
    return ms_TokenNew(ERROR, msg, strlen(msg), lex->line, lex->col);
}

// Return a token from the value stored in the current Lexer buffer
static inline ms_Token *LexerTokenFromBuffer(ms_Lexer *lex, ms_TokenType type) {
    assert(lex);
    assert(lex->buffer);
    const char *buf = dsbuf_char_ptr(lex->buffer);
    size_t len = dsbuf_len(lex->buffer);
    return LexerTokenNew(lex, type, buf, len);
}

// Return the next character in the stream.
//
// Returns EOF if there are no more characters in the stream
// or an error occurs.
static inline int LexerNextChar(ms_Lexer *lex) {
    assert(lex);
    assert(lex->reader);
    int c = ms_StreamNextChar(lex->reader);
    if (c != EOF) { lex->col++; }
    return c;
}

// Peek at the next character in the stream without consuming it.
static inline int LexerPeek(ms_Lexer *lex) {
    assert(lex);
    int n = LexerNextChar(lex);
    if (n != EOF) { LexerBackup(lex); }
    return n;
}

// Starting at the character prev, attempt to lex an entire
// numeric token and return it to the caller.
static ms_Token *LexerLexNumber(ms_Lexer *lex, int prev) {
    assert(lex);
    LexerAddToBuffer(lex, prev);

    // Accept hexadecimal numbers
    if ((prev == '0') && (LexerAcceptOne(lex, "xX"))) {
        if (LexerAcceptRun(lex, BASE_16_DIGITS) <= 0) {
            return LexerTokenFromBuffer(lex, ERROR);
        }
        return LexerTokenFromBuffer(lex, HEX_NUMBER);
    }

    // Accept basic digits
    ms_TokenType type = (prev == '.') ? FLOAT_NUMBER : INT_NUMBER;
    LexerAcceptRun(lex, BASE_10_DIGITS);

    // Accept standard decimal digits
    if ((prev != '.') && (LexerAcceptOne(lex, "."))) {
        LexerAcceptRun(lex, BASE_10_DIGITS);
        type = FLOAT_NUMBER;
    }

    // Accept exponential notation
    if (LexerAcceptOne(lex, "eE")) {
        LexerAcceptRun(lex, BASE_10_DIGITS);
        type = FLOAT_NUMBER;
    }

    return LexerTokenFromBuffer(lex, type);
}

// Starting at the character prev, attempt to lex an entire
// word token and return it to the caller.
static ms_Token *LexerLexWord(ms_Lexer *lex, int prev) {
    assert(lex);

    // Accept every character except symbols and spaces
    LexerAddToBuffer(lex, prev);
    int num = LexerAcceptExcept(lex, SYMBOLS);

    // Determine token type (since there are a few others)
    // Make sure we got at least one character for our
    // global and built-in function identifiers
    ms_TokenType type;
    switch (prev) {
        case '@':
            type = (num < 1) ? ERROR : GLOBAL;
            break;
        case '$':
            type = (num < 1) ? ERROR : BUILTIN_FUNC;
            break;
        default:
            type = LexerGetWordTokenType(lex);
    }

    if (type == ERROR) {
        return LexerTokenFromBuffer(lex, ERROR);
    }
    return LexerTokenFromBuffer(lex, type);
}

// Starting at the character prev, attempt to lex an entire
// string token and return it to the caller.
static ms_Token *LexerLexString(ms_Lexer *lex, int first) {
    assert(lex);

    int n;
    int prev = EOF;

    // Accept every next character but:
    // - an escaped version of the opening quotation mark
    // - a new line (this is an error)
    while ((n = LexerNextChar(lex)) != EOF) {
        if ((n == first) && (prev != '\\')) {
            break;
        }
        if ((n == '\n') || (n == '\r')) {
            LexerIncrementLine(lex);
            return LexerTokenFromBuffer(lex, ERROR);
        }
        LexerAddToBuffer(lex, n);
        prev = n;
    }

    if ((n == EOF) && (prev != first)) {
        return LexerTokenFromBuffer(lex, ERROR);
    }
    return LexerTokenFromBuffer(lex, STRING);
}

// Accept the next character in the stream if it appears within
// the string valid. Return true if that character was accepted.
//
// Does not consume the next character.
static bool LexerAcceptOne(ms_Lexer *lex, const char *valid) {
    assert(lex);
    assert(valid);

    int n = LexerNextChar(lex);
    if (n == EOF) { return false; }

    if (InStr(n, valid)) {
        LexerAddToBuffer(lex, n);
        return true;
    }

    LexerBackup(lex);
    return false;
}

// Accept the as many characters from the stream as possible which
// appear in the input string valid.
static int LexerAcceptRun(ms_Lexer *lex, const char *valid) {
    assert(lex);
    assert(valid);

    int count = 0;
    int n;
    while ((n = LexerNextChar(lex)) != EOF) {
        if (InStr(n, valid)) {
            count++;
            LexerAddToBuffer(lex, n);
        } else {
            LexerBackup(lex);
            break;  // Break if the last character didn't match
        }
    }

    return count;
}

// Accept any character outside of the range given by the "invalid"
// string input.
static int LexerAcceptExcept(ms_Lexer *lex, const char *invalid) {
    assert(lex);
    assert(invalid);

    int count = 0;
    int n;
    while ((n = LexerNextChar(lex)) != EOF) {
        if (InStr(n, invalid)) {
            LexerBackup(lex);
            break;  // Break if the last character didn't match
        } else {
            count++;
            LexerAddToBuffer(lex, n);
        }
    }

    return count;
}

// Increment the line counter and position counter for the lexer.
static inline void LexerIncrementLine(ms_Lexer *lex) {
    assert(lex);
    lex->line++;
    lex->col = 1;
}

// Backup from the previous character.
static inline void LexerBackup(ms_Lexer *lex) {
    assert(lex);
    int res = ms_StreamUnread(lex->reader);
    if (res != EOF) {
        lex->col--;
    }
}

// Add the given character to the lexer's buffer
static inline void LexerAddToBuffer(ms_Lexer *lex, int c) {
    assert(lex);
    assert(lex->buffer);
    dsbuf_append_char(lex->buffer, c);
}

// Reset the internal buffer
static inline bool LexerResetBuffer(ms_Lexer *lex) {
    assert(lex);
    dsbuf_destroy(lex->buffer);
    lex->buffer = dsbuf_new_buffer(DEFAULT_TOKEN_BUFFER);
    if (!lex->buffer) { return false; }
    return true;
}

// Check if the value in the current lexer buffer is a keyword.
static ms_TokenType LexerGetWordTokenType(ms_Lexer *lex) {
    assert(lex);
    assert(lex->buffer);
    assert(lex->kwcache);

    const char *word = dsbuf_char_ptr(lex->buffer);
    ms_TokenType *type = dsdict_get(lex->kwcache, (void *)word);
    if (!type) {
        return IDENTIFIER;
    }

    return *type;
}

// Construct the keyword cache hash table
static bool LexerConstructKeywordCache(ms_Lexer *lex) {
    assert(lex);
    if (lex->kwcache) { return true; }

    lex->kwcache = dsdict_new((dsdict_hash_fn)hash_fnv1,
                              (dsdict_compare_fn)strcmp, NULL, NULL);
    if (!lex->kwcache) {
        return false;
    }

    size_t len = sizeof(KEYWORDS) / sizeof(KEYWORDS[0]);
    for (size_t i = 0; i < len; i++) {
        dsdict_put(lex->kwcache, (void *)KEYWORDS[i].kw, &KEYWORDS[i].type);
    }

    return true;
}

// Check if a single character is in a string.
static inline bool InStr(int needle, const char *haystack) {
    assert(needle != '\0');
    assert(haystack);

    for (int i = 0; haystack[i]; i++) {
        if (needle == haystack[i]) {
            return true;
        }
    }

    return false;
}
