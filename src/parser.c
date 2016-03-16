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
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include "libds/array.h"
#include "libds/dict.h"
#include "libds/hash.h"
#include "lexer.h"
#include "parser.h"
#include "lang.h"
#include "vm.h"

/*
 * FORWARD DECLARATIONS
 */

static const int EXPRESSION_LIST_DEFAULT_LEN = 10;

struct ms_Parser {
    ms_Lexer *lex;                          /** lexer object */
    ms_Token *cur;                          /** current token */
    ms_Token *nxt;                          /** "lookahead" token */
    size_t line;                            /** current line */
    size_t col;                             /** current column */
    ms_AST *ast;                            /** current abstract syntax tree */
    ms_ParseError *err;                     /** current parser error */
};

static const char *const ERR_OUT_OF_MEMORY = "Out of memory";
static const char *const ERR_MISMATCHED_PARENS = "Mismatched parentheses (ln: %d, col: %d)";
static const char *const ERR_EXPECTED_EXPRESSION = "Expected expression (ln: %d, col: %d)";
static const char *const ERR_INVALID_SYNTAX_GOT_TOK = "Invalid syntax '%s' (ln: %d, col: %d)";

static ms_ParseResult ParserParseExpression(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseOrExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseAndExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseEqualityExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseComparisonExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseBitwiseOrExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseBitwiseXorExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseBitwiseAndExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseBitShiftExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseArithmeticExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseTermExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParsePowerExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseUnaryExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseAtomExpr(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseAccessor(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseExprList(ms_Parser *prs, ms_Expr **list);
static ms_ParseResult ParserParseAtom(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserExprCombineBinary(ms_Parser *prs, ms_Expr *left, ms_ExprBinaryOp op, ms_Expr *right, ms_Expr **newexpr);
static ms_ParseResult ParserExprCombineUnary(ms_Parser *prs, ms_Expr *inner, ms_ExprUnaryOp op, ms_Expr **newexpr);

static inline ms_Token *ParserAdvanceToken(ms_Parser *prs);
static inline void ParserConsumeToken(ms_Parser *prs);
static bool ParserExpectToken(ms_Parser *prs, ms_TokenType type);
static void ParserErrorSet(ms_Parser *prs, const char* msg, const ms_Token *tok, ...);
static void ParseErrorDestroy(ms_ParseError *err);

/*
 * PUBLIC FUNCTIONS
 */

ms_Parser *ms_ParserNew(void) {
    ms_Parser * prs = malloc(sizeof(ms_Parser));
    if (!prs) {
        return NULL;
    }

    prs->lex = ms_LexerNew();
    if (!prs->lex) {
        ms_ParserDestroy(prs);
        return NULL;
    }

    prs->cur = NULL;
    prs->nxt = NULL;
    prs->ast = NULL;
    prs->err = NULL;
    return prs;
}

bool ms_ParserInitFile(ms_Parser *prs, const char *fname) {
    if (!prs) {
        return false;
    }

    if (!ms_LexerInitFile(prs->lex, fname)) {
        return false;
    }

    ms_TokenDestroy(prs->cur);
    prs->cur = ms_LexerNextToken(prs->lex);
    if (!prs->cur) {
        return false;
    }
    prs->line = prs->cur->line;
    prs->col = prs->cur->col;

    ms_TokenDestroy(prs->nxt);
    prs->nxt = ms_LexerNextToken(prs->lex);

    ms_ASTDestroy(prs->ast);
    prs->ast = NULL;

    if (prs->err) {
        ParseErrorDestroy(prs->err);
        prs->err = NULL;
    }
    return true;
}

bool ms_ParserInitString(ms_Parser *prs, const char *str) {
    return ms_ParserInitStringL(prs, str, strlen(str));
}

bool ms_ParserInitStringL(ms_Parser *prs, const char *str, size_t len) {
    if (!prs) {
        return false;
    }

    if (!ms_LexerInitStringL(prs->lex, str, len)) {
        return false;
    }

    ms_TokenDestroy(prs->cur);
    prs->cur = ms_LexerNextToken(prs->lex);
    if (!prs->cur) {
        return false;
    }
    prs->line = prs->cur->line;
    prs->col = prs->cur->col;

    ms_TokenDestroy(prs->nxt);
    prs->nxt = ms_LexerNextToken(prs->lex);

    ms_ASTDestroy(prs->ast);
    prs->ast = NULL;

    if (prs->err) {
        ParseErrorDestroy(prs->err);
        prs->err = NULL;
    }
    return true;
}

ms_ParseResult ms_ParserParse(ms_Parser *prs, ms_VMByteCode **code, const ms_AST **ast, const ms_ParseError **err) {
    assert(prs);
    assert(code);
    assert(err);

    if (prs->ast) {
        ms_ASTDestroy(prs->ast);
    }

    *err = NULL;
    ms_ParseResult res = ParserParseExpression(prs, &prs->ast);

    if (prs->cur) {
        ParserErrorSet(prs, ERR_INVALID_SYNTAX_GOT_TOK, prs->cur,
                       dsbuf_char_ptr(prs->cur->value), prs->line, prs->col);
        res = PARSE_ERROR;
    }

    if (res == PARSE_ERROR) {
        *err = prs->err;
        *code = NULL;
    } else {
        *code = ms_ExprToOpCodes(prs->ast);
        if (ast) {
            *ast = prs->ast;
        }
    }
    return res;
}

void ms_ParserDestroy(ms_Parser *prs) {
    if (!prs) { return; }
    ms_LexerDestroy(prs->lex);
    prs->lex = NULL;
    ms_TokenDestroy(prs->cur);
    prs->cur = NULL;
    ms_ASTDestroy(prs->ast);
    prs->ast = NULL;
    ParseErrorDestroy(prs->err);
    prs->err = NULL;
    free(prs);
}

/*
 * PRIVATE FUNCTIONS
 */

/*
 * Expression Grammar:
 *
 * expr:            or_expr ('||' or_expr)*
 * or_expr:         and_expr ('&&' and_expr)*
 * and_expr:        eq_expr (('!='|'==') eq_expr)*
 * eq_expr:         cmp_expr (('>'|'>='|'<'|'<=') cmp_expr)*
 * cmp_expr:        bor_expr ('|' bor_expr)*
 * bor_expr:        bxor_expr ('@' bxor_expr)*
 * bxor_expr:       band_expr ('&' band_expr)*
 * band_expr:       shift_expr (('<<'|'>>') shift_expr)*
 * shift_expr:      arith_expr (('+'|'-') arith_expr)*
 * arith_expr:      term_expr (('*'|'/'|'\'|'%') term_expr)*
 * power_expr:      atom_expr ('**' arith_expr)*
 * term_expr:       ('-'|'!'|'~') atom_expr | power_expr
 * atom_expr:       atom [accessor]*
 * atom:            NUMBER | STRING | KW_TRUE | KW_FALSE | KW_NULL |
 *                  IDENTIFIER | BUILTIN_FUNC | expr | '(' expr ')'
 *
 * accessor:        arg_list
 * expr_list:       expr [',' expr]*
 * arg_list:        '(' expr_list ')'
 */

static ms_ParseResult ParserParseExpression(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);
    return ParserParseOrExpr(prs, expr);
}

static ms_ParseResult ParserParseOrExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParseAndExpr(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    while (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprBinaryOp op;
        switch (cur->type) {
            case OP_OR:          op = BINARY_OR;             break;
            default:
                *expr = left;
                return res;
        }

        ParserConsumeToken(prs);
        ms_Expr *right;
        if ((res = ParserParseAndExpr(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseAndExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParseEqualityExpr(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    while (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprBinaryOp op;
        switch (cur->type) {
            case OP_AND:          op = BINARY_AND;             break;
            default:
                *expr = left;
                return res;
        }

        ParserConsumeToken(prs);
        ms_Expr *right;
        if ((res = ParserParseEqualityExpr(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseEqualityExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParseComparisonExpr(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    while (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprBinaryOp op;
        switch (cur->type) {
            case OP_DOUBLE_EQ:          op = BINARY_EQ;             break;
            case OP_NOT_EQ:             op = BINARY_NOT_EQ;         break;
            default:
                *expr = left;
                return res;
        }

        ParserConsumeToken(prs);
        ms_Expr *right;
        if ((res = ParserParseComparisonExpr(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseComparisonExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParseBitwiseOrExpr(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    while (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprBinaryOp op;
        switch (cur->type) {
            case OP_GT:          op = BINARY_GT;          break;
            case OP_GE:          op = BINARY_GE;          break;
            case OP_LT:          op = BINARY_LT;          break;
            case OP_LE:          op = BINARY_LE;          break;
            default:
                *expr = left;
                return res;
        }

        ParserConsumeToken(prs);
        ms_Expr *right;
        if ((res = ParserParseBitwiseOrExpr(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseBitwiseOrExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParseBitwiseXorExpr(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    while (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprBinaryOp op;
        switch (cur->type) {
            case OP_BITWISE_OR:          op = BINARY_BITWISE_OR;          break;
            default:
                *expr = left;
                return res;
        }

        ParserConsumeToken(prs);
        ms_Expr *right;
        if ((res = ParserParseBitwiseXorExpr(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseBitwiseXorExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParseBitwiseAndExpr(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    while (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprBinaryOp op;
        switch (cur->type) {
            case OP_BITWISE_XOR:          op = BINARY_BITWISE_XOR;          break;
            default:
                *expr = left;
                return res;
        }

        ParserConsumeToken(prs);
        ms_Expr *right;
        if ((res = ParserParseBitwiseAndExpr(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseBitwiseAndExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParseBitShiftExpr(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    while (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprBinaryOp op;
        switch (cur->type) {
            case OP_BITWISE_AND:          op = BINARY_BITWISE_AND;          break;
            default:
                *expr = left;
                return res;
        }

        ParserConsumeToken(prs);
        ms_Expr *right;
        if ((res = ParserParseBitShiftExpr(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseBitShiftExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParseArithmeticExpr(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    while (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprBinaryOp op;
        switch (cur->type) {
            case OP_SHIFT_LEFT:          op = BINARY_SHIFT_LEFT;          break;
            case OP_SHIFT_RIGHT:         op = BINARY_SHIFT_RIGHT;         break;
            default:
                *expr = left;
                return res;
        }

        ParserConsumeToken(prs);
        ms_Expr *right;
        if ((res = ParserParseArithmeticExpr(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseArithmeticExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParseTermExpr(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    while (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprBinaryOp op;
        switch (cur->type) {
            case OP_PLUS:          op = BINARY_PLUS;          break;
            case OP_MINUS:         op = BINARY_MINUS;         break;
            default:
                *expr = left;
                return res;
        }

        ParserConsumeToken(prs);
        ms_Expr *right;
        if ((res = ParserParseTermExpr(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseTermExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParsePowerExpr(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    while (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprBinaryOp op;
        switch (cur->type) {
            case OP_TIMES:          op = BINARY_TIMES;          break;
            case OP_DIVIDE:         op = BINARY_DIVIDE;         break;
            case OP_IDIVIDE:        op = BINARY_IDIVIDE;        break;
            case OP_MODULO:         op = BINARY_MODULO;         break;
            default:
                *expr = left;
                return res;
        }

        ParserConsumeToken(prs);
        ms_Expr *right;
        if ((res = ParserParsePowerExpr(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParsePowerExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParseUnaryExpr(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    while (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprBinaryOp op;
        switch (cur->type) {
            case OP_EXPONENTIATE:   op = BINARY_EXPONENTIATE;   break;
            default:
                *expr = left;
                return res;
        }

        ParserConsumeToken(prs);
        ms_Expr *right;
        if ((res = ParserParseTermExpr(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseUnaryExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res = PARSE_ERROR;
    *expr = NULL;

    if (prs->cur) {
        ms_Token *cur = prs->cur;
        ms_ExprUnaryOp op;
        switch (cur->type) {
            case OP_BITWISE_NOT:        op = UNARY_BITWISE_NOT;     break;
            case OP_NOT:                op = UNARY_NOT;             break;
            case OP_MINUS:              // fallthrough
            case OP_UMINUS:             op = UNARY_MINUS;           break;
            default:
                if ((res = ParserParseAtomExpr(prs, expr)) == PARSE_ERROR) {
                    return res;
                }
                goto parse_unary_expr_end_loop;
        }

        ParserConsumeToken(prs);

        ms_Expr *inner;
        if ((res = ParserParseUnaryExpr(prs, &inner)) == PARSE_ERROR) {
            return res;
        }
        if ((res = ParserExprCombineUnary(prs, inner, op, expr)) == PARSE_ERROR) {
            return res;
        }
    }

parse_unary_expr_end_loop:
    if (!(*expr)) {
        ParserErrorSet(prs, ERR_EXPECTED_EXPRESSION, NULL, prs->line, prs->col);
    }

    return res;
}

static ms_ParseResult ParserParseAtomExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left;
    if ((res = ParserParseAtom(prs, &left)) == PARSE_ERROR) {
        return res;
    }

    if (!ParserExpectToken(prs, LPAREN)) {
        *expr = left;
        return res;
    }

    while (prs->cur) {
        ms_Expr *right;
        if ((res = ParserParseAccessor(prs, &right)) == PARSE_ERROR) {
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, BINARY_CALL, right, &combined)) == PARSE_ERROR) {
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseAccessor(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    if (prs->cur->type == LPAREN) {
        ParserConsumeToken(prs);
        return ParserParseExprList(prs, expr);
    }

    return PARSE_SUCCESS;
}

static ms_ParseResult ParserParseExprList(ms_Parser *prs, ms_Expr **list) {
    assert(prs);
    assert(list);

    DSArray *params = dsarray_new_cap(EXPRESSION_LIST_DEFAULT_LEN, NULL,
                                      (dsarray_free_fn)ms_ExprDestroy);
    if (!params) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    *list = ms_ExprNewWithList(params);
    if (!(*list)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        dsarray_destroy(params);
        return PARSE_ERROR;
    }

    /* no parameters, close and return */
    if ((prs->cur) && (prs->cur->type == RPAREN)) {
        ParserConsumeToken(prs);
        return PARSE_SUCCESS;
    }

    /* produce the set of parameters */
    while(prs->cur) {
        ms_Expr *param;
        if (ParserParseExpression(prs, &param) == PARSE_ERROR) {
            dsarray_destroy(params);
            return PARSE_ERROR;
        }
        dsarray_append(params, param);
        if (prs->cur) {
            if (prs->cur->type == RPAREN) {
                ParserConsumeToken(prs);
                break;
            }

            if (!ParserExpectToken(prs, COMMA)) {
                ms_ExprDestroy(*list);
                return PARSE_ERROR;
            }
            ParserConsumeToken(prs);
        }
    }

    return PARSE_SUCCESS;
}

static ms_ParseResult ParserParseAtom(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res = PARSE_SUCCESS;
    ms_Token *cur = prs->cur;

    if (!cur) {
        ParserErrorSet(prs, ERR_EXPECTED_EXPRESSION, NULL, prs->line, prs->col);
        return PARSE_ERROR;
    }

    switch (cur->type) {
            /* floating point number literals */
        case FLOAT_NUMBER: {
            const char *val = dsbuf_char_ptr(cur->value);
            *expr = ms_ExprFloatFromString(val);
            if (!(*expr)) {
                ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                res = PARSE_ERROR;
            }
            break;
        }

            /* integer literals */
        case INT_NUMBER:
        case HEX_NUMBER: {
            const char *val = dsbuf_char_ptr(cur->value);
            *expr = ms_ExprIntFromString(val);
            if (!(*expr)) {
                ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                res = PARSE_ERROR;
            }
            break;
        }

            /* string literals */
        case STRING: {
            ms_VMData p;
            p.s = cur->value;
            *expr = ms_ExprNewWithVal(VMVAL_STR, p);
            if (!(*expr)) {
                ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                res = PARSE_ERROR;
            }
            cur->value = NULL; /* prevent the buffer being destroyed when the token is destroyed */
            break;
        }

            /* boolean literals */
        case KW_TRUE:
        case KW_FALSE: {
            ms_VMData p;
            p.b = (cur->type == KW_TRUE) ? true : false;
            *expr = ms_ExprNewWithVal(VMVAL_BOOL, p);
            if (!(*expr)) {
                ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                res = PARSE_ERROR;
            }
            break;
        }

            /* null literal */
        case KW_NULL: {
            ms_VMData p;
            p.n = MS_VM_NULL_POINTER;
            *expr = ms_ExprNewWithVal(VMVAL_NULL, p);
            if (!(*expr)) {
                ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                res = PARSE_ERROR;
            }
            break;
        }

            /* reference an identifier (either builtin or other identifier) */
        case IDENTIFIER:
        case BUILTIN_FUNC: {
            *expr = ms_ExprNewWithIdent(dsbuf_char_ptr(cur->value));
            if (!(*expr)) {
                ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                res = PARSE_ERROR;
            }
            break;
        }

            /* parenthetical expression */
        case LPAREN:
            ParserConsumeToken(prs);
            if ((res = ParserParseExpression(prs, expr)) == PARSE_ERROR) {
                return res;
            }
            if (!ParserExpectToken(prs, RPAREN)) {
                ParserErrorSet(prs, ERR_MISMATCHED_PARENS, prs->cur, prs->line, prs->col);
                return PARSE_ERROR;
            }
            break;

            /* encountered another expression perhaps */
        default:
            ParserErrorSet(prs, ERR_EXPECTED_EXPRESSION, prs->cur, prs->line, prs->col);
            res = PARSE_ERROR;
            break;
    }

    ParserConsumeToken(prs);
    return res;
}

static ms_ParseResult ParserExprCombineBinary(ms_Parser *prs, ms_Expr *left, ms_ExprBinaryOp op, ms_Expr *right, ms_Expr **newexpr) {
    assert(prs);
    assert(left);
    assert(right);
    assert(newexpr);

    *newexpr = ms_ExprNew(EXPRTYPE_BINARY);
    if (!(*newexpr)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, NULL);
        return PARSE_ERROR;
    }

    *newexpr = ms_ExprFlatten(*newexpr, left, EXPRLOC_LEFT);
    *newexpr = ms_ExprFlatten(*newexpr, right, EXPRLOC_RIGHT);
    (*newexpr)->expr.b->op = op;

    return PARSE_SUCCESS;
}

static ms_ParseResult ParserExprCombineUnary(ms_Parser *prs, ms_Expr *inner, ms_ExprUnaryOp op, ms_Expr **newexpr) {
    assert(prs);
    assert(inner);
    assert(newexpr);
    assert(op != UNARY_NONE);

    *newexpr = ms_ExprNew(EXPRTYPE_UNARY);
    if (!(*newexpr)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, NULL);
        return PARSE_ERROR;
    }

    *newexpr = ms_ExprFlatten(*newexpr, inner, EXPRLOC_UNARY);
    (*newexpr)->expr.u->op = op;

    return PARSE_SUCCESS;
}

// Move the pointer to the next token in the lexer stream without
// discarding the previous token.
//
// BE CAREFUL WITH THIS FUNCTION. Abusing this function will cause memory
// leaks since token objects are not freed by this function.
static inline ms_Token *ParserAdvanceToken(ms_Parser *prs) {
    assert(prs);
    assert(prs->lex);
    ms_Token *old = prs->cur;
    prs->cur = prs->nxt;
    if (prs->cur) {
        prs->line = prs->cur->line;
        prs->col = prs->cur->col;
    }
    prs->nxt = (prs->nxt) ? (ms_LexerNextToken(prs->lex)) : (NULL);
    return old;
}

// Consume the next token in the stream.
static inline void ParserConsumeToken(ms_Parser *prs) {
    ms_TokenDestroy(ParserAdvanceToken(prs));
}

// Check if the next token to see if it matches our expected next token.
static bool ParserExpectToken(ms_Parser *prs, ms_TokenType type) {
    assert(prs);

    if ((!prs->cur) || (prs->cur->type != type)) {
        return false;
    }

    return true;
}

// Generate a new ParseError object attached to the Parser.
static void ParserErrorSet(ms_Parser *prs, const char *msg, const ms_Token *tok, ...) {
    assert(prs);

    if (prs->err) {
        ParseErrorDestroy(prs->err);
        prs->err = NULL;
    }

    prs->err = malloc(sizeof(ms_ParseError));
    if (!prs->err) {
        return;
    }

    va_list args, args2;
    va_start(args, tok);
    va_copy(args2, args);

    int len = vsnprintf(NULL, 0, msg, args);
    prs->err->msg = malloc((size_t)len + 1);
    if (prs->err->msg) {
        vsnprintf(prs->err->msg, len + 1, msg, args2);
    }

    prs->err->tok = (tok) ?
                    ms_TokenNew(tok->type, dsbuf_char_ptr(tok->value),
                                dsbuf_len(tok->value), tok->line, tok->col) :
                    NULL;
    va_end(args);
    va_end(args2);
    return;
}

// Clean up a Parse Error object.
static void ParseErrorDestroy(ms_ParseError *err) {
    if (!err) { return; }
    ms_TokenDestroy(err->tok);
    err->tok = NULL;
    free(err->msg);
    err->msg = NULL;
    free(err);
}
