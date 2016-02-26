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

static const int EXPRESSION_STACK_DEFAULT_LEN = 10;
static const int OPERATOR_STACK_DEFAULT_LEN = 10;

struct ms_Parser {
    ms_Lexer *lex;
    ms_Token *cur;
    ms_AST *ast;
    DSDict *opcache;
};

static const char *const EXPECTING_GOT_ERROR = "PARSE ERROR: Expecting '%s', got '%s' (ln: %d, col: %d)";
static const char *const EXPECTING_TOKEN_ERROR = "PARSE ERROR: Expecting '%s', found nothing";

static ms_ParseResult ParserParseExpression(ms_Parser *prs, ms_Expr **expr, ms_ParseError **err);
static bool ParserExprShouldCombine(ms_Parser *prs, ms_Token *top, ms_Token *next);
static ms_ParseResult ParserExprCombine(DSArray *exprstack, ms_Token *op, ms_ParseError **err);
static inline ms_Token *ParserCurrentToken(ms_Parser *prs);
static inline ms_Token *ParserNextToken(ms_Parser *prs);
static inline void ParserConsumeToken(ms_Parser *prs);
static bool ParserExpectToken(ms_Parser *prs, ms_TokenType type, ms_ParseError *err);
static bool ParserConstructOpCache(ms_Parser *prs);
static ms_ParseError *ParseErrorNew(const char* msg, ms_Token *tok, ...);

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

    if (!ParserConstructOpCache(prs)) {
        ms_ParserDestroy(prs);
        return NULL;
    }

    prs->cur = NULL;
    prs->ast = NULL;
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

    ms_ASTDestroy(prs->ast);
    prs->ast = NULL;
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

    ms_ASTDestroy(prs->ast);
    prs->ast = NULL;
    return true;
}

void ms_ParserDestroy(ms_Parser *prs) {
    if (!prs) { return; }
    ms_LexerDestroy(prs->lex);
    ms_TokenDestroy(prs->cur);
    ms_ASTDestroy(prs->ast);
    free(prs);
}

ms_ParseResult ms_ParserParse(ms_Parser *prs, ms_VMByteCode **code, ms_ParseError **err) {
    assert(prs);
    assert(code);
    assert(err);

    *err = NULL;
    ms_ParseResult res = ParserParseExpression(prs, &prs->ast, err);
    *code = (*err) ? NULL : ms_ExprToOpCodes(prs->ast);
    return res;
}

void ms_ParseErrorDestroy(ms_ParseError *err) {
    if (!err) { return; }
    ms_TokenDestroy(err->tok);
    free(err->msg);
    free(err);
}

/*
 * PRIVATE FUNCTIONS
 */

// Parse out an expression value from the input stream
static ms_ParseResult ParserParseExpression(ms_Parser *prs, ms_Expr **expr, ms_ParseError **err) {
    assert(prs);
    assert(expr);
    assert(err);

    // Data structure for defining parse tree
    DSArray *exprstack = dsarray_new_cap(EXPRESSION_STACK_DEFAULT_LEN,
                                         NULL, (dsarray_free_fn)ms_ExprDestroy);
    if (!exprstack) {
        *err = ParseErrorNew("Out of memory", NULL);
        return PARSE_ERROR;
    }

    // Operator stack
    DSArray *opstack = dsarray_new_cap(OPERATOR_STACK_DEFAULT_LEN,
                                       NULL, (dsarray_free_fn)ms_TokenDestroy);
    if (!opstack) {
        *err = ParseErrorNew("Out of memory", NULL);
        dsarray_destroy(exprstack);
        return PARSE_ERROR;
    }

    // Iterate on every token in the stream performing Shunting Yard
    ms_TokenType prevtype = ERROR;
    ms_ParseResult res = PARSE_SUCCESS;
    for (ms_Token *cur = ParserCurrentToken(prs), *prev = NULL; cur;
            prev = cur, cur = ParserCurrentToken(prs)) {
        // By default, consume (destroy) the current token and advance the pointer
        void (*cleanup_token)(ms_Parser *) = ParserConsumeToken;

        switch (cur->type) {
            case NUMBER:        // Fall through
            case HEX_NUMBER: {
                const char *val = dsbuf_char_ptr(cur->value);
                ms_Expr *newexpr = ms_ExprNumberFromString(val);
                if (!newexpr) {
                    *err = ParseErrorNew("Out of memory", cur);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                dsarray_append(exprstack, newexpr);
                break;
            }
            case STRING: {
                ms_VMPrimitive p;
                p.s = cur->value;
                ms_Expr *newexpr = ms_ExprNewWithVal(VMVAL_STR, p);
                if (!newexpr) {
                    *err = ParseErrorNew("Out of memory", cur);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                cur->value = NULL; /* prevent the buffer being destroyed when the token is destroyed */
                dsarray_append(exprstack, newexpr);
                break;
            }
            case KW_TRUE:       // Fall through
            case KW_FALSE: {
                ms_VMPrimitive p;
                p.b = (cur->type == KW_TRUE) ? true : false;
                ms_Expr *newexpr = ms_ExprNewWithVal(VMVAL_BOOL, p);
                if (!newexpr) {
                    *err = ParseErrorNew("Out of memory", cur);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                dsarray_append(exprstack, newexpr);
                break;
            }
            case KW_NULL: {
                ms_VMPrimitive p;
                p.n = MS_VM_NULL_POINTER;
                ms_Expr *newexpr = ms_ExprNewWithVal(VMVAL_NULL, p);
                if (!newexpr) {
                    *err = ParseErrorNew("Out of memory", cur);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                dsarray_append(exprstack, newexpr);
                break;
            }
            case LPAREN: {
                dsarray_append(opstack, cur);
                cleanup_token = (void (*)(ms_Parser *))ParserNextToken;       /* leave reference to token */
                break;
            }
            case RPAREN: {
                bool found = false;
                while (dsarray_len(opstack) > 0) {
                    ms_Token *tok = dsarray_top(opstack);
                    if (tok->type != LPAREN) {
                        tok = dsarray_pop(opstack);
                        if ((res = ParserExprCombine(exprstack, tok, err)) == PARSE_ERROR) {
                            goto parse_expr_cleanup;
                        }
                        ms_TokenDestroy(tok);
                    } else {
                        found = true;
                        ms_Token *op = dsarray_pop(opstack);
                        ms_TokenDestroy(op);
                        break;
                    }
                }
                if (!found) {
                    *err = ParseErrorNew("Mismatched parentheses", cur);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                break;
            }
            case OP_MINUS: {    // Unary minus case
                if ((!prev) || (ms_TokenTypeIsOp(prevtype)) || (prevtype == LPAREN)) {
                    cur->type = OP_UMINUS;
                    dsarray_append(opstack, cur);
                    cleanup_token = (void (*)(ms_Parser *))ParserNextToken;       /* leave reference to token */
                    break;
                }
            }
            default: {
                ms_Token *top = dsarray_top(opstack);
                if ((dsarray_len(opstack) > 0) && ParserExprShouldCombine(prs, top, cur)) {
                    top = dsarray_pop(opstack);
                    if ((res = ParserExprCombine(exprstack, top, err)) == PARSE_ERROR) {
                        goto parse_expr_cleanup;
                    }
                    ms_TokenDestroy(top);
                }
                dsarray_append(opstack, cur);
                cleanup_token = (void (*)(ms_Parser *))ParserNextToken;       /* leave reference to token */
                break;
            }
        }

        prevtype = cur->type;
        cleanup_token(prs);
    }

    while (dsarray_len(opstack) > 0) {
        /* intentionally always pop this so it sticks around in case
         * of error (isn't cleaned up by dsarray_destroy calls below) */
        ms_Token *tok = dsarray_pop(opstack);
        if ((tok->type == RPAREN) || (tok->type == LPAREN)) {
            *err = ParseErrorNew("Mismatched parentheses", tok);
            res = PARSE_ERROR;
            goto parse_expr_cleanup;
        }
        if ((res = ParserExprCombine(exprstack, tok, err)) == PARSE_ERROR) {
            goto parse_expr_cleanup;
        }
        ms_TokenDestroy(tok);
    }

    *expr = dsarray_pop(exprstack);

parse_expr_cleanup:
    dsarray_destroy(opstack);
    dsarray_destroy(exprstack);
    return res;
}

// Decide if the token at the top of the operator stack should
// be used in combining a new expression before adding the next token.
static bool ParserExprShouldCombine(ms_Parser *prs, ms_Token *top, ms_Token *next) {
    assert(prs);
    assert(top);
    assert(next);

    if (top->type == LPAREN) {
        return false;
    }

    const char *topname = ms_TokenName(top);
    const char *nextname = ms_TokenName(next);
    ms_ExprOpPrecedence *topop = dsdict_get(prs->opcache, (void *)topname);
    ms_ExprOpPrecedence *nextop = dsdict_get(prs->opcache, (void *)nextname);

    if ((nextop->assoc == ASSOC_LEFT) && (nextop->precedence <= topop->precedence)) {
        return true;
    }

    if ((nextop->assoc == ASSOC_RIGHT) && (nextop->precedence < topop->precedence)) {
        return true;
    }

    return false;
}

// Combine an expression using the order of precedence defined for
static ms_ParseResult ParserExprCombine(DSArray *exprstack, ms_Token *tok, ms_ParseError **err) {
    assert(exprstack);
    assert(tok);

    // Handle the unary minus
    if (tok->type == OP_UMINUS) {
        ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
        if (!expr) {
            *err = ParseErrorNew("Out of memory", tok);
            return PARSE_ERROR;
        }

        if(dsarray_len(exprstack) < 1) {
            *err = ParseErrorNew("Expected an operand, but got none", tok);
            return PARSE_ERROR;
        }
        ms_Expr *operand = dsarray_pop(exprstack);

        expr = ms_ExprFlatten(expr, operand, EXPRLOC_UNARY);
        expr->expr.u->op = UNARY_MINUS;

        dsarray_append(exprstack, expr);
        return PARSE_SUCCESS;
    }

    // Binary operations
    ms_ExprBinaryOp op = ms_ExprTokenToBinaryOp(tok->type);
    if (op == BINARY_EMPTY) {
        *err = ParseErrorNew("Expected a binary operator but got none.", tok);
        return PARSE_ERROR;
    }

    ms_Expr *expr = ms_ExprNew(EXPRTYPE_BINARY);
    if (!expr) {
        *err = ParseErrorNew("Out of memory", tok);
        return PARSE_ERROR;
    }

    if(dsarray_len(exprstack) < 2) {
        *err = ParseErrorNew("Expected an operand, but got none", tok);
        return PARSE_ERROR;
    }

    ms_Expr *second = dsarray_pop(exprstack);
    ms_Expr *first = dsarray_pop(exprstack);

    expr = ms_ExprFlatten(expr, first, EXPRLOC_LEFT);
    expr = ms_ExprFlatten(expr, second, EXPRLOC_RIGHT);
    expr->expr.b->op = op;

    dsarray_append(exprstack, expr);
    return PARSE_SUCCESS;
}

// Peek at the next token in the stream.
static inline ms_Token *ParserCurrentToken(ms_Parser *prs) {
    assert(prs);
    return prs->cur;
}

// Move the pointer to the next token in the lexer stream without
// discarding the previous token.
//
// BE CAREFUL WITH THIS FUNCTION. Abusing this function will cause memory
// leaks since token objects are not freed by this function.
static inline ms_Token *ParserNextToken(ms_Parser *prs) {
    assert(prs);
    assert(prs->lex);
    ms_Token *old = prs->cur;
    prs->cur = ms_LexerNextToken(prs->lex);
    return old;
}

// Consume the next token in the stream.
static inline void ParserConsumeToken(ms_Parser *prs) {
    assert(prs);
    assert(prs->lex);
    ms_TokenDestroy(prs->cur);
    prs->cur = ms_LexerNextToken(prs->lex);
    return;
}

// Check if the next token to see if it matches our expected next token.
static bool ParserExpectToken(ms_Parser *prs, ms_TokenType type, ms_ParseError *err) {
    assert(prs);

    if (!prs->cur) {
        err = ParseErrorNew(EXPECTING_TOKEN_ERROR, NULL, ms_TokenTypeName(type));
    } else if(prs->cur->type != type) {
        err = ParseErrorNew(EXPECTING_GOT_ERROR, prs->cur, ms_TokenTypeName(type),
                               ms_TokenName(prs->cur), prs->cur->line, prs->cur->col);
    }

    return (err == NULL);
}

// Construct an operator cache for constant time access to operator information
static bool ParserConstructOpCache(ms_Parser *prs) {
    assert(prs);

    prs->opcache = dsdict_new((dsdict_hash_fn)hash_fnv1,
                              (dsdict_compare_fn)strcmp, NULL, NULL);
    if (!prs->opcache) {
        return false;
    }

    const ms_ExprOpPrecedence *tbl;
    size_t len = ms_ExprOpPrecedenceTable(&tbl);
    for (size_t i = 0; i < len; i++) {
        const ms_ExprOpPrecedence *op = &tbl[i];
        const char *name = ms_TokenTypeName(op->type);
        dsdict_put(prs->opcache, (void *)name, (void *)op);
    }

    return true;
}

// Generate a new ParseError object.
static ms_ParseError *ParseErrorNew(const char* msg, ms_Token *tok, ...) {
    ms_ParseError *err = malloc(sizeof(ms_ParseError));
    if (!err) {
        return NULL;
    }

    size_t len = strlen(msg);
    err->msg = malloc(len + (size_t)(.25 * len));
    if (!err->msg) {
        return NULL;
    }

    va_list vargs;
    va_start(vargs, tok); /* tok is the last named parameter in the list */

    vsprintf(err->msg, msg, vargs);
    if (!err->msg) {
        ms_ParseErrorDestroy(err);
        va_end(vargs);
        return NULL;
    }

    err->tok = tok;
    va_end(vargs);
    return err;
}
