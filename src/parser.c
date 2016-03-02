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
    ms_ParseError *err;
};

static const char *const ERR_OUT_OF_MEMORY = "Out of memory";
static const char *const ERR_MISMATCHED_PARENS = "Mismatched parentheses (ln: %d, col: %d)";
static const char *const ERR_EXPECTED_OPERAND = "Expected operand (ln: %d, col: %d)";
static const char *const ERR_EXPECTED_BINARY_OP = "Expected binary operator (ln: %d, col: %d)";
static const char *const ERR_EXPECTING_TOK = "Expecting '%s'";
static const char *const ERR_EXPECTING_TOK_GOT_TOK = "Expecting '%s', got '%s' (ln: %d, col: %d)";
static const char *const ERR_INVALID_SYNTAX_GOT_TOK = "Invalid syntax '%s' (ln: %d, col: %d)";

static ms_ParseResult ParserParseExpression(ms_Parser *prs, ms_Expr **expr);
static bool ParserExprShouldCombine(ms_Parser *prs, ms_Token *top, ms_Token *next);
static ms_ParseResult ParserExprCombine(ms_Parser *prs, DSArray *exprstack, ms_Token *tok);
static inline ms_Token *ParserCurrentToken(ms_Parser *prs);
static inline ms_Token *ParserNextToken(ms_Parser *prs);
static inline void ParserConsumeToken(ms_Parser *prs);
static bool ParserExpectToken(ms_Parser *prs, ms_TokenType type);
static bool ParserConstructOpCache(ms_Parser *prs);
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

    if (!ParserConstructOpCache(prs)) {
        ms_ParserDestroy(prs);
        return NULL;
    }

    prs->cur = NULL;
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

    if (prs->err) {
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
    dsdict_destroy(prs->opcache);
    prs->opcache = NULL;
    ParseErrorDestroy(prs->err);
    prs->err = NULL;
    free(prs);
}

/*
 * PRIVATE FUNCTIONS
 */

// Parse out an expression value from the input stream
static ms_ParseResult ParserParseExpression(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    // Data structure for defining parse tree
    DSArray *exprstack = dsarray_new_cap(EXPRESSION_STACK_DEFAULT_LEN,
                                         NULL, (dsarray_free_fn)ms_ExprDestroy);
    if (!exprstack) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, NULL);
        return PARSE_ERROR;
    }

    // Operator stack
    DSArray *opstack = dsarray_new_cap(OPERATOR_STACK_DEFAULT_LEN,
                                       NULL, (dsarray_free_fn)ms_TokenDestroy);
    if (!opstack) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, NULL);
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
                /* floating point number literals */
            case FLOAT_NUMBER: {
                const char *val = dsbuf_char_ptr(cur->value);
                ms_Expr *newexpr = ms_ExprFloatFromString(val);
                if (!newexpr) {
                    ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                dsarray_append(exprstack, newexpr);
                break;
            }

                /* integer literals */
            case INT_NUMBER:
            case HEX_NUMBER: {
                const char *val = dsbuf_char_ptr(cur->value);
                ms_Expr *newexpr = ms_ExprIntFromString(val);
                if (!newexpr) {
                    ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                dsarray_append(exprstack, newexpr);
                break;
            }

                /* string literals */
            case STRING: {
                ms_VMPrimitive p;
                p.s = cur->value;
                ms_Expr *newexpr = ms_ExprNewWithVal(VMVAL_STR, p);
                if (!newexpr) {
                    ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                cur->value = NULL; /* prevent the buffer being destroyed when the token is destroyed */
                dsarray_append(exprstack, newexpr);
                break;
            }

                /* boolean literals */
            case KW_TRUE:
            case KW_FALSE: {
                ms_VMPrimitive p;
                p.b = (cur->type == KW_TRUE) ? true : false;
                ms_Expr *newexpr = ms_ExprNewWithVal(VMVAL_BOOL, p);
                if (!newexpr) {
                    ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                dsarray_append(exprstack, newexpr);
                break;
            }

                /* null literal */
            case KW_NULL: {
                ms_VMPrimitive p;
                p.n = MS_VM_NULL_POINTER;
                ms_Expr *newexpr = ms_ExprNewWithVal(VMVAL_NULL, p);
                if (!newexpr) {
                    ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                dsarray_append(exprstack, newexpr);
                break;
            }

                /* begin parenthesized expression */
            case LPAREN: {
                dsarray_append(opstack, cur);
                cleanup_token = (void (*)(ms_Parser *))ParserNextToken;       /* leave reference to token */
                break;
            }

                /* attempt to complete the parenthesized expression */
            case RPAREN: {
                bool found = false;
                while (dsarray_len(opstack) > 0) {
                    ms_Token *tok = dsarray_top(opstack);
                    if (tok->type != LPAREN) {
                        tok = dsarray_pop(opstack);
                        if ((res = ParserExprCombine(prs, exprstack, tok)) == PARSE_ERROR) {
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
                    ParserErrorSet(prs, ERR_MISMATCHED_PARENS, cur, cur->line, cur->col);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                break;
            }

                /* determine if these unary operators are to the LEFT of their operand */
            case OP_BITWISE_NOT:
            case OP_NOT: {
                if ((prev) && (!ms_TokenTypeIsOp(prevtype)) && (prevtype != LPAREN)) {
                    ParserErrorSet(prs, ERR_EXPECTED_OPERAND, cur, cur->line, cur->col);
                    res = PARSE_ERROR;
                    goto parse_expr_cleanup;
                }
                goto parse_expr_evaluate_op;
            }
                /* determine if this is a unary or binary minus sign */
            case OP_MINUS: {
                if ((!prev) || (ms_TokenTypeIsOp(prevtype)) || (prevtype == LPAREN)) {
                    cur->type = OP_UMINUS;
                    dsarray_append(opstack, cur);
                    cleanup_token = (void (*)(ms_Parser *))ParserNextToken;       /* leave reference to token */
                    break;
                }
                goto parse_expr_evaluate_op;    /* in case this case gets separated from below */
            }

            /* goto label for unary minus, logical not, bitwise not operators */
parse_expr_evaluate_op:

                /* evaluate standard binary operators */
            case OP_PLUS:
            case OP_TIMES:
            case OP_DIVIDE:
            case OP_IDIVIDE:
            case OP_MODULO:
            case OP_EXPONENTIATE:
            case OP_SHIFT_LEFT:
            case OP_SHIFT_RIGHT:
            case OP_BITWISE_AND:
            case OP_BITWISE_OR:
            case OP_BITWISE_XOR:
            case OP_LE:
            case OP_LT:
            case OP_GE:
            case OP_GT:
            case OP_DOUBLE_EQ:
            case OP_EQ:
            case OP_NOT_EQ:
            case OP_AND:
            case OP_OR: {
                ms_Token *top = dsarray_top(opstack);
                if ((dsarray_len(opstack) > 0) && ParserExprShouldCombine(prs, top, cur)) {
                    top = dsarray_pop(opstack);
                    if ((res = ParserExprCombine(prs, exprstack, top)) == PARSE_ERROR) {
                        goto parse_expr_cleanup;
                    }
                    ms_TokenDestroy(top);
                }
                dsarray_append(opstack, cur);
                cleanup_token = (void (*)(ms_Parser *))ParserNextToken;       /* leave reference to token */
                break;
            }

                /* handle erroneous tokens in the input */
            default:
                ParserErrorSet(prs, ERR_INVALID_SYNTAX_GOT_TOK, cur,
                               dsbuf_char_ptr(cur->value), cur->line, cur->col);
                res = PARSE_ERROR;
                goto parse_expr_cleanup;
        }

        prevtype = cur->type;
        cleanup_token(prs);
    }

    while (dsarray_len(opstack) > 0) {
        /* intentionally always pop this so it sticks around in case
         * of error (isn't cleaned up by dsarray_destroy calls below) */
        ms_Token *tok = dsarray_pop(opstack);
        if ((tok->type == RPAREN) || (tok->type == LPAREN)) {
            ParserErrorSet(prs, ERR_MISMATCHED_PARENS, tok, tok->line, tok->col);
            res = PARSE_ERROR;
            goto parse_expr_cleanup;
        }
        if ((res = ParserExprCombine(prs, exprstack, tok)) == PARSE_ERROR) {
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
static ms_ParseResult ParserExprCombine(ms_Parser *prs, DSArray *exprstack, ms_Token *tok) {
    assert(prs);
    assert(exprstack);
    assert(tok);

    // Handle the unary minus
    ms_ExprUnaryOp uop = ms_ExprTokenToUnaryOp(tok->type);
    if (uop != UNARY_NONE) {
        ms_Expr *expr = ms_ExprNew(EXPRTYPE_UNARY);
        if (!expr) {
            ParserErrorSet(prs, ERR_OUT_OF_MEMORY, tok);
            return PARSE_ERROR;
        }

        if(dsarray_len(exprstack) < 1) {
            ParserErrorSet(prs, ERR_EXPECTED_OPERAND, tok, tok->line, tok->col);
            return PARSE_ERROR;
        }
        ms_Expr *operand = dsarray_pop(exprstack);

        expr = ms_ExprFlatten(expr, operand, EXPRLOC_UNARY);
        expr->expr.u->op = uop;

        dsarray_append(exprstack, expr);
        return PARSE_SUCCESS;
    }

    // Binary operations
    ms_ExprBinaryOp op = ms_ExprTokenToBinaryOp(tok->type);
    if (op == BINARY_EMPTY) {
        ParserErrorSet(prs, ERR_EXPECTED_BINARY_OP, tok, tok->line, tok->col);
        return PARSE_ERROR;
    }

    ms_Expr *expr = ms_ExprNew(EXPRTYPE_BINARY);
    if (!expr) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, tok);
        return PARSE_ERROR;
    }

    if(dsarray_len(exprstack) < 2) {
        ParserErrorSet(prs, ERR_EXPECTED_OPERAND, tok, tok->line, tok->col);
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
static bool ParserExpectToken(ms_Parser *prs, ms_TokenType type) {
    assert(prs);

    if (!prs->cur) {
        ParserErrorSet(prs, ERR_EXPECTING_TOK, NULL, ms_TokenTypeName(type));
    } else if(prs->cur->type != type) {
        ParserErrorSet(prs, ERR_EXPECTING_TOK_GOT_TOK, prs->cur, ms_TokenTypeName(type),
                       ms_TokenName(prs->cur), prs->cur->line, prs->cur->col);
    }

    return (prs->err == NULL);
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
static void ParserErrorSet(ms_Parser *prs, const char* msg, const ms_Token *tok, ...) {
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

    prs->err->tok = ms_TokenNew(tok->type, dsbuf_char_ptr(tok->value),
                                dsbuf_len(tok->value), tok->line, tok->col);
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
