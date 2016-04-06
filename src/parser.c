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

static const size_t ARGUMENT_LIST_DEFAULT_CAP = 10;
static const size_t EXPRESSION_LIST_DEFAULT_CAP = 10;
static const size_t MODULE_DEFAULT_CAP = 10;
static const size_t STATEMENT_BLOCK_DEFAULT_CAP = 10;

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
static const char *const ERR_EXPECTED_IDENTIFIER = "Expected identifier (ln: %d, col: %d)";
static const char *const ERR_EXPECTED_KEYWORD = "Expected keyword '%s' (ln: %d, col: %d)";
static const char *const ERR_EXPECTED_STATEMENT = "Expected statement (ln: %d, col: %d)";
static const char *const ERR_EXPECTED_TOKEN = "Expected '%s' (ln: %d, col: %d)";
static const char *const ERR_INVALID_SYNTAX_GOT_TOK = "Invalid syntax '%s' (ln: %d, col: %d)";
static const char *const ERR_MUST_ASSIGN_TO_IDENT = "Assignment target must be identifier (ln: %d, col: %d)";
static const char *const ERR_MUST_ASSIGN_TO_QIDENT = "Assignment target must be identifier or qualified identifier (ln: %d, col: %d)";
static const char *const ERR_MUST_IMPORT_QIDENT = "Imported module must be identifier or qualified identifier (ln: %d, col: %d)";

static ms_ParseResult ParserParseModule(ms_Parser *prs, ms_Module **module);
static ms_ParseResult ParserParseStatement(ms_Parser *prs, ms_Stmt **stmt);
static ms_ParseResult ParserParseBlock(ms_Parser *prs, ms_StmtBlock **block);
static ms_ParseResult ParserParseDeleteStatement(ms_Parser *prs, ms_StmtDelete **del);
static ms_ParseResult ParserParseForStatement(ms_Parser *prs, ms_StmtFor **forstmt);
static ms_ParseResult ParserParseForIncrement(ms_Parser *prs, ms_Expr *ident, bool declare, ms_StmtForIncrement **inc, ms_StmtBlock **block);
static ms_ParseResult ParserParseForIterator(ms_Parser *prs, ms_Expr *ident, bool declare, ms_StmtForIterator **iter, ms_StmtBlock **block);
static ms_ParseResult ParserParseIfStatement(ms_Parser *prs, ms_StmtIf **ifstmt);
static ms_ParseResult ParserParseElseStatement(ms_Parser *prs, ms_StmtIfElse **elif);
static ms_ParseResult ParserParseImportStatement(ms_Parser *prs, ms_StmtImport **import);
static ms_ParseResult ParserParseMergeStatement(ms_Parser *prs, ms_StmtMerge **merge);
static ms_ParseResult ParserParseReturnStatement(ms_Parser *prs, ms_StmtReturn **ret);
static ms_ParseResult ParserParseFunctionDeclaration(ms_Parser *prs, ms_StmtDeclaration **decl);
static ms_ParseResult ParserParseDeclaration(ms_Parser *prs, bool req_keyword, ms_StmtDeclaration **decl);
static ms_ParseResult ParserParseAssignment(ms_Parser *prs, ms_Stmt **stmt);
static ms_ParseResult ParserParseSimpleAssignment(ms_Parser *prs, ms_Expr *name, ms_Stmt **stmt);
static ms_ParseResult ParserParseCompoundAssignment(ms_Parser *prs, ms_Expr *name, ms_Stmt **stmt);
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
static ms_ParseResult ParserParseAccessor(ms_Parser *prs, ms_Expr **expr, ms_ExprBinaryOp *op);
static ms_ParseResult ParserParseExprList(ms_Parser *prs, ms_Expr **list, ms_TokenType closer);
static ms_ParseResult ParserParseAtom(ms_Parser *prs, ms_Expr **expr);
static ms_ParseResult ParserParseFunctionExpression(ms_Parser *prs, bool require_name, ms_Expr **expr);
static ms_ParseResult ParserExprCombineBinary(ms_Parser *prs, ms_Expr *left, ms_ExprBinaryOp op, ms_Expr *right, ms_Expr **newexpr);
static ms_ParseResult ParserExprCombineUnary(ms_Parser *prs, ms_Expr *inner, ms_ExprUnaryOp op, ms_Expr **newexpr);

static inline ms_Token *ParserAdvanceToken(ms_Parser *prs);
static inline void ParserConsumeToken(ms_Parser *prs);
static inline void ParserConsumeNewlines(ms_Parser *prs);
static bool ParserExpectToken(ms_Parser *prs, ms_TokenType type);
static bool ParserExpectTokenIfNotEOF(ms_Parser *prs, ms_TokenType type);
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
        prs->ast = NULL;
    }

    *err = NULL;
    ms_ParseResult res = ParserParseModule(prs, &prs->ast);

    if ((res != PARSE_ERROR) && (prs->cur)) {
        ParserErrorSet(prs, ERR_INVALID_SYNTAX_GOT_TOK, prs->cur,
                       dsbuf_char_ptr(prs->cur->value), prs->line, prs->col);
        res = PARSE_ERROR;
    }

    if (res == PARSE_ERROR) {
        *err = prs->err;
        *code = NULL;
    } else {
        //*code = ms_ExprToOpCodes(prs->ast);
        *code = NULL;
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
    ms_TokenDestroy(prs->nxt);
    prs->nxt = NULL;
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
 * module:          (stmt)*
 * stmt:            'break' | 'continue' | del_stmt | for_stmt | if_stmt |
 *                  import_stmt | merge_stmt | ret_stmt | func_decl |
 *                  declare | assign | expr
 * for_stmt:        'for' ('var') expr ':=' expr (':' expr (':' expr)) |
 *                  'for' ('var') expr 'in' expr block |
 *                  'for' expr block
 * if_stmt:         'if' expr block (else_stmt)
 * else_stmt:       'else' if_stmt | 'else' block
 * del_stmt:        'delete' expr
 * import_stmt:     'import' expr (':' IDENTIFIER)
 * merge_stmt:      'merge' expr ':=' expr
 * ret_stmt:        'return' (expr)
 * block:           '{' stmt* '}'
 * func_decl:       'func' IDENTIFIER '(' ident_list ')' block
 * declare:         'var' IDENTIFIER (':=' expr) (',' IDENTIFIER (':=' expr))*
 * assign:          expr (':=' | '+=' | '-=' | '*=' | '/=' | '\=' | '%=') expr
 *
 * expr:            or_expr ('||' or_expr)*
 * or_expr:         and_expr ('&&' and_expr)*
 * and_expr:        eq_expr (('!='|'==') eq_expr)*
 * eq_expr:         cmp_expr (('>'|'>='|'<'|'<=') cmp_expr)*
 * cmp_expr:        bor_expr ('|' bor_expr)*
 * bor_expr:        bxor_expr ('^' bxor_expr)*
 * bxor_expr:       band_expr ('&' band_expr)*
 * band_expr:       shift_expr (('<<'|'>>') shift_expr)*
 * shift_expr:      arith_expr (('+'|'-') arith_expr)*
 * arith_expr:      term_expr (('*'|'/'|'\'|'%') term_expr)*
 * power_expr:      atom_expr ('**' arith_expr)*
 * term_expr:       ('-'|'!'|'~') atom_expr | power_expr
 * atom_expr:       atom [accessor]*
 * atom:            NUMBER | STRING | KW_TRUE | KW_FALSE | KW_NULL | GLOBAL |
 *                  IDENTIFIER | BUILTIN_FUNC | func_expr | expr | '(' expr ')'
 * func_expr:       'func' (IDENTIFIER) '(' ident_list ')' block
 *
 * accessor:        arg_list | sub_list | '.' IDENTIFIER
 * expr_list:       (expr (',' expr)*)
 * arg_list:        '(' expr_list ')'
 * sub_list:        '[' expr_list ']'
 * ident_list:      (IDENTIFIER (',' IDENTIFIER)*)
 */

/*
 * For the most part, the parsing functions bubble their return values up
 * as output parameters (pointers to pointers). It is for this reason that
 * very few functions appear to clean up their allocated memory in the case
 * of an error. However, note that everything ultimately bubbles up to the
 * parser `ast` field which is always cleaned up when the parser is destroyed
 * or whenever the parser is reinitialized with a new file or string.
 */

static ms_ParseResult ParserParseModule(ms_Parser *prs, ms_Module **module) {
    assert(prs);
    assert(module);

    *module = dsarray_new_cap(MODULE_DEFAULT_CAP, NULL,
                              (dsarray_free_fn)ms_StmtDestroy);
    if (!(*module)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    ParserConsumeNewlines(prs);
    while (prs->cur) {
        ms_Stmt *stmt = NULL;
        if (ParserParseStatement(prs, &stmt) == PARSE_ERROR) {
            ms_StmtDestroy(stmt);
            return PARSE_ERROR;
        }
        if (!ParserExpectTokenIfNotEOF(prs, NEWLINE_TOK)) {
            ms_StmtDestroy(stmt);
            ParserErrorSet(prs, ERR_EXPECTED_TOKEN, prs->cur, TOK_NEWLINE, prs->line, prs->col);
            return PARSE_ERROR;
        }
        ParserConsumeNewlines(prs);
        dsarray_append(*module, stmt);
    }

    return PARSE_SUCCESS;
}

static ms_ParseResult ParserParseStatement(ms_Parser *prs, ms_Stmt **stmt) {
    assert(prs);
    assert(stmt);

    ms_ParseResult res;
    ms_Token *cur = prs->cur;

    if (!cur) {
        ParserErrorSet(prs, ERR_EXPECTED_STATEMENT, NULL, prs->line, prs->col);
        return PARSE_ERROR;
    }

    *stmt = calloc(1, sizeof(ms_Stmt));
    if (!(*stmt)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }
    (*stmt)->type = STMTTYPE_EMPTY;

    switch (cur->type) {
        case KW_BREAK:
            (*stmt)->type = STMTTYPE_BREAK;
            (*stmt)->cmpnt.brk = NULL;
            ParserConsumeToken(prs);
            res = PARSE_SUCCESS;
            break;
        case KW_CONTINUE:
            (*stmt)->type = STMTTYPE_CONTINUE;
            (*stmt)->cmpnt.cont = NULL;
            ParserConsumeToken(prs);
            res = PARSE_SUCCESS;
            break;
        case KW_DEL:
            (*stmt)->type = STMTTYPE_DELETE;
            res = ParserParseDeleteStatement(prs, &(*stmt)->cmpnt.del);
            break;
        case KW_FOR:
            (*stmt)->type = STMTTYPE_FOR;
            res = ParserParseForStatement(prs, &(*stmt)->cmpnt.forstmt);
            break;
        case KW_IF:
            (*stmt)->type = STMTTYPE_IF;
            res = ParserParseIfStatement(prs, &(*stmt)->cmpnt.ifstmt);
            break;
        case KW_IMPORT:
            (*stmt)->type = STMTTYPE_IMPORT;
            res = ParserParseImportStatement(prs, &(*stmt)->cmpnt.import);
            break;
        case KW_MERGE:
            (*stmt)->type = STMTTYPE_MERGE;
            res = ParserParseMergeStatement(prs, &(*stmt)->cmpnt.merge);
            break;
        case KW_RETURN:
            (*stmt)->type = STMTTYPE_RETURN;
            res = ParserParseReturnStatement(prs, &(*stmt)->cmpnt.ret);
            break;
        case KW_FUNC:
            (*stmt)->type = STMTTYPE_DECLARATION;
            res = ParserParseFunctionDeclaration(prs, &(*stmt)->cmpnt.decl);
            break;
        case KW_VAR:
            (*stmt)->type = STMTTYPE_DECLARATION;
            res = ParserParseDeclaration(prs, true, &(*stmt)->cmpnt.decl);
            break;
        case IDENTIFIER:
            res = ParserParseAssignment(prs, stmt);
            break;
        default:
            (*stmt)->type = STMTTYPE_EXPRESSION;
            res = ParserParseExpression(prs, &(*stmt)->cmpnt.expr);
            break;
    }

    return res;
}

static ms_ParseResult ParserParseBlock(ms_Parser *prs, ms_StmtBlock **block) {
    assert(prs);
    assert(block);

    *block = dsarray_new_cap(STATEMENT_BLOCK_DEFAULT_CAP, NULL,
                             (dsarray_free_fn)ms_StmtDestroy);
    if (!(*block)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, LBRACE)) {
        ParserErrorSet(prs, ERR_EXPECTED_TOKEN, prs->cur, "{", prs->line, prs->col);
        return PARSE_ERROR;
    }

    ParserConsumeToken(prs);
    ParserConsumeNewlines(prs);

    while ((prs->cur) && (!ParserExpectToken(prs, RBRACE))) {
        ms_Stmt *stmt = NULL;
        if (ParserParseStatement(prs, &stmt) == PARSE_ERROR) {
            ms_StmtDestroy(stmt);
            return PARSE_ERROR;
        }
        if (ParserExpectToken(prs, RBRACE)) {
            ms_StmtDestroy(stmt);
            break;
        }
        if (!ParserExpectTokenIfNotEOF(prs, NEWLINE_TOK)) {
            ms_StmtDestroy(stmt);
            ParserErrorSet(prs, ERR_EXPECTED_TOKEN, prs->cur, TOK_NEWLINE, prs->line, prs->col);
            return PARSE_ERROR;
        }
        ParserConsumeNewlines(prs);
        dsarray_append(*block, stmt);
    }

    if (!ParserExpectToken(prs, RBRACE)) {
        ParserErrorSet(prs, ERR_EXPECTED_TOKEN, prs->cur, "}", prs->line, prs->col);
        return PARSE_ERROR;
    }

    ParserConsumeToken(prs);
    return PARSE_SUCCESS;
}

static ms_ParseResult ParserParseDeleteStatement(ms_Parser *prs, ms_StmtDelete **del) {
    assert(prs);
    assert(del);

    *del = calloc(1, sizeof(ms_StmtDelete));
    if (!(*del)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, KW_DEL)) {
        ParserErrorSet(prs, ERR_EXPECTED_KEYWORD, prs->cur, TOK_KW_DEL, prs->line, prs->col);
        return PARSE_ERROR;
    }

    ParserConsumeToken(prs);
    return ParserParseExpression(prs, &(*del)->expr);
}

static ms_ParseResult ParserParseForStatement(ms_Parser *prs, ms_StmtFor **forstmt) {
    assert(prs);
    assert(forstmt);

    *forstmt = calloc(1, sizeof(ms_StmtFor));
    if (!(*forstmt)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, KW_FOR)) {
        ParserErrorSet(prs, ERR_EXPECTED_KEYWORD, prs->cur, TOK_KW_FOR, prs->line, prs->col);
        return PARSE_ERROR;
    }
    ParserConsumeToken(prs);

    /* one of { iterator, incremented } FOR statement with declaration */
    bool declare = false;
    if (ParserExpectToken(prs, KW_VAR)) {
        ParserConsumeToken(prs);
        declare = true;
    }

    /* one of { iterator, incremented } FOR statement without declaration */
    ms_Expr *ident;
    if (ParserParseExpression(prs, &ident) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    /* verify that the expression is an identifier */
    if (declare) {
        if (!ms_ExprIsIdent(ident)) {
            ms_ExprDestroy(ident);
            ParserErrorSet(prs, ERR_MUST_ASSIGN_TO_IDENT, prs->cur, prs->line, prs->col);
            return PARSE_ERROR;
        }
    } else {
        if (!ms_ExprIsQualifiedIdent(ident)) {
            ms_ExprDestroy(ident);
            ParserErrorSet(prs, ERR_MUST_ASSIGN_TO_QIDENT, prs->cur, prs->line, prs->col);
            return PARSE_ERROR;
        }
    }

    if (ParserExpectToken(prs, OP_EQ)) {
        ParserConsumeToken(prs);
        (*forstmt)->type = FORSTMT_INCREMENT;
        return ParserParseForIncrement(prs, ident, declare, &(*forstmt)->clause.inc, &(*forstmt)->block);
    } else if (ParserExpectToken(prs, KW_IN)) {
        ParserConsumeToken(prs);
        (*forstmt)->type = FORSTMT_ITERATOR;
        return ParserParseForIterator(prs, ident, declare, &(*forstmt)->clause.iter, &(*forstmt)->block);
    }

    /* prohibit 'var' keyword for generic single expression FOR statements */
    if (declare) {
        ms_ExprDestroy(ident);
        ParserErrorSet(prs, ERR_EXPECTED_EXPRESSION, prs->cur, prs->line, prs->col);
        return PARSE_ERROR;
    }

    /* generic single expression FOR statement */
    (*forstmt)->type = FORSTMT_EXPR;
    (*forstmt)->clause.expr = calloc(1, sizeof(ms_StmtForExpr));
    if (!(*forstmt)->clause.expr) {
        ms_ExprDestroy(ident);
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }
    (*forstmt)->clause.expr->expr = ident;
    return ParserParseBlock(prs, &(*forstmt)->block);
}

static ms_ParseResult ParserParseForIncrement(ms_Parser *prs, ms_Expr *ident, bool declare, ms_StmtForIncrement **inc, ms_StmtBlock **block) {
    assert(prs);
    assert(ident);
    assert(inc);

    *inc = calloc(1, sizeof(ms_StmtForIncrement));
    if (!(*inc)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }
    (*inc)->declare = declare;
    (*inc)->ident = ident;

    if (ParserParseExpression(prs, &(*inc)->init) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, COLON)) {
        return PARSE_SUCCESS;
    }
    ParserConsumeToken(prs);

    if (ParserParseExpression(prs, &(*inc)->end) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, COLON)) {
        return PARSE_SUCCESS;
    }
    ParserConsumeToken(prs);

    if (ParserParseExpression(prs, &(*inc)->step) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    return ParserParseBlock(prs, block);
}

static ms_ParseResult ParserParseForIterator(ms_Parser *prs, ms_Expr *ident, bool declare, ms_StmtForIterator **iter, ms_StmtBlock **block) {
    assert(prs);
    assert(ident);
    assert(iter);

    *iter = calloc(1, sizeof(ms_StmtForIterator));
    if (!(*iter)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }
    (*iter)->declare = declare;
    (*iter)->ident = ident;

    if (ParserParseExpression(prs, &(*iter)->iter) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    return ParserParseBlock(prs, block);
}

static ms_ParseResult ParserParseIfStatement(ms_Parser *prs, ms_StmtIf **ifstmt) {
    assert(prs);
    assert(ifstmt);

    *ifstmt = calloc(1, sizeof(ms_StmtIf));
    if (!(*ifstmt)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, KW_IF)) {
        ParserErrorSet(prs, ERR_EXPECTED_KEYWORD, prs->cur, TOK_KW_IF, prs->line, prs->col);
        return PARSE_ERROR;
    }

    ParserConsumeToken(prs);
    if (ParserParseExpression(prs, &(*ifstmt)->expr) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    if (ParserParseBlock(prs, &(*ifstmt)->block) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, KW_ELSE)) {
        return PARSE_ERROR;
    }

    return ParserParseElseStatement(prs, &(*ifstmt)->elif);
}

static ms_ParseResult ParserParseElseStatement(ms_Parser *prs, ms_StmtIfElse **elif) {
    assert(prs);
    assert(elif);

    *elif = calloc(1, sizeof(ms_StmtIfElse));
    if (!(*elif)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, KW_ELSE)) {
        ParserErrorSet(prs, ERR_EXPECTED_KEYWORD, prs->cur, TOK_KW_ELSE, prs->line, prs->col);
        return PARSE_ERROR;
    }

    ParserConsumeToken(prs);
    if (ParserExpectToken(prs, KW_IF)) {
        (*elif)->type = IFELSE_IF;
        return ParserParseIfStatement(prs, &(*elif)->clause.ifstmt);
    }

    (*elif)->clause.elstmt = malloc(sizeof(ms_StmtElse));
    if (!(*elif)->clause.elstmt) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    (*elif)->type = IFELSE_ELSE;
    return ParserParseBlock(prs, &(*elif)->clause.elstmt->block);
}

static ms_ParseResult ParserParseImportStatement(ms_Parser *prs, ms_StmtImport **import) {
    assert(prs);
    assert(import);

    *import = calloc(1, sizeof(ms_StmtImport));
    if (!(*import)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, KW_IMPORT)) {
        ParserErrorSet(prs, ERR_EXPECTED_KEYWORD, prs->cur, TOK_KW_IMPORT, prs->line, prs->col);
        return PARSE_ERROR;
    }

    ParserConsumeToken(prs);
    if (ParserParseExpression(prs, &(*import)->ident) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    if (!ms_ExprIsQualifiedIdent((*import)->ident)) {
        ParserErrorSet(prs, ERR_MUST_IMPORT_QIDENT, prs->cur, prs->line, prs->col);
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, COLON)) {
        return PARSE_SUCCESS;
    }

    ParserConsumeToken(prs);

    if (!ParserExpectToken(prs, IDENTIFIER)) {
        ParserErrorSet(prs, ERR_EXPECTED_IDENTIFIER, prs->cur, prs->line, prs->col);
        return PARSE_ERROR;
    }

    /* steal the identifier from the token */
    (*import)->alias = prs->cur->value;
    prs->cur->value = NULL;
    ParserConsumeToken(prs);
    return PARSE_SUCCESS;
}

static ms_ParseResult ParserParseMergeStatement(ms_Parser *prs, ms_StmtMerge **merge) {
    assert(prs);
    assert(merge);

    *merge = calloc(1, sizeof(ms_StmtMerge));
    if (!(*merge)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, KW_MERGE)) {
        ParserErrorSet(prs, ERR_EXPECTED_KEYWORD, prs->cur, TOK_KW_MERGE, prs->line, prs->col);
        return PARSE_ERROR;
    }

    ParserConsumeToken(prs);
    if (ParserParseExpression(prs, &(*merge)->left) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, OP_EQ)) {
        ParserErrorSet(prs, ERR_EXPECTED_TOKEN, prs->cur, TOK_OP_EQ, prs->line, prs->col);
        return PARSE_ERROR;
    }

    ParserConsumeToken(prs);
    return ParserParseExpression(prs, &(*merge)->right);
}

static ms_ParseResult ParserParseReturnStatement(ms_Parser *prs, ms_StmtReturn **ret) {
    assert(prs);
    assert(ret);

    *ret = calloc(1, sizeof(ms_StmtReturn));
    if (!(*ret)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, KW_RETURN)) {
        ParserErrorSet(prs, ERR_EXPECTED_KEYWORD, prs->cur, TOK_KW_RETURN, prs->line, prs->col);
        return PARSE_ERROR;
    }

    ParserConsumeToken(prs);

    if (!prs->cur || ParserExpectToken(prs, NEWLINE_TOK) || ParserExpectToken(prs, RBRACE)) {
        ms_ValData p;
        p.n = MS_VM_NULL_POINTER;
        (*ret)->expr = ms_ExprNewWithVal(MSVAL_NULL, p);
        if (!(*ret)->expr) {
            ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
            return PARSE_ERROR;
        }

        ParserConsumeToken(prs);
        return PARSE_SUCCESS;
    }

    return ParserParseExpression(prs, &(*ret)->expr);
}

static ms_ParseResult ParserParseFunctionDeclaration(ms_Parser *prs, ms_StmtDeclaration **decl) {
    assert(prs);
    assert(decl);

    *decl = calloc(1, sizeof(ms_StmtDeclaration));
    if (!(*decl)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    if (ParserParseFunctionExpression(prs, true, &(*decl)->expr) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    const ms_Ident *name = (*decl)->expr->cmpnt.u->atom.val.val.fn->ident;  /* just... lol */
    (*decl)->ident = dsbuf_dup(name);
    if (!(*decl)->ident) {
        return PARSE_ERROR;
    }

    return PARSE_SUCCESS;
}

static ms_ParseResult ParserParseDeclaration(ms_Parser *prs, bool req_keyword, ms_StmtDeclaration **decl) {
    assert(prs);
    assert(decl);

    *decl = calloc(1, sizeof(ms_StmtDeclaration));
    if (!(*decl)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    if ((req_keyword) && (!ParserExpectToken(prs, KW_VAR))) {
        ParserErrorSet(prs, ERR_EXPECTED_KEYWORD, prs->cur, TOK_KW_VAR, prs->line, prs->col);
        return PARSE_ERROR;
    }

    ParserConsumeToken(prs);
    if (!ParserExpectToken(prs, IDENTIFIER)) {
        ParserErrorSet(prs, ERR_EXPECTED_IDENTIFIER, prs->cur, prs->line, prs->col);
        return PARSE_ERROR;
    }

    /* take the identifier from the current token so the buffer isn't destroyed */
    (*decl)->ident = prs->cur->value;
    prs->cur->value = NULL;
    ParserConsumeToken(prs);

    /* allow a declaration without initialization */
    if (!ParserExpectToken(prs, OP_EQ)) {
        /* allow multiple comma separated declarations */
        if (ParserExpectToken(prs, COMMA)) {
            return ParserParseDeclaration(prs, false, &(*decl)->next);
        }

        return PARSE_SUCCESS;
    }

    ParserConsumeToken(prs);
    if (ParserParseExpression(prs, &(*decl)->expr) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    /* allow multiple comma separated declarations */
    if (ParserExpectToken(prs, COMMA)) {
        return ParserParseDeclaration(prs, false, &(*decl)->next);
    }

    return PARSE_SUCCESS;
}

static ms_ParseResult ParserParseAssignment(ms_Parser *prs, ms_Stmt **stmt) {
    assert(prs);
    assert(stmt);

    ms_Expr *name;
    if (ParserParseExpression(prs, &name) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    /* some sort of assignment (simple or compound) */
    if (ParserExpectToken(prs, OP_EQ)) {
        if (!ms_ExprIsQualifiedIdent(name)) {
            ms_ExprDestroy(name);
            ParserErrorSet(prs, ERR_MUST_ASSIGN_TO_QIDENT, prs->cur, prs->line, prs->col);
            return PARSE_ERROR;
        }

        if (ParserParseSimpleAssignment(prs, name, stmt) == PARSE_ERROR) {
            ms_ExprDestroy(name);
            return PARSE_ERROR;
        }

        return PARSE_SUCCESS;
    } else if (ParserExpectToken(prs, OP_PLUS_EQUALS) ||
               ParserExpectToken(prs, OP_MINUS_EQUALS) ||
               ParserExpectToken(prs, OP_TIMES_EQUALS) ||
               ParserExpectToken(prs, OP_DIVIDE_EQUALS) ||
               ParserExpectToken(prs, OP_IDIVIDE_EQUALS) ||
               ParserExpectToken(prs, OP_MODULO_EQUALS)) {

        if (!ms_ExprIsQualifiedIdent(name)) {
            ms_ExprDestroy(name);
            ParserErrorSet(prs, ERR_MUST_ASSIGN_TO_QIDENT, prs->cur, prs->line, prs->col);
            return PARSE_ERROR;
        }

        if (ParserParseCompoundAssignment(prs, name, stmt) == PARSE_ERROR) {
            ms_ExprDestroy(name);
            return PARSE_ERROR;
        }

        return PARSE_SUCCESS;
    }

    /* ONLY an expression */
    (*stmt)->type = STMTTYPE_EXPRESSION;
    (*stmt)->cmpnt.expr = name;
    return PARSE_SUCCESS;
}

static ms_ParseResult ParserParseSimpleAssignment(ms_Parser *prs, ms_Expr *name, ms_Stmt **stmt) {
    assert(prs);
    assert(name);
    assert(stmt);

    ParserConsumeToken(prs);
    (*stmt)->cmpnt.assign = calloc(1, sizeof(ms_StmtAssignment));
    if (!((*stmt)->cmpnt.assign)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    (*stmt)->type = STMTTYPE_ASSIGNMENT;
    (*stmt)->cmpnt.assign->ident = name;
    return ParserParseExpression(prs, &(*stmt)->cmpnt.assign->expr);
}

static ms_ParseResult ParserParseCompoundAssignment(ms_Parser *prs, ms_Expr *name, ms_Stmt **stmt) {
    assert(prs);
    assert(name);
    assert(stmt);

    /* translate compound operator to binary op */
    ms_ExprBinaryOp op = BINARY_EMPTY;
    switch (prs->cur->type) {
        case OP_PLUS_EQUALS:        op = BINARY_PLUS;       break;
        case OP_MINUS_EQUALS:       op = BINARY_MINUS;      break;
        case OP_TIMES_EQUALS:       op = BINARY_TIMES;      break;
        case OP_DIVIDE_EQUALS:      op = BINARY_DIVIDE;     break;
        case OP_IDIVIDE_EQUALS:     op = BINARY_IDIVIDE;    break;
        case OP_MODULO_EQUALS:      op = BINARY_MODULO;     break;
        default:                    assert(false);          break;
    }

    ParserConsumeToken(prs);
    (*stmt)->cmpnt.assign = calloc(1, sizeof(ms_StmtAssignment));
    if (!((*stmt)->cmpnt.assign)) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    (*stmt)->type = STMTTYPE_ASSIGNMENT;
    (*stmt)->cmpnt.assign->ident = name;
    ms_Expr *right = NULL;

    /* parse the right piece of the expanded compound expression  */
    if (ParserParseExpression(prs, &right) == PARSE_ERROR) {
        return PARSE_ERROR;
    }

    /* duplicate the identifier expression being set to be used in
     * the left piece of the resulting compound expression */
    ms_Expr *left = ms_ExprDup(name);
    if (!left) {
        ms_ExprDestroy(right);
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    /* combine the two pieces of the compound expression */
    ms_Expr *combined;
    if (ParserExprCombineBinary(prs, left, op, right, &combined) == PARSE_ERROR) {
        ms_ExprDestroy(left);
        ms_ExprDestroy(right);
        return PARSE_ERROR;
    }

    (*stmt)->cmpnt.assign->expr = combined;
    return PARSE_SUCCESS;
}

static ms_ParseResult ParserParseExpression(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);
    return ParserParseOrExpr(prs, expr);
}

static ms_ParseResult ParserParseOrExpr(ms_Parser *prs, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    ms_ParseResult res;
    ms_Expr *left = NULL;
    if ((res = ParserParseAndExpr(prs, &left)) == PARSE_ERROR) {
        *expr = left;
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
        ms_Expr *right = NULL;
        if ((res = ParserParseAndExpr(prs, &right)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            ms_ExprDestroy(right);
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
    ms_Expr *left = NULL;
    if ((res = ParserParseEqualityExpr(prs, &left)) == PARSE_ERROR) {
        *expr = left;
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
        ms_Expr *right = NULL;
        if ((res = ParserParseEqualityExpr(prs, &right)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            ms_ExprDestroy(right);
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
    ms_Expr *left = NULL;
    if ((res = ParserParseComparisonExpr(prs, &left)) == PARSE_ERROR) {
        *expr = left;
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
        ms_Expr *right = NULL;
        if ((res = ParserParseComparisonExpr(prs, &right)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            ms_ExprDestroy(right);
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
    ms_Expr *left = NULL;
    if ((res = ParserParseBitwiseOrExpr(prs, &left)) == PARSE_ERROR) {
        *expr = left;
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
        ms_Expr *right = NULL;
        if ((res = ParserParseBitwiseOrExpr(prs, &right)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            ms_ExprDestroy(right);
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
    ms_Expr *left = NULL;
    if ((res = ParserParseBitwiseXorExpr(prs, &left)) == PARSE_ERROR) {
        *expr = left;
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
        ms_Expr *right = NULL;
        if ((res = ParserParseBitwiseXorExpr(prs, &right)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            ms_ExprDestroy(right);
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
        *expr = left;
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
    ms_Expr *left = NULL;
    if ((res = ParserParseBitShiftExpr(prs, &left)) == PARSE_ERROR) {
        *expr = left;
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
        ms_Expr *right = NULL;
        if ((res = ParserParseBitShiftExpr(prs, &right)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            ms_ExprDestroy(right);
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
    ms_Expr *left = NULL;
    if ((res = ParserParseArithmeticExpr(prs, &left)) == PARSE_ERROR) {
        *expr = left;
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
        ms_Expr *right = NULL;
        if ((res = ParserParseArithmeticExpr(prs, &right)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            ms_ExprDestroy(right);
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
    ms_Expr *left = NULL;
    if ((res = ParserParseTermExpr(prs, &left)) == PARSE_ERROR) {
        *expr = left;
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
        ms_Expr *right = NULL;
        if ((res = ParserParseTermExpr(prs, &right)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            ms_ExprDestroy(right);
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
    ms_Expr *left = NULL;
    if ((res = ParserParsePowerExpr(prs, &left)) == PARSE_ERROR) {
        *expr = left;
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
        ms_Expr *right = NULL;
        if ((res = ParserParsePowerExpr(prs, &right)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            ms_ExprDestroy(right);
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
    ms_Expr *left = NULL;
    if ((res = ParserParseUnaryExpr(prs, &left)) == PARSE_ERROR) {
        *expr = left;
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
        ms_Expr *right = NULL;
        if ((res = ParserParseTermExpr(prs, &right)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            ms_ExprDestroy(right);
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

        ms_Expr *inner = NULL;
        if ((res = ParserParseUnaryExpr(prs, &inner)) == PARSE_ERROR) {
            ms_ExprDestroy(inner);
            return res;
        }
        if ((res = ParserExprCombineUnary(prs, inner, op, expr)) == PARSE_ERROR) {
            ms_ExprDestroy(inner);
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
    ms_Expr *left = NULL;
    if ((res = ParserParseAtom(prs, &left)) == PARSE_ERROR) {
        *expr = left;
        return res;
    }

    while (ParserExpectToken(prs, LPAREN) ||
            ParserExpectToken(prs, LBRACKET) ||
            ParserExpectToken(prs, PERIOD)) {
        ms_ExprBinaryOp op;
        ms_Expr *right;
        if ((res = ParserParseAccessor(prs, &right, &op)) == PARSE_ERROR) {
            ms_ExprDestroy(left);
            return res;
        }

        ms_Expr *combined;
        if ((res = ParserExprCombineBinary(prs, left, op, right, &combined)) == PARSE_ERROR) {
            ms_ExprDestroy(right);
            ms_ExprDestroy(left);
            return res;
        }
        left = combined;
    }

    *expr = left;
    return res;
}

static ms_ParseResult ParserParseAccessor(ms_Parser *prs, ms_Expr **expr, ms_ExprBinaryOp *op) {
    assert(prs);
    assert(expr);

    if (prs->cur->type == LPAREN) {
        ParserConsumeToken(prs);
        *op = BINARY_CALL;
        return ParserParseExprList(prs, expr, RPAREN);
    } else if (prs->cur->type == LBRACKET) {
        ParserConsumeToken(prs);
        *op = BINARY_GETATTR;
        return ParserParseExprList(prs, expr, RBRACKET);
    } else if (prs->cur->type == PERIOD) {
        ParserConsumeToken(prs);

        if (!ParserExpectToken(prs, IDENTIFIER)) {
            ParserErrorSet(prs, ERR_EXPECTED_IDENTIFIER, prs->cur, prs->line, prs->col);
            return PARSE_ERROR;
        }

        *expr = ms_ExprNewWithIdent(dsbuf_char_ptr(prs->cur->value), dsbuf_len(prs->cur->value));
        if (!(*expr)) {
            ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
            return PARSE_ERROR;
        }

        ParserConsumeToken(prs);
        *op = BINARY_GETATTR;
        return PARSE_SUCCESS;
    }

    return PARSE_SUCCESS;
}

static ms_ParseResult ParserParseExprList(ms_Parser *prs, ms_Expr **list, ms_TokenType closer) {
    assert(prs);
    assert(list);

    DSArray *params = dsarray_new_cap(EXPRESSION_LIST_DEFAULT_CAP, NULL,
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
    if ((prs->cur) && (prs->cur->type == closer)) {
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
            if (prs->cur->type == closer) {
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
            ms_ValData p;
            p.s = cur->value;
            *expr = ms_ExprNewWithVal(MSVAL_STR, p);
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
            ms_ValData p;
            p.b = (cur->type == KW_TRUE) ? true : false;
            *expr = ms_ExprNewWithVal(MSVAL_BOOL, p);
            if (!(*expr)) {
                ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                res = PARSE_ERROR;
            }
            break;
        }

            /* null literal */
        case KW_NULL: {
            ms_ValData p;
            p.n = MS_VM_NULL_POINTER;
            *expr = ms_ExprNewWithVal(MSVAL_NULL, p);
            if (!(*expr)) {
                ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                res = PARSE_ERROR;
            }
            break;
        }

            /* reference an identifier (either builtin, global, or other identifier) */
        case IDENTIFIER:
        case BUILTIN_FUNC:
        case GLOBAL: {
            *expr = ms_ExprNewWithIdent(dsbuf_char_ptr(cur->value), dsbuf_len(cur->value));
            if (!(*expr)) {
                ParserErrorSet(prs, ERR_OUT_OF_MEMORY, cur);
                res = PARSE_ERROR;
            }
            break;
        }

            /* function literal */
        case KW_FUNC:
            return ParserParseFunctionExpression(prs, false, expr);

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

static ms_ParseResult ParserParseFunctionExpression(ms_Parser *prs, bool require_name, ms_Expr **expr) {
    assert(prs);
    assert(expr);

    /*
     * Function declarations for mscript of the form
     *
     *      func name(arg1, arg2) {
     *          return arg1 + arg2
     *      }
     *
     * are internally rewritten as the following
     *
     *      var name := func name(arg1, arg2) {
     *          return arg1 + arg2
     *      }
     *
     * when declared at the top level. This obviates the need for
     * programmers to declare functions using the cumbersome `var`
     * syntax for functions declared as statements.
     *
     * This does not preclude programmers from using anonymous
     * function declarations as expressions, however, as in
     *
     *      api.ProcessData(func (arg1, arg2) {
     *          return arg1 + arg2
     *      })
     *
     * Note that top-level function declarations require a name
     * following the `func` keyword, which is in contrast to
     * anonymous functions declared as expressions.
     */

    ms_ValFunc *fn = calloc(1, sizeof(ms_ValFunc));
    if (!fn) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    *expr = ms_ExprNewWithFunc(fn);
    if (!(*expr)) {
        ms_ValFuncDestroy(fn);
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    if (!ParserExpectToken(prs, KW_FUNC)) {
        ParserErrorSet(prs, ERR_EXPECTED_KEYWORD, prs->cur, TOK_KW_FUNC, prs->line, prs->col);
        return PARSE_ERROR;
    }
    ParserConsumeToken(prs);

    /* potentially optional name (see description above) */
    bool has_name = ParserExpectToken(prs, IDENTIFIER);
    if (!has_name && require_name) {
        ParserErrorSet(prs, ERR_EXPECTED_IDENTIFIER, prs->cur, prs->line, prs->col);
        return PARSE_ERROR;
    }

    /* steal the identifier for the declaration */
    if (has_name) {
        fn->ident = prs->cur->value;
        prs->cur->value = NULL;
        ParserConsumeToken(prs);
    }

    /* parse the argument name list */
    if (!ParserExpectToken(prs, LPAREN)) {
        ParserErrorSet(prs, ERR_EXPECTED_TOKEN, prs->cur, "(", prs->line, prs->col);
        return PARSE_ERROR;
    }
    ParserConsumeToken(prs);

    fn->args = dsarray_new_cap(ARGUMENT_LIST_DEFAULT_CAP, NULL,
                                  (dsarray_free_fn)dsbuf_destroy);
    if (!fn->args) {
        ParserErrorSet(prs, ERR_OUT_OF_MEMORY, prs->cur);
        return PARSE_ERROR;
    }

    while (!ParserExpectToken(prs, RPAREN)) {
        if (!ParserExpectToken(prs, IDENTIFIER)) {
            ParserErrorSet(prs, ERR_EXPECTED_IDENTIFIER, prs->cur, prs->line,
                           prs->col);
            return PARSE_ERROR;
        }

        ms_Ident *ident = prs->cur->value;
        prs->cur->value = NULL;
        dsarray_append(fn->args, ident);
        ParserConsumeToken(prs);

        if (ParserExpectToken(prs, COMMA)) {
            ParserConsumeToken(prs);
        } else {
            if (!ParserExpectToken(prs, RPAREN)) {
                ParserErrorSet(prs, ERR_EXPECTED_TOKEN, prs->cur, TOK_COMMA, prs->line, prs->col);
                return PARSE_ERROR;
            }
        }
    }

    if (!ParserExpectToken(prs, RPAREN)) {
        ParserErrorSet(prs, ERR_EXPECTED_TOKEN, prs->cur, TOK_RPAREN, prs->line, prs->col);
        return PARSE_ERROR;
    }

    ParserConsumeToken(prs);
    return ParserParseBlock(prs, &fn->block);
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
    (*newexpr)->cmpnt.b->op = op;

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
    (*newexpr)->cmpnt.u->op = op;

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
    assert(prs);
    ms_TokenDestroy(ParserAdvanceToken(prs));
}

// Consume newlines until there are none remaining
static inline void ParserConsumeNewlines(ms_Parser *prs) {
    assert(prs);
    while (ParserExpectToken(prs, NEWLINE_TOK)) {
        ParserConsumeToken(prs);
    }
}

// Check if the next token to see if it matches our expected next token.
static bool ParserExpectToken(ms_Parser *prs, ms_TokenType type) {
    assert(prs);

    if ((!prs->cur) || (prs->cur->type != type)) {
        return false;
    }

    return true;
}

// Check if the next token to see if it matches our expected next token or if
// there are NO tokens left.
static bool ParserExpectTokenIfNotEOF(ms_Parser *prs, ms_TokenType type) {
    assert(prs);

    if (!prs->cur) {
        return true;
    }

    if (prs->cur->type != type) {
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
