#include <memory>
#include <vector>
#include "parser.h"
#include "ast.h"
#include "token.h"

namespace parser {

enum {
    LOWEST = 1,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL         // myFunction(X)
};

Parser::Parser(std::unique_ptr<lexer::Lexer> lexer) : l(std::move(lexer)) {
    // Initialize the map of prefix parse functions
    registerPrefix(token::IDENT, [this]() { return parseIdentifier(); });
    // Read two tokens, so curToken and peekToken are both set
    nextToken();
    nextToken();
}

void Parser::nextToken() {
    curToken = peekToken;
    peekToken = l->nextToken();
}

std::unique_ptr<ast::Program> Parser::ParseProgram() {
    auto program = std::make_unique<ast::Program>();

    while (!curTokenIs(token::EOF_)) {
        auto stmt = parseStatement();
        if (stmt) {
            program->Statements.push_back(std::move(stmt));
        }
        nextToken();
    }

    return program;
}

std::unique_ptr<ast::Statement> Parser::parseStatement() {
    if (curTokenIs(token::LET)) {
        return parseLetStatement();
    } else if (curTokenIs(token::RETURN)) {
        return parseReturnStatement();
    } else {
        return nullptr;
    }
}

std::unique_ptr<ast::LetStatement> Parser::parseLetStatement() {
    auto stmt = std::make_unique<ast::LetStatement>();
    stmt->Token = curToken;

    if (!expectPeek(token::IDENT)) {
        return nullptr;
    }

    stmt->Name = std::make_unique<ast::Identifier>(curToken, curToken.Literal);

    if (!expectPeek(token::ASSIGN)) {
        return nullptr;
    }

    while (!curTokenIs(token::SEMICOLON)) {
        nextToken();
    }

    return stmt;
}

std::unique_ptr<ast::Statement> Parser::parseReturnStatement() {
    auto stmt = std::make_unique<ast::ReturnStatement>();
    stmt->Token = curToken;

    // TODO Skipping expression parsing for now. Implement it later.
    while (!curTokenIs(token::SEMICOLON)) {
        nextToken();
    }

    return stmt;
}

std::unique_ptr<ast::Statement> Parser::parseExpressionStatement() {
    auto stmt = std::make_unique<ast::ExpressionStatement>();
    stmt->Token = curToken;
    stmt->Expression = parseExpression(LOWEST);

    if (peekTokenIs(token::SEMICOLON)) {
        nextToken();
    }
    return stmt;
}

std::unique_ptr<ast::Expression> Parser::parseExpression(int precedence) {
    auto prefix = prefixParseFns[curToken.Type];
    if (!prefix) {
        return nullptr;
    }

    auto leftExp = prefix();

    return leftExp;
}

std::unique_ptr<ast::Expression> Parser::parseIdentifier() {
    auto identifier = std::make_unique<ast::Identifier>();
    identifier->Token = curToken;
    identifier->Value = curToken.Literal;
    return identifier;
}

void Parser::registerPrefix(token::TokenType tokenType, prefixParseFn fn) {
    prefixParseFns[tokenType] = fn;
}

bool Parser::curTokenIs(token::TokenType type) {
    return curToken.Type == type;
}

bool Parser::peekTokenIs(token::TokenType type) {
    return peekToken.Type == type;
}

bool Parser::expectPeek(token::TokenType type) {
    if (peekTokenIs(type)) {
        nextToken();
        return true;
    } else {
        peekError(type);
        return false;
    }
}

void Parser::peekError(token::TokenType type) {
    std::string message = "Expected next token to be " + type + ", got " + peekToken.Type + " instead";
    errors.push_back(message);
}

std::unique_ptr<Parser> New(std::unique_ptr<lexer::Lexer> lexer) {
    return std::make_unique<Parser>(std::move(lexer));
}

}
