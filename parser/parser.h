#ifndef PARSER_H
#define PARSER_H

#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
#include "ast.h"
#include "lexer.h"
#include "token.h"

namespace parser {

enum Precedence {
    LOWEST = 1,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL         // myFunction(X)
};

using prefixParseFn = std::function<std::unique_ptr<ast::Expression>()>;
using infixParseFn = std::function<std::unique_ptr<ast::Expression>(std::unique_ptr<ast::Expression>)>;

class Parser {
public:
    explicit Parser(std::unique_ptr<lexer::Lexer> lexer);
    void nextToken();
    std::unique_ptr<ast::Program> ParseProgram();
    const std::vector<std::string>& Errors() const { return errors; }
    int peekPrecedence() const;
    int curPrecedence() const;

private:
    std::unique_ptr<lexer::Lexer> l;
    token::Token curToken;
    token::Token peekToken;
    std::vector<std::string> errors;
    static const std::unordered_map<token::TokenType, Precedence> precedences;

    std::unordered_map<token::TokenType, prefixParseFn> prefixParseFns;
    std::unordered_map<token::TokenType, infixParseFn> infixParseFns;

    void registerPrefix(const token::TokenType& tokenType, prefixParseFn fn);
    void registerInfix(const token::TokenType& tokenType, infixParseFn fn);
    void noPrefixParseFnError(token::TokenType type);
    std::unique_ptr<ast::Statement> parseStatement();
    std::unique_ptr<ast::LetStatement> parseLetStatement();
    std::unique_ptr<ast::Statement> parseReturnStatement();
    std::unique_ptr<ast::Statement> parseExpressionStatement();
    std::unique_ptr<ast::Expression> parseExpression(int precedence);
    std::unique_ptr<ast::Expression> parseIdentifier();
    std::unique_ptr<ast::Expression> parseIntegerLiteral();
    std::unique_ptr<ast::Expression> parsePrefixExpression();
    std::unique_ptr<ast::Expression> parseBoolean();
    std::unique_ptr<ast::Expression> parseInfixExpression(std::unique_ptr<ast::Expression> left);
    std::unique_ptr<ast::Expression> parseGroupedExpression();
    std::unique_ptr<ast::BlockStatement> parseBlockStatement();
    std::unique_ptr<ast::Expression> parseIfExpression();

    bool curTokenIs(token::TokenType type);
    bool peekTokenIs(token::TokenType type);
    bool expectPeek(token::TokenType type);
    void peekError(token::TokenType type);
};

std::unique_ptr<Parser> New(std::unique_ptr<lexer::Lexer> lexer);

}

#endif
