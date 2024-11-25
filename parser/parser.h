#ifndef PARSER_H
#define PARSER_H

#include <functional>
#include <memory>
#include <unordered_map>
#include "ast.h"
#include "lexer.h"
#include "token.h"

namespace parser {

using prefixParseFn = std::function<std::unique_ptr<ast::Expression>()>;

class Parser {
public:
    explicit Parser(std::unique_ptr<lexer::Lexer> lexer);
    void nextToken();
    std::unique_ptr<ast::Program> ParseProgram();
    const std::vector<std::string>& Errors() const { return errors; }

private:
    std::unique_ptr<lexer::Lexer> l;
    token::Token curToken;
    token::Token peekToken;
    std::vector<std::string> errors;

    std::unordered_map<token::TokenType, prefixParseFn> prefixParseFns;

    void registerPrefix(token::TokenType tokenType, prefixParseFn fn);
    std::unique_ptr<ast::Statement> parseStatement();
    std::unique_ptr<ast::LetStatement> parseLetStatement();
    std::unique_ptr<ast::Statement> parseReturnStatement();
    std::unique_ptr<ast::Statement> parseExpressionStatement();
    std::unique_ptr<ast::Expression> parseExpression(int precedence);
    std::unique_ptr<ast::Expression> parseIdentifier();

    bool curTokenIs(token::TokenType type);
    bool peekTokenIs(token::TokenType type);
    bool expectPeek(token::TokenType type);
    void peekError(token::TokenType type);
};

std::unique_ptr<Parser> New(std::unique_ptr<lexer::Lexer> lexer);

}

#endif
