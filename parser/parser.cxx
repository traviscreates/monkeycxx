#include <algorithm>
#include <iterator>
#include <memory>
#include <unordered_map>
#include <vector>
#include "parser.h"
#include "ast.h"
#include "token.h"

namespace parser {

const std::unordered_map<token::TokenType, Precedence> Parser::precedences = {
    {token::EQ, EQUALS},
    {token::NOT_EQ, EQUALS},
    {token::LT, LESSGREATER},
    {token::GT, LESSGREATER},
    {token::PLUS, SUM},
    {token::MINUS, SUM},
    {token::SLASH, PRODUCT},
    {token::ASTERISK, PRODUCT},
    {token::LPAREN, CALL}
};

Parser::Parser(std::unique_ptr<lexer::Lexer> lexer) : l(std::move(lexer)) {
    registerPrefix(token::IDENT, [this]() { return parseIdentifier(); });
    registerPrefix(token::INT, [this]() { return parseIntegerLiteral(); });
    registerPrefix(token::BANG, [this]() { return parsePrefixExpression(); });
    registerPrefix(token::MINUS, [this]() { return parsePrefixExpression(); });
    registerPrefix(token::FALSE, [this]() { return parseBoolean(); });
    registerPrefix(token::TRUE, [this]() { return parseBoolean(); });
    registerPrefix(token::LPAREN, [this]() { return parseGroupedExpression(); });
    registerPrefix(token::IF, [this]() { return parseIfExpression(); });
    registerPrefix(token::FUNCTION, [this]() { return parseFunctionLiteral(); });

    registerInfix(token::PLUS, [this](std::unique_ptr<ast::Expression> left) {
        return parseInfixExpression(std::move(left));
    });
    registerInfix(token::MINUS, [this](std::unique_ptr<ast::Expression> left) {
        return parseInfixExpression(std::move(left));
    });
    registerInfix(token::SLASH, [this](std::unique_ptr<ast::Expression> left) {
        return parseInfixExpression(std::move(left));
    });
    registerInfix(token::ASTERISK, [this](std::unique_ptr<ast::Expression> left) {
        return parseInfixExpression(std::move(left));
    });
    registerInfix(token::EQ, [this](std::unique_ptr<ast::Expression> left) {
        return parseInfixExpression(std::move(left));
    });
    registerInfix(token::NOT_EQ, [this](std::unique_ptr<ast::Expression> left) {
        return parseInfixExpression(std::move(left));
    });
    registerInfix(token::LT, [this](std::unique_ptr<ast::Expression> left) {
        return parseInfixExpression(std::move(left));
    });
    registerInfix(token::GT, [this](std::unique_ptr<ast::Expression> left) {
        return parseInfixExpression(std::move(left));
    });
    registerInfix(token::LPAREN, [this](std::unique_ptr<ast::Expression> function) {
        return parseCallExpression(std::move(function));
    });

    // Read two tokens, so curToken and peekToken are both set
    nextToken();
    nextToken();
}

std::vector<std::unique_ptr<ast::Expression>> Parser::parseCallArguments() {
    std::vector<std::unique_ptr<ast::Expression>> args;

    if (peekTokenIs(token::RPAREN)) {
        nextToken(); // Consume ')'
        return args;
    }

    nextToken(); // Move to the first argument
    args.push_back(parseExpression(LOWEST));

    while (peekTokenIs(token::COMMA)) {
        nextToken(); // Consume ','
        nextToken(); // Move to the next argument
        args.push_back(parseExpression(LOWEST));
    }

    if (!expectPeek(token::RPAREN)) {
        return {};
    }

    return args;
}

std::unique_ptr<ast::Expression> Parser::parseCallExpression(std::unique_ptr<ast::Expression> function) {
    auto callExpression = std::make_unique<ast::CallExpression>();
    callExpression->Token = curToken;
    callExpression->Function = std::move(function);
    callExpression->Arguments = parseCallArguments();
    return callExpression;
}

void Parser::nextToken() {
    curToken = peekToken;
    peekToken = l->nextToken();
}

void Parser::registerPrefix(const token::TokenType& tokenType, prefixParseFn fn) {
    prefixParseFns[tokenType] = fn;
}

void Parser::registerInfix(const token::TokenType& tokenType, infixParseFn fn) {
    infixParseFns[tokenType] = fn;
}

void Parser::noPrefixParseFnError(token::TokenType type) {
    std::string msg = "no prefix parse function for " + type + " found";
    errors.push_back(msg);
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
        return parseExpressionStatement();
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

    nextToken();

    stmt->Value = parseExpression(LOWEST);
    if (peekTokenIs(token::SEMICOLON)) {
        nextToken();
    }

    return stmt;
}

std::unique_ptr<ast::Statement> Parser::parseReturnStatement() {
    auto stmt = std::make_unique<ast::ReturnStatement>();
    stmt->Token = curToken;

    nextToken();

    stmt->ReturnValue = parseExpression(LOWEST);
    if (peekTokenIs(token::SEMICOLON)) {
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
        noPrefixParseFnError(curToken.Type);
        return nullptr;
    }

    auto leftExp = prefix();
    while (!peekTokenIs(token::SEMICOLON) && precedence < peekPrecedence()) {
        auto infix = infixParseFns[peekToken.Type];
        if (!infix) {
            return leftExp;
        }
        nextToken();
        leftExp = infix(std::move(leftExp));
    }

    return leftExp;
}

std::unique_ptr<ast::Expression> Parser::parseIdentifier() {
    auto identifier = std::make_unique<ast::Identifier>();
    identifier->Token = curToken;
    identifier->Value = curToken.Literal;
    return identifier;
}

std::unique_ptr<ast::Expression> Parser::parseIntegerLiteral() {
    auto intLitPtr = std::make_unique<ast::IntegerLiteral>(curToken);

    try {
        intLitPtr->Value = std::stoll(curToken.Literal);
    } catch (const std::invalid_argument& e) {
        std::string msg = "could not parse \"" + curToken.Literal + "\" as integer";
        errors.push_back(msg);
        return nullptr;
    } catch (const std::out_of_range& e) {
        std::string msg = "integer value out of range for \"" + curToken.Literal + "\"";
        errors.push_back(msg);
        return nullptr;
    }

    return intLitPtr;
}

std::unique_ptr<ast::Expression> Parser::parsePrefixExpression() {
    auto prefixExpressionPtr = std::make_unique<ast::PrefixExpression>(
        curToken, curToken.Literal
    );
    nextToken();
    prefixExpressionPtr->Right = parseExpression(PREFIX);
    return prefixExpressionPtr;
}

std::unique_ptr<ast::Expression> Parser::parseBoolean() {
    auto boolean = std::make_unique<ast::Boolean>();
    boolean->Token = curToken;
    boolean->Value = curTokenIs(token::TRUE);
    return boolean;
}

std::unique_ptr<ast::Expression> Parser::parseInfixExpression(std::unique_ptr<ast::Expression> left) {
    auto infixExpressionPtr = std::make_unique<ast::InfixExpression>(
        curToken, std::move(left), curToken.Literal, nullptr
    );

    int precedence = curPrecedence();
    nextToken();
    infixExpressionPtr->Right = parseExpression(precedence);

    return infixExpressionPtr;
}

std::unique_ptr<ast::Expression> Parser::parseGroupedExpression() {
    nextToken();

    auto exp = parseExpression(LOWEST);
    if (!expectPeek(token::RPAREN)) {
        return nullptr;
    }

    return exp;
}

std::unique_ptr<ast::BlockStatement> Parser::parseBlockStatement() {
    auto block = std::make_unique<ast::BlockStatement>();
    block->Token = curToken;

    nextToken();

    while (!curTokenIs(token::RBRACE) && !curTokenIs(token::EOF_)) {
        auto stmt = parseStatement();
        if (stmt) {
            block->Statements.push_back(std::move(stmt));
        }
        nextToken();
    }

    return block;
}

std::unique_ptr<ast::Expression> Parser::parseIfExpression() {
    auto exp = std::make_unique<ast::IfExpression>();
    exp->Token = curToken;
    if (!expectPeek(token::LPAREN)) {
        return nullptr;
    }

    nextToken();

    exp->Condition = parseExpression(Precedence::LOWEST);
    if (!exp->Condition) {
        return nullptr;
    }

    if (!expectPeek(token::RPAREN)) {
        return nullptr;
    }

    if (!expectPeek(token::LBRACE)) {
        return nullptr;
    }

    exp->Consequence = parseBlockStatement();
    if (!exp->Consequence) {
        return nullptr;
    }

    if (peekTokenIs(token::ELSE)) {
        nextToken();
        if (!expectPeek(token::LBRACE)) {
            return nullptr;
        }
        exp->Alternative = parseBlockStatement();
    }

    return exp;
}

std::vector<std::unique_ptr<ast::Identifier>> Parser::parseFunctionParameters() {
    std::vector<std::unique_ptr<ast::Identifier>> identifiers;

    if (peekTokenIs(token::RPAREN)) {
        nextToken();
        return identifiers;
    }

    nextToken();

    auto ident = std::make_unique<ast::Identifier>();
    ident->Token = curToken;
    ident->Value = curToken.Literal;
    identifiers.push_back(std::move(ident));

    while (peekTokenIs(token::COMMA)) {
        nextToken();
        nextToken();
        auto ident = std::make_unique<ast::Identifier>();
        ident->Token = curToken;
        ident->Value = curToken.Literal;
        identifiers.push_back(std::move(ident));
    }

    if (!expectPeek(token::RPAREN)) {
        return {};
    }

    return identifiers;
}

std::unique_ptr<ast::FunctionLiteral> Parser::parseFunctionLiteral() {
    auto funcLitPtr = std::make_unique<ast::FunctionLiteral>();
    funcLitPtr->Token = curToken;
    if (!expectPeek(token::LPAREN)) {
        return nullptr;
    }

    funcLitPtr->Parameters = parseFunctionParameters();
    if (!expectPeek(token::LBRACE)) {
        return nullptr;
    }

    funcLitPtr->Body = parseBlockStatement();

    return funcLitPtr;
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

int Parser::peekPrecedence() const {
    auto it = precedences.find(peekToken.Type);
    if (it != precedences.end()) {
        return it->second;
    }
    return LOWEST;
}

int Parser::curPrecedence() const {
    auto it = precedences.find(curToken.Type);
    if (it != precedences.end()) {
        return it->second;
    }
    return LOWEST;
}

}
