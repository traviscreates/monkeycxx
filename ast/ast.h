#ifndef AST_H
#define AST_H

#include <memory>
#include <string>
#include <vector>
#include "token.h"

namespace ast {

class Node {
public:
    virtual std::string TokenLiteral() const = 0;
    virtual ~Node() = default;
};

class Statement : public Node {
public:
    virtual void statementNode() = 0;
};

class Expression : public Node {
public:
    virtual void expressionNode() = 0;
};

class Program : public Node {
public:
    std::vector<std::unique_ptr<Statement>> Statements;
    std::string TokenLiteral() const override;
    std::string String() const;
};

class LetStatement : public Statement {
public:
    token::Token Token;
    std::unique_ptr<class Identifier> Name;
    std::unique_ptr<Expression> Value;

    void statementNode() override;
    std::string TokenLiteral() const override;
};

class ReturnStatement : public Statement {
public:
    token::Token Token;
    std::unique_ptr<Expression> ReturnValue;

    void statementNode() override;
    std::string TokenLiteral() const override;
};

class Identifier : public Expression {
public:
    token::Token Token;
    std::string Value;

    Identifier();
    Identifier(const token::Token& tok, const std::string& val);
    void expressionNode() override;
    std::string TokenLiteral() const override;
};

class ExpressionStatement : public Statement {
public:
    token::Token Token;
    std::unique_ptr<Expression> Expression;

    void statementNode() override;
    std::string TokenLiteral() const override {
        return Token.Literal;
    }
};

}

#endif
