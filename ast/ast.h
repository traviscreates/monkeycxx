#ifndef AST_H
#define AST_H

#include <ios>
#include <memory>
#include <string>
#include <vector>
#include "token.h"

namespace ast {

class Node {
public:
    virtual std::string TokenLiteral() const = 0;
    virtual std::string String() const = 0;
    virtual ~Node() = default;
};

class Statement : public Node {
public:
    virtual void statementNode() = 0;
};

class Expression : public Node {
public:
    virtual void expressionNode() = 0;
    virtual ~Expression() = default;

};

class Program : public Node {
public:
    std::vector<std::unique_ptr<Statement>> Statements;
    std::string TokenLiteral() const override;
    std::string String() const override;
};

class LetStatement : public Statement {
public:
    token::Token Token;
    std::unique_ptr<class Identifier> Name;
    std::unique_ptr<Expression> Value;

    void statementNode() override;
    std::string TokenLiteral() const override;
    std::string String() const override;
};

class ReturnStatement : public Statement {
public:
    token::Token Token;
    std::unique_ptr<Expression> ReturnValue;

    void statementNode() override;
    std::string TokenLiteral() const override;
    std::string String() const override;
};

class ExpressionStatement : public Statement {
public:
    token::Token Token;
    std::unique_ptr<Expression> Expression;

    void statementNode() override;
    std::string TokenLiteral() const override;
    std::string String() const override;
};

class Identifier : public Expression {
public:
    token::Token Token;
    std::string Value;

    Identifier();
    Identifier(const token::Token& tok, const std::string& val);
    void expressionNode() override;
    std::string TokenLiteral() const override;
    std::string String() const override;
};

class IntegerLiteral : public Expression {
public:
    token::Token Token;
    int64_t Value;

    IntegerLiteral(const token::Token& tok);
    IntegerLiteral(const token::Token& tok, const int64_t& val);
    void expressionNode() override;
    std::string TokenLiteral() const override;
    std::string String() const override;
};

class PrefixExpression : public Expression {
public:
    token::Token Token;
    std::string Operator;
    std::unique_ptr<Expression> Right;

    PrefixExpression(const token::Token& tok, const std::string& op);
    void expressionNode() override;
    std::string TokenLiteral() const override;
    std::string String() const override;
};

class InfixExpression : public Expression {
public:
    token::Token Token;
    std::unique_ptr<Expression> Left;
    std::string Operator;
    std::unique_ptr<Expression> Right;

    InfixExpression(
        const token::Token& tok,
        std::unique_ptr<Expression> left,
        const std::string& op,
        std::unique_ptr<Expression> right
    );

    void expressionNode() override;
    std::string TokenLiteral() const override;
    std::string String() const override;
};

class Boolean : public Expression {
public:
    token::Token Token;
    bool Value;

    void expressionNode() override;
    std::string TokenLiteral() const override;
    std::string String() const override;
};

class BlockStatement : public Statement {
public:
    token::Token Token;  // The '{' token
    std::vector<std::unique_ptr<Statement>> Statements;

    BlockStatement() = default;

    void statementNode() override;
    std::string TokenLiteral() const override;
    std::string String() const override;
};

class IfExpression : public Expression {
public:
    token::Token Token;  // The 'if' token
    std::unique_ptr<Expression> Condition;
    std::unique_ptr<BlockStatement> Consequence;
    std::unique_ptr<BlockStatement> Alternative;

    IfExpression() = default;
    IfExpression(
        const token::Token& token,
        std::unique_ptr<Expression> condition,
        std::unique_ptr<BlockStatement> consequence,
        std::unique_ptr<BlockStatement> alternative
    );

    void expressionNode() override;
    std::string TokenLiteral() const override;
    std::string String() const override;
};

}

#endif


