#include <cstdint>
#include <sstream>
#include "ast.h"
#include "token.h"

namespace ast {

std::string Program::TokenLiteral() const {
    if (!Statements.empty()) {
        return Statements[0]->TokenLiteral();
    } else {
        return "";
    }
}

void LetStatement::statementNode() { }

std::string LetStatement::TokenLiteral() const {
    return Token.Literal;
}

std::string LetStatement::String() const {
    std::string result = TokenLiteral() + " " + Name->String() + " = ";
    if (Value) {
        result += Value->String();
    }
    result += ";";
    return result;
}

void ReturnStatement::statementNode() { }

std::string ReturnStatement::TokenLiteral() const {
    return Token.Literal;
}

std::string ReturnStatement::String() const {
    std::string result = TokenLiteral() + " ";
    if (ReturnValue) {
        result += ReturnValue->String();
    }
    result += ";";
    return result;
}

void Identifier::expressionNode() { }

Identifier::Identifier() : Token(), Value("") { }

Identifier::Identifier(const token::Token& tok, const std::string& val)
    : Token(tok), Value(val) { }

std::string Identifier::TokenLiteral() const {
    return Token.Literal;
}

std::string Identifier::String() const {
    return Value;
}

std::string Program::String() const {
    std::string result;
    for (const auto& stmt : Statements) {
        result += stmt->String();
    }
    return result;
}

void ExpressionStatement::statementNode() { }

std::string ExpressionStatement::TokenLiteral() const {
    return Token.Literal;
}

std::string ExpressionStatement::String() const {
    if (Expression) {
        return Expression->String();
    }
    return "";
}

IntegerLiteral::IntegerLiteral(const token::Token& tok)
    : Token(tok) { }

IntegerLiteral::IntegerLiteral(const token::Token& tok, const int64_t& val)
    : Token(tok), Value(val) { }

void IntegerLiteral::expressionNode() { }

std::string IntegerLiteral::TokenLiteral() const {
    return Token.Literal;
}

std::string IntegerLiteral::String() const {
    return Token.Literal;
}

PrefixExpression::PrefixExpression(const token::Token& tok, const std::string& op)
    : Token(tok), Operator(op) { }

void PrefixExpression::expressionNode() { }

std::string PrefixExpression::TokenLiteral() const {
    return Token.Literal;
}

std::string PrefixExpression::String() const {
    std::ostringstream out;
    out << "(" << Operator;
    if (Right) {
        out << Right->String();
    }
    out << ")";
    return out.str();
}

InfixExpression::InfixExpression(const token::Token& tok, std::unique_ptr<Expression> left, const std::string& op, std::unique_ptr<Expression> right)
    : Token(tok), Left(std::move(left)), Operator(op), Right(std::move(right)) { }

void InfixExpression::expressionNode() { }

std::string InfixExpression::TokenLiteral() const {
    return Token.Literal;
}

std::string InfixExpression::String() const {
    std::ostringstream out;
    out << "(" << Left->String() << " " << Operator << " " << Right->String() << ")";
    return out.str();
}

void Boolean::expressionNode() { }

std::string Boolean::TokenLiteral() const {
    return Token.Literal;
}

std::string Boolean::String() const {
    return Token.Literal;
}

void BlockStatement::statementNode() {}

std::string BlockStatement::TokenLiteral() const {
    return Token.Literal;
}

std::string BlockStatement::String() const {
    std::ostringstream out;
    for (const auto& statement : Statements) {
        out << statement->String();
    }
    return out.str();
}

IfExpression::IfExpression(const token::Token& token,
                           std::unique_ptr<Expression> condition,
                           std::unique_ptr<BlockStatement> consequence,
                           std::unique_ptr<BlockStatement> alternative)
    : Token(token),
      Condition(std::move(condition)),
      Consequence(std::move(consequence)),
      Alternative(std::move(alternative)) {}

void IfExpression::expressionNode() {}

std::string IfExpression::TokenLiteral() const {
    return Token.Literal;
}

std::string IfExpression::String() const {
    std::ostringstream out;
    out << "if" << Condition->String() << " " << Consequence->String();
    if (Alternative) {
        out << "else " << Alternative->String();
    }
    return out.str();
}

}
