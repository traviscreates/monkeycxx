#include "ast.h"

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

void ReturnStatement::statementNode() { }

std::string ReturnStatement::TokenLiteral() const {
    return Token.Literal;
}

void Identifier::expressionNode() { }

Identifier::Identifier() : Token(), Value("") { }

Identifier::Identifier(const token::Token& tok, const std::string& val)
    : Token(tok), Value(val) { }

std::string Identifier::TokenLiteral() const {
    return Token.Literal;
}

std::string Program::String() const {
    std::string result;
    for (const auto& stmt : Statements) {
        result += stmt->TokenLiteral() + " ";
        auto letStmt = dynamic_cast<const LetStatement*>(stmt.get());
        if (letStmt) {
            result += letStmt->Name->TokenLiteral() + " = " + letStmt->Value->TokenLiteral() + ";";
        }
    }
    return result;
}

void ExpressionStatement::statementNode() { }

}
