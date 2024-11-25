#include <gtest/gtest.h>
#include "ast.h"
#include "token.h"

TEST(ASTTest, TestString) {
    token::Token letToken = {token::LET, "let"};
    token::Token identToken1 = {token::IDENT, "myVar"};
    token::Token identToken2 = {token::IDENT, "anotherVar"};

    auto name = std::make_unique<ast::Identifier>(identToken1, "myVar");
    auto value = std::make_unique<ast::Identifier>(identToken2, "anotherVar");

    auto letStmt = std::make_unique<ast::LetStatement>();
    letStmt->Token = letToken;
    letStmt->Name = std::move(name);
    letStmt->Value = std::move(value);

    ast::Program program;
    program.Statements.push_back(std::move(letStmt));

    EXPECT_EQ(program.String(), "let myVar = anotherVar;");
}
