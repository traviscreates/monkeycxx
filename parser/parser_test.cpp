#include <gtest/gtest.h>
#include <memory>
#include "ast.h"
#include "lexer.h"
#include "parser.h"

void checkParserErrors(const parser::Parser& p) {
    const auto& errors = p.Errors();
    if (errors.empty()) {
        return;
    }
    ADD_FAILURE() << "Parser has " << errors.size() << " errors.";
    for (const auto& msg : errors) {
        ADD_FAILURE() << "Parser error: " << msg;
    }
}

bool testLetStatement(ast::Statement* s, const std::string& name) {
    if (s->TokenLiteral() != "let") {
        return false;
    }

    auto letStmt = dynamic_cast<ast::LetStatement*>(s);
    if (!letStmt) {
        return false;
    }

    if (letStmt->Name->Value != name) {
        return false;
    }

    if (letStmt->Name->TokenLiteral() != name) {
        return false;
    }

    return true;
}

bool testReturnStatement(ast::Statement* s) {
    if (s->TokenLiteral() != "return") {
        return false;
    }

    auto letStmt = dynamic_cast<ast::ReturnStatement*>(s);
    if (!letStmt) {
        return false;
    }

    return true;
}

TEST(ParserTests, TestLetStatements) {
    std::string input = R"(
let x = 5;
let y = 10;
let foobar = 838383;
)";

    auto l = std::make_unique<lexer::Lexer>(input);
    auto p = parser::New(std::move(l));
    std::unique_ptr<ast::Program> program = p->ParseProgram();
    checkParserErrors(*p);
    ASSERT_NE(program, nullptr);
    ASSERT_EQ(program->Statements.size(), 3);

    struct Test {
        std::string expectedIdentifier;
    };

    std::vector<Test> tests = { {"x"}, {"y"}, {"foobar"} };

    for (size_t i = 0; i < tests.size(); ++i) {
        auto stmt = program->Statements[i].get();
        EXPECT_TRUE(testLetStatement(stmt, tests[i].expectedIdentifier));
    }
}

TEST(ParserTests, TestReturnStatements) {
    std::string input = R"(
return 5;
return 10;
return 993322;
)";

    auto l = std::make_unique<lexer::Lexer>(input);
    auto p = parser::New(std::move(l));
    std::unique_ptr<ast::Program> program = p->ParseProgram();
    checkParserErrors(*p);
    ASSERT_NE(program, nullptr);
    ASSERT_EQ(program->Statements.size(), 3);

    struct Test {
        std::string expectedIdentifier;
    };

    std::vector<Test> tests = { {"5"}, {"10"}, {"993322"} };

    for (size_t i = 0; i < tests.size(); ++i) {
        auto stmt = program->Statements[i].get();
        EXPECT_TRUE(testReturnStatement(stmt));
    }
}

