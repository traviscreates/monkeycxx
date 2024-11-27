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

bool testBoolean(const ast::Expression* expr, bool value) {
    const auto* boolean = dynamic_cast<const ast::Boolean*>(expr);
    if (boolean->Value != value) {
        ADD_FAILURE() << "Boolean value is not " << value << ". Got: " << boolean->Value;
        return false;
    }
    return true;
}

bool testIntegerLiteral(const ast::Expression* expr, int64_t value) {
    const auto* integerLiteral = dynamic_cast<const ast::IntegerLiteral*>(expr);
    if (!integerLiteral) {
        ADD_FAILURE() << "Expression is not ast::IntegerLiteral. Got: " << typeid(*expr).name();
        return false;
    }
    if (integerLiteral->Value != value) {
        ADD_FAILURE() << "IntegerLiteral value is not " << value << ". Got: " << integerLiteral->Value;
        return false;
    }
    return true;
}

bool testIdentifier(const ast::Expression* expr, std::string value) {
    const auto* identifier = dynamic_cast<const ast::Identifier*>(expr);
    if (!identifier) {
        ADD_FAILURE() << "Expression is not ast::Identifier. Got: " << typeid(*expr).name();
        return false;
    }
    if (identifier->Value != value) {
        ADD_FAILURE() << "Identifier value is not " << value << ". Got: " << identifier->Value;
        return false;
    }
    if (identifier->TokenLiteral() != value) {
        ADD_FAILURE() << "Identifier TokenLiteral is not " << value << ". Got: " << identifier->TokenLiteral();
        return false;
    }
    return true;
}

using LiteralExpectedType = std::variant<int64_t, std::string>;

bool testLiteralExpression(const ast::Expression& exp, const LiteralExpectedType& expected) {
    return std::visit([&exp](auto&& arg) -> bool {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, bool>) {
            return testBoolean(&exp, arg);
        } else if constexpr (std::is_same_v<T, int64_t>) {
            return testIntegerLiteral(&exp, arg);
        } else if constexpr (std::is_same_v<T, std::string>) {
            return testIdentifier(&exp, arg);
        } else {
            std::cerr << "Type of exp not handled." << std::endl;
            return false;
        }
    }, expected);
}

// TODO: refactor tests
bool testInfixExpression(
    const ast::Expression* expr,
    const std::variant<bool, int64_t, std::string>& left,
    const std::string& operator_,
    const std::variant<bool, int64_t, std::string>& right
) {
    const auto* opExp = dynamic_cast<const ast::InfixExpression*>(expr);
    if (!opExp) {
        ADD_FAILURE() << "Expression is not ast::InfixExpression. Got: "
                      << typeid(*expr).name();
        return false;
    }

    auto testLeft = [&](const auto& val) -> bool {
        return testLiteralExpression(*opExp->Left.get(), val);
    };
    if (!std::visit(testLeft, left)) {
        return false;
    }

    if (opExp->Operator != operator_) {
        ADD_FAILURE() << "Operator is not '" << operator_ << "'. Got: "
                      << opExp->Operator;
        return false;
    }

    auto testRight = [&](const auto& val) -> bool {
        return testLiteralExpression(*opExp->Right.get(), val);
    };
    if (!std::visit(testRight, right)) {
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

TEST(ParserTests, TestIdentifierExpression) {
    std::string input = "foobar;";
    auto lexer = std::make_unique<lexer::Lexer>(input);
    auto parser = parser::New(std::move(lexer));

    auto program = parser->ParseProgram();
    checkParserErrors(*parser);
    ASSERT_EQ(program->Statements.size(), 1) << "Program has incorrect number of statements.";

    auto stmt = dynamic_cast<ast::ExpressionStatement*>(program->Statements[0].get());
    ASSERT_NE(stmt, nullptr) << "First statement is not an ExpressionStatement.";

    auto ident = dynamic_cast<ast::Identifier*>(stmt->Expression.get());
    ASSERT_NE(ident, nullptr) << "Expression is not an Identifier.";
    EXPECT_EQ(ident->Value, "foobar") << "Identifier value is incorrect.";
    EXPECT_EQ(ident->TokenLiteral(), "foobar") << "Identifier TokenLiteral is incorrect.";
}

TEST(ParserTests, TestIntegerLiteralExpression) {
    std::string input = "5;";
    auto lexer = std::make_unique<lexer::Lexer>(input);
    auto parser = parser::New(std::move(lexer));

    auto program = parser->ParseProgram();
    checkParserErrors(*parser);
    ASSERT_EQ(program->Statements.size(), 1) << "Program has incorrect number of statements.";

    auto stmt = dynamic_cast<ast::ExpressionStatement*>(program->Statements[0].get());
    ASSERT_NE(stmt, nullptr) << "First statement is not an ExpressionStatement.";

    auto int_lit = dynamic_cast<ast::IntegerLiteral*>(stmt->Expression.get());
    ASSERT_NE(int_lit, nullptr) << "Expression is not an Identifier.";
    EXPECT_EQ(int_lit->Value, 5) << "Identifier value is incorrect.";
    EXPECT_EQ(int_lit->TokenLiteral(), "5") << "Identifier TokenLiteral is incorrect.";
}

// TODO: refactor to combine int64_t and bool types
TEST(ParserTests, TestParsingPrefixExpressions) {
    struct PrefixTest {
        std::string input;
        std::string operator_;
        int64_t integerValue;
    };

    std::vector<PrefixTest> prefixTests = {
        {"!5;", "!", 5},
        {"-15;", "-", 15},
    };

    for (const auto& tt : prefixTests) {
        auto lexer = std::make_unique<lexer::Lexer>(tt.input);
        auto parser = std::make_unique<parser::Parser>(std::move(lexer));

        auto program = parser->ParseProgram();
        checkParserErrors(*parser);
        ASSERT_EQ(program->Statements.size(), 1)
            << "program.Statements does not contain 1 statement";

        const auto* stmt = dynamic_cast<const ast::ExpressionStatement*>(program->Statements[0].get());
        ASSERT_NE(stmt, nullptr) << "program.Statements[0] is not ast::ExpressionStatement";

        const auto* exp = dynamic_cast<const ast::PrefixExpression*>(stmt->Expression.get());
        ASSERT_NE(exp, nullptr) << "stmt is not ast::PrefixExpression";
        EXPECT_EQ(exp->Operator, tt.operator_) << "exp.Operator is not '" << tt.operator_ << "'";
        EXPECT_TRUE(testIntegerLiteral(exp->Right.get(), tt.integerValue));
    }
}

// TODO: refactor to combine int64_t and bool types
TEST(ParserTests, TestParsingPrefixExpressionsWithBooleanInput) {
    struct PrefixTest {
        std::string input;
        std::string operator_;
        bool value;
    };

    std::vector<PrefixTest> prefixTests = {
        {"!true;", "!", true},
        {"!false;", "!", false}
    };

    for (const auto& tt : prefixTests) {
        auto lexer = std::make_unique<lexer::Lexer>(tt.input);
        auto parser = std::make_unique<parser::Parser>(std::move(lexer));

        auto program = parser->ParseProgram();
        checkParserErrors(*parser);
        ASSERT_EQ(program->Statements.size(), 1)
            << "program.Statements does not contain 1 statement";

        const auto* stmt = dynamic_cast<const ast::ExpressionStatement*>(program->Statements[0].get());
        ASSERT_NE(stmt, nullptr) << "program.Statements[0] is not ast::ExpressionStatement";

        const auto* exp = dynamic_cast<const ast::PrefixExpression*>(stmt->Expression.get());
        ASSERT_NE(exp, nullptr) << "stmt is not ast::PrefixExpression";
        EXPECT_EQ(exp->Operator, tt.operator_) << "exp.Operator is not '" << tt.operator_ << "'";
        EXPECT_TRUE(testBoolean(exp->Right.get(), tt.value));
    }
}

// TODO: use testInfixExpression
TEST(ParserTests, TestParsingInfixExpressions) {
    struct InfixTest {
        std::string input;
        int64_t leftValue;
        std::string op;
        int64_t rightValue;
    };

    std::vector<InfixTest> infixTests = {
        {"5 + 5;", 5, "+", 5},
        {"5 - 5;", 5, "-", 5},
        {"5 * 5;", 5, "*", 5},
        {"5 / 5;", 5, "/", 5},
        {"5 > 5;", 5, ">", 5},
        {"5 < 5;", 5, "<", 5},
        {"5 == 5;", 5, "==", 5},
        {"5 != 5;", 5, "!=", 5}
    };

    for (const auto& tt : infixTests) {
        auto lexer = std::make_unique<lexer::Lexer>(tt.input);
        auto parser = parser::New(std::move(lexer));

        auto program = parser->ParseProgram();
        checkParserErrors(*parser);
        ASSERT_EQ(program->Statements.size(), 1)
            << "program.Statements does not contain 1 statement";

        auto stmt = dynamic_cast<ast::ExpressionStatement*>(program->Statements[0].get());
        ASSERT_NE(stmt, nullptr) << "program.Statements[0] is not ast::ExpressionStatement";

        auto infixExpr = dynamic_cast<ast::InfixExpression*>(stmt->Expression.get());
        ASSERT_NE(infixExpr, nullptr) << "stmt.Expression is not ast::InfixExpression";
        EXPECT_TRUE(testIntegerLiteral(infixExpr->Left.get(), tt.leftValue));
        EXPECT_EQ(infixExpr->Operator, tt.op) << "exp.Operator is not '" << tt.op << "'";
        EXPECT_TRUE(testIntegerLiteral(infixExpr->Right.get(), tt.rightValue));
    }
}

// TODO: use testInfixExpression
TEST(ParserTests, TestParsingInfixBooleanExpressions) {
    struct InfixTest {
        std::string input;
        bool leftValue;
        std::string op;
        bool rightValue;
    };

    std::vector<InfixTest> infixTests = {
        {"true == true", true, "==", true},
        {"true != false", true, "!=", false},
        {"false == false", false, "==", false}
    };

    for (const auto& tt : infixTests) {
        auto lexer = std::make_unique<lexer::Lexer>(tt.input);
        auto parser = parser::New(std::move(lexer));

        auto program = parser->ParseProgram();
        checkParserErrors(*parser);
        ASSERT_EQ(program->Statements.size(), 1)
            << "program.Statements does not contain 1 statement";

        auto stmt = dynamic_cast<ast::ExpressionStatement*>(program->Statements[0].get());
        ASSERT_NE(stmt, nullptr) << "program.Statements[0] is not ast::ExpressionStatement";

        auto infixExpr = dynamic_cast<ast::InfixExpression*>(stmt->Expression.get());
        ASSERT_NE(infixExpr, nullptr) << "stmt.Expression is not ast::InfixExpression";
        EXPECT_TRUE(testBoolean(infixExpr->Left.get(), tt.leftValue));
        EXPECT_EQ(infixExpr->Operator, tt.op) << "exp.Operator is not '" << tt.op << "'";
        EXPECT_TRUE(testBoolean(infixExpr->Right.get(), tt.rightValue));
    }
}

TEST(ParserTests, TestParsingOperatorPrecedence) {
    struct OperatorPrecedenceTest {
        std::string input;
        std::string expected;
    };

    std::vector<OperatorPrecedenceTest> tests {
        { "-a * b", "((-a) * b)" },
        { "!-a", "(!(-a))" },
        { "a + b + c", "((a + b) + c)" },
        { "a + b - c", "((a + b) - c)" },
        { "a * b * c", "((a * b) * c)" },
        { "a * b / c", "((a * b) / c)" },
        { "a + b / c", "(a + (b / c))" },
        { "a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)" },
        { "3 + 4; -5 * 5", "(3 + 4)((-5) * 5)" },
        { "5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))" },
        { "5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))" },
        { "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        { "1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)" },
        { "(5 + 5) * 2", "((5 + 5) * 2)" },
        { "2 / (5 + 5)", "(2 / (5 + 5))" },
        { "-(5 + 5)", "(-(5 + 5))" },
        { "!(true == true)", "(!(true == true))" }
    };

    for (const auto& tt : tests) {
        auto lexer = std::make_unique<lexer::Lexer>(tt.input);
        auto parser = parser::New(std::move(lexer));

        auto program = parser->ParseProgram();
        checkParserErrors(*parser);
        ASSERT_GT(program->Statements.size(), 0) << "program.Statements does not contain statements";

        auto result = program->String();
        ASSERT_EQ(result, tt.expected) << "Expected: " << tt.expected << ", but got: " << result;
    }
}

TEST(ParserTests, TestBoolean) {
    struct BooleanTest {
        std::string input;
        std::string expected;
    };

    std::vector<BooleanTest> tests {
        { "false;", "false" },
        { "true;", "true" }
    };

    for (const auto& tt : tests) {
        auto lexer = std::make_unique<lexer::Lexer>(tt.input);
        auto parser = parser::New(std::move(lexer));

        auto program = parser->ParseProgram();
        checkParserErrors(*parser);
        ASSERT_EQ(program->Statements.size(), 1) << "Program has incorrect number of statements.";

        auto stmt = dynamic_cast<ast::ExpressionStatement*>(program->Statements[0].get());
        ASSERT_NE(stmt, nullptr) << "First statement is not an ExpressionStatement.";

        auto bool_expr = dynamic_cast<ast::Boolean*>(stmt->Expression.get());
        ASSERT_NE(bool_expr, nullptr) << "Expression is not a Boolean";
        EXPECT_EQ(bool_expr->String(), tt.expected) << "Boolean value is incorrect.";
        EXPECT_EQ(bool_expr->TokenLiteral(), tt.expected) << "Boolean TokenLiteral is incorrect.";
    }

    std::vector<BooleanTest> tests2 {
        { "3 > 5 == false", "((3 > 5) == false)" },
        { "3 < 5 == true", "((3 < 5) == true)" }
    };

    for (const auto& tt : tests2) {
        auto lexer = std::make_unique<lexer::Lexer>(tt.input);
        auto parser = parser::New(std::move(lexer));

        auto program = parser->ParseProgram();
        checkParserErrors(*parser);
        ASSERT_GT(program->Statements.size(), 0)
            << "program.Statements does not contain statements";

        auto result = program->String();
        ASSERT_EQ(result, tt.expected) << "Expected: " << tt.expected << ", but got: " << result;
    }
}

TEST(ParserTests, TestIfExpression) {
    std::string input = "if (x < y) { x }";
    auto lexer = std::make_unique<lexer::Lexer>(input);
    auto parser = parser::New(std::move(lexer));

    auto program = parser->ParseProgram();
    checkParserErrors(*parser);
    ASSERT_EQ(program->Statements.size(), 1) << "program.Statements does not contain 1 statement";

    auto* stmt = dynamic_cast<ast::ExpressionStatement*>(program->Statements[0].get());
    ASSERT_NE(stmt, nullptr) << "program.Statements[0] is not ast::ExpressionStatement";

    auto* exp = dynamic_cast<ast::IfExpression*>(stmt->Expression.get());
    ASSERT_NE(exp, nullptr) << "stmt.Expression is not ast::IfExpression";
    ASSERT_TRUE(testInfixExpression(exp->Condition.get(), "x", "<", "y"));
    ASSERT_EQ(exp->Consequence->Statements.size(), 1)
        << "consequence does not contain 1 statement. got=" << exp->Consequence->Statements.size();

    auto* consequence = dynamic_cast<ast::ExpressionStatement*>(exp->Consequence->Statements[0].get());
    ASSERT_NE(consequence, nullptr) << "Consequence statement is not ast::ExpressionStatement";
    ASSERT_TRUE(testIdentifier(consequence->Expression.get(), "x"));
    ASSERT_EQ(exp->Alternative, nullptr)
        << "exp.Alternative was not null. got=" << exp->Alternative.get();
}

TEST(ParserTests, TestIfElseExpression) {
    std::string input = "if (x < y) { x } else { y }";
    auto lexer = std::make_unique<lexer::Lexer>(input);
    auto parser = parser::New(std::move(lexer));

    auto program = parser->ParseProgram();
    checkParserErrors(*parser);
    ASSERT_EQ(program->Statements.size(), 1) << "Statements does not contain 1 statement";

    auto* stmt = dynamic_cast<ast::ExpressionStatement*>(program->Statements[0].get());
    ASSERT_NE(stmt, nullptr) << "Statements[0] is not ast::ExpressionStatement";

    auto* exp = dynamic_cast<ast::IfExpression*>(stmt->Expression.get());
    ASSERT_NE(exp, nullptr) << "Expression is not ast::IfExpression";
    ASSERT_TRUE(testInfixExpression(exp->Condition.get(), "x", "<", "y"));
    ASSERT_EQ(exp->Consequence->Statements.size(), 1)
        << "Consequence does not contain 1 statement. Got=" << exp->Consequence->Statements.size();

    auto* consequence = dynamic_cast<ast::ExpressionStatement*>(exp->Consequence->Statements[0].get());
    ASSERT_NE(consequence, nullptr) << "Consequence statement is not ast::ExpressionStatement";
    ASSERT_TRUE(testIdentifier(consequence->Expression.get(), "x"));
    ASSERT_NE(exp->Alternative, nullptr) << "Alternative statement should not be a nullptr";
    ASSERT_EQ(exp->Alternative->Statements.size(), 1)
        << "Alternative does not contain 1 statement. Got=" << exp->Alternative->Statements.size();

    auto* alternative = dynamic_cast<ast::ExpressionStatement*>(exp->Alternative->Statements[0].get());
    ASSERT_NE(alternative, nullptr) << "Alternative statement is not ast::ExpressionStatement";
    ASSERT_TRUE(testIdentifier(alternative->Expression.get(), "y"));
}
