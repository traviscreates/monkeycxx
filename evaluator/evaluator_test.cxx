#include "gtest/gtest.h"
#include "lexer.h"
#include "parser.h"
#include "object.h"
#include "evaluator.h"

std::shared_ptr<object::Object> testEval(const std::string& input) {
    auto lexer = std::make_unique<lexer::Lexer>(input);
    auto parser = parser::New(std::move(lexer));
    auto program = parser->ParseProgram();
    return evaluator::Eval(*program);
}

bool testIntegerObject(const object::Object& obj, int64_t expected) {
    const auto* intObj = dynamic_cast<const object::Integer*>(&obj);

    if (!intObj) {
        ADD_FAILURE() << "object::Object is not Integer. Got: " << typeid(obj).name();
        return false;
    }

    if (intObj->Value != expected) {
        ADD_FAILURE() << "object::Object has wrong value. Got: " << intObj->Value
                      << ", expected: " << expected;
        return false;
    }

    return true;
}

bool testBooleanObject(const object::Object& obj, bool expected) {
    const auto* boolObj = dynamic_cast<const object::Boolean*>(&obj);

    if (!boolObj) {
        std::cerr << "object::Object is not Boolean. Got: " << typeid(obj).name() << std::endl;
        return false;
    }

    if (boolObj->Value != expected) {
        std::cerr << "object::Object has wrong value. Got: " << std::boolalpha << boolObj->Value
                  << ", expected: " << expected << std::endl;
        return false;
    }

    return true;
}

TEST(EvaluatorTests, TestEvalIntegerExpression) {
    struct TestCase {
        std::string input;
        int64_t expected;
    };

    std::vector<TestCase> tests = {
        {"5", 5},
        {"10", 10},
        {"-5", -5},
        {"-10", -10},
    };

    for (const auto& test : tests) {
        auto evaluated = testEval(test.input);
        ASSERT_TRUE(evaluated) << "Evaluated object is null.";
        ASSERT_TRUE(testIntegerObject(*evaluated, test.expected));
    }
}

TEST(EvaluatorTests, TestEvalBooleanExpression) {
    struct TestCase {
        std::string input;
        bool expected;
    };

    std::vector<TestCase> tests = {
        {"false", false},
        {"true", true},
    };

    for (const auto& test : tests) {
        auto evaluated = testEval(test.input);
        ASSERT_TRUE(evaluated) << "Evaluated object is null.";
        ASSERT_TRUE(testBooleanObject(*evaluated, test.expected));
    }
}

TEST(EvaluatorTests, TestBangOperator) {
    struct TestCase {
        std::string input;
        bool expected;
    };

    std::vector<TestCase> tests = {
        {"!true", false},
        {"!false", true},
        {"!5", false},
        {"!!true", true},
        {"!!false", false},
        {"!!5", true},
    };

    for (const auto& test : tests) {
        auto evaluated = testEval(test.input);
        ASSERT_TRUE(testBooleanObject(*evaluated, test.expected));
    }
}
