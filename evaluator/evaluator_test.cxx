#include "gtest/gtest.h"
#include "lexer.h"
#include "parser.h"
#include "object.h"
#include "evaluator.h"

struct TestCase {
    std::string input;
    int64_t expected;
};

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

TEST(EvaluatorTests, TestEvalIntegerExpression) {
    std::vector<TestCase> tests = {
        {"5", 5},
        {"10", 10},
    };

    for (const auto& test : tests) {
        auto evaluated = testEval(test.input);
        ASSERT_TRUE(evaluated) << "Evaluated object is null";
        ASSERT_TRUE(testIntegerObject(*evaluated, test.expected));
    }
}
