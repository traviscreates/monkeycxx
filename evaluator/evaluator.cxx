#include "evaluator.h"
#include <memory>
#include <string>
#include "ast.h"
#include "object.h"

namespace evaluator {

    const std::shared_ptr<object::Null> NULL_ = std::make_shared<object::Null>();
    const std::shared_ptr<object::Boolean> TRUE = std::make_shared<object::Boolean>(
            object::Boolean{true});
    const std::shared_ptr<object::Boolean> FALSE = std::make_shared<object::Boolean>(
            object::Boolean{false});

    std::shared_ptr<object::Object> Eval(const ast::Node& node) {
        if (const auto* program = dynamic_cast<const ast::Program*>(&node)) {
            return evalProgram(program);
        }

        if (const auto* exprStmt = dynamic_cast<const ast::ExpressionStatement*>(&node)) {
            return Eval(*exprStmt->Expression);
        }

        if (const auto* intLiteral = dynamic_cast<const ast::IntegerLiteral*>(&node)) {
            return std::make_shared<object::Integer>(object::Integer{intLiteral->Value});
        }

        if (const auto* boolNode = dynamic_cast<const ast::Boolean*>(&node)) {
            return boolNode->Value ? TRUE : FALSE;
        }

        if (const auto* prefixExpr = dynamic_cast<const ast::PrefixExpression*>(&node)) {
            auto right = Eval(*prefixExpr->Right);
            return evalPrefixExpression(prefixExpr->Operator, right);
        }

        if (const auto* infixExpr = dynamic_cast<const ast::InfixExpression*>(&node)) {
            auto left = Eval(*infixExpr->Left);
            auto right = Eval(*infixExpr->Right);
            return evalInfixExpression(infixExpr->Operator, left, right);
        }

        if (const auto* blockStmt = dynamic_cast<const ast::BlockStatement*>(&node)) {
            return evalBlockStatement(*blockStmt);
        }

        if (const auto* ifExpr = dynamic_cast<const ast::IfExpression*>(&node)) {
            return evalIfExpression(*ifExpr);
        }

        if (const auto* returnStmt = dynamic_cast<const ast::ReturnStatement*>(&node)) {
            auto result = Eval(*returnStmt->ReturnValue);
            return std::make_shared<object::ReturnValue>(result);
        }

        return nullptr;
    }

    std::shared_ptr<object::Object> evalProgram(const ast::Program* program) {
        std::shared_ptr<object::Object> result = nullptr;

        for (const auto& stmt : program->Statements) {
            result = Eval(*stmt);

            if (const auto* returnValue = dynamic_cast<const object::ReturnValue*>(result.get())) {
                return returnValue->Value;
            }
        }

        return result;
    }

    std::shared_ptr<object::Object> evalPrefixExpression(
            const std::string& op, const std::shared_ptr<object::Object>& right) {
        if (op == "!") {
            return evalBangOperatorExpression(right);
        }

        if (op == "-") {
            return evalMinusPrefixOperatorExpression(right);
        }

        return NULL_;
    }

    std::shared_ptr<object::Object> evalBangOperatorExpression(
            const std::shared_ptr<object::Object>& right) {
        if (right == TRUE) {
            return FALSE;
        }

        if (right == FALSE || right == NULL_) {
            return TRUE;
        }

        return FALSE;
    }

    std::shared_ptr<object::Object> evalMinusPrefixOperatorExpression(
            const std::shared_ptr<object::Object>& right) {
        if (right->Type() != object::ObjectType::INTEGER) {
            return NULL_;
        }

        auto intObj = std::dynamic_pointer_cast<object::Integer>(right);
        if (intObj) {
            return std::make_shared<object::Integer>(object::Integer{-intObj->Value});
        }

        return NULL_;
    }

    std::shared_ptr<object::Object> evalInfixExpression(
            const std::string& op, const std::shared_ptr<object::Object>& left,
            const std::shared_ptr<object::Object>& right) {
        if (
            left->Type() == object::ObjectType::BOOLEAN &&
            right->Type() == object::ObjectType::BOOLEAN) {
            return evalBooleanInfixExpression(op, left, right);
        }

        if (
            left->Type() == object::ObjectType::INTEGER &&
            right->Type() == object::ObjectType::INTEGER) {
            return evalIntegerInfixExpression(op, left, right);
        }

        return NULL_;
    }

    std::shared_ptr<object::Object> evalIntegerInfixExpression(
            const std::string& op, const std::shared_ptr<object::Object>& left,
            const std::shared_ptr<object::Object>& right) {
        auto leftIntObj = std::dynamic_pointer_cast<object::Integer>(left);
        auto rightIntObj = std::dynamic_pointer_cast<object::Integer>(right);
        if (!leftIntObj || !rightIntObj) {
            return NULL_;
        }

        int64_t leftInt = leftIntObj->Value;
        int64_t rightInt = rightIntObj->Value;

        if (op == "+") {
            return std::make_shared<object::Integer>(object::Integer{leftInt + rightInt});
        } else if (op == "-") {
            return std::make_shared<object::Integer>(object::Integer{leftInt - rightInt});
        } else if (op == "*") {
            return std::make_shared<object::Integer>(object::Integer{leftInt * rightInt});
        } else if (op == "/") {
            if (rightInt == 0) {
                return NULL_;
            }
            return std::make_shared<object::Integer>(object::Integer{leftInt / rightInt});
        } else if (op == "<") {
            return leftInt < rightInt ? TRUE : FALSE;
        } else if (op == ">") {
            return leftInt > rightInt ? TRUE : FALSE;
        } else if (op == "==") {
            return leftInt == rightInt ? TRUE : FALSE;
        } else if (op == "!=") {
            return leftInt != rightInt ? TRUE : FALSE;
        }

        return NULL_;
    }

    std::shared_ptr<object::Object> evalBooleanInfixExpression(
            const std::string& op, const std::shared_ptr<object::Object>& left,
            const std::shared_ptr<object::Object>& right) {
        if (op == "==") {
            return left == right ? TRUE : FALSE;
        }

        if (op == "!=") {
            return left != right ? TRUE : FALSE;
        }

        return NULL_;
    }

    std::shared_ptr<object::Object> evalIfExpression(const ast::IfExpression& ifExpr) {
        auto condition = Eval(*ifExpr.Condition);

        if (isTruthy(condition)) {
            return Eval(*ifExpr.Consequence);
        }

        if (ifExpr.Alternative) {
            return Eval(*ifExpr.Alternative);
        }

        return NULL_;
    }

    std::shared_ptr<object::Object> evalBlockStatement(const ast::BlockStatement& blockStmnt) {
        std::shared_ptr<object::Object> result = nullptr;

        for (const auto& stmt : blockStmnt.Statements) {
            result = Eval(*stmt);

            if (const auto* returnValue = dynamic_cast<const object::ReturnValue*>(result.get())) {
                return result;
            }
        }

        return result;
    }

    bool isTruthy(const std::shared_ptr<object::Object>& obj) {
        if (obj == NULL_) {
            return false;
        }

        if (obj == TRUE) {
            return true;
        }

        if (obj == FALSE) {
            return false;
        }

        return true;
    }
}
