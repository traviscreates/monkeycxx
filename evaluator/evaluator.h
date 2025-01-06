#ifndef EVALUATOR_H
#define EVALUATOR_H

#include <memory>
#include <vector>
#include "ast.h"
#include "object.h"

namespace evaluator {
    extern const std::shared_ptr<object::Null> NULL_;
    extern const std::shared_ptr<object::Boolean> TRUE;
    extern const std::shared_ptr<object::Boolean> FALSE;

    std::shared_ptr<object::Object> Eval(const ast::Node& node);
    std::shared_ptr<object::Object> evalStatements(
            const std::vector<std::unique_ptr<ast::Statement>>& statements);
    std::shared_ptr<object::Object> evalPrefixExpression(
            const std::string& op, const std::shared_ptr<object::Object>& right);
    std::shared_ptr<object::Object> evalBangOperatorExpression(
            const std::shared_ptr<object::Object>& right);
    std::shared_ptr<object::Object> evalMinusPrefixOperatorExpression(
            const std::shared_ptr<object::Object>& right);
    std::shared_ptr<object::Object> evalInfixExpression(
            const std::string& op, const std::shared_ptr<object::Object>& left,
            const std::shared_ptr<object::Object>& right);
    std::shared_ptr<object::Object> evalIntegerInfixExpression(
            const std::string& op, const std::shared_ptr<object::Object>& left,
            const std::shared_ptr<object::Object>& right);
    std::shared_ptr<object::Object> evalBooleanInfixExpression(
            const std::string& op, const std::shared_ptr<object::Object>& left,
            const std::shared_ptr<object::Object>& right);
    std::shared_ptr<object::Object> evalIfExpression(const ast::IfExpression& ifExpr);
    bool isTruthy(const std::shared_ptr<object::Object>& obj);
}

#endif
