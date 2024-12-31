#include "evaluator.h"
#include <memory>
#include "ast.h"
#include "object.h"

namespace evaluator {

    std::shared_ptr<object::Object> Eval(const ast::Node& node) {
       if (const auto* program = dynamic_cast<const ast::Program*>(&node)) {
           return evalStatements(program->Statements);
       }

       if (const auto* exprStmt = dynamic_cast<const ast::ExpressionStatement*>(&node)) {
           return Eval(*exprStmt->Expression);
       }

       if (const auto* intLiteral = dynamic_cast<const ast::IntegerLiteral*>(&node)) {
           return std::make_shared<object::Integer>(object::Integer{intLiteral->Value});
       }

       return nullptr;
    }

    std::shared_ptr<object::Object> evalStatements(const std::vector<std::unique_ptr<ast::Statement>> &statements) {
        std::shared_ptr<object::Object> result = nullptr;

        for (const auto& stmt : statements) {
            result = Eval(*stmt);
        }

        return result;
    }

}
