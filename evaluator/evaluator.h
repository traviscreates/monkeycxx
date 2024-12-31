#ifndef EVALUATOR_H
#define EVALUATOR_H

#include <memory>
#include <vector>
#include "ast.h"
#include "object.h"

namespace evaluator {
    std::shared_ptr<object::Object> Eval(const ast::Node& node);
    std::shared_ptr<object::Object> evalStatements(const std::vector<std::unique_ptr<ast::Statement>>& statements);
}

#endif
