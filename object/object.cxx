#include "object.h"

namespace object {

std::string Boolean::Inspect() const {
    return Value ? "true" : "false";
}

std::string Integer::Inspect() const {
    return std::to_string(Value);
}

std::string Null::Inspect() const {
    return "null";
}

}
