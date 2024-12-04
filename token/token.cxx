#include <unordered_map>
#include "token.h"

namespace token {

static const std::unordered_map<std::string, TokenType> keywords = {
    {"fn", FUNCTION},
    {"let", LET},
    {"true", TRUE},
    {"false", FALSE},
    {"if", IF},
    {"else", ELSE},
    {"return", RETURN},
};

TokenType LookupIdent(const std::string& ident) {
    auto it = keywords.find(ident);
    if (it != keywords.end()) {
        return it->second;
    }
    return IDENT;
}

}
