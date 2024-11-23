#pragma once

#include <iostream>
#include <string>
#include "lexer/lexer.h"
#include "token/token.h"

namespace repl {

const std::string PROMPT = ">> ";

void Start(std::istream& in, std::ostream& out);

}
