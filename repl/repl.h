#pragma once

#include <iostream>
#include <string>

namespace repl {

const std::string PROMPT = ">> ";

void Start(std::istream& in, std::ostream& out);

}
