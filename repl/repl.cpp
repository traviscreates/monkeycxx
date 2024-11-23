#include <iostream>
#include "repl.h"

namespace repl {

void Start(std::istream& in, std::ostream& out) {
    std::string line;

    while (true) {
        out << PROMPT;
        if (!std::getline(in, line)) {
            return;
        }

        lexer::Lexer l(line);
        token::Token tok;

        while ((tok = l.nextToken()).Type != token::EOF_) {
            out << tok.Type << " " << tok.Literal << std::endl;
        }
    }
}

}
