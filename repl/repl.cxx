#include <iostream>
#include <memory>
#include <ostream>
#include <string>
#include <vector>
#include "repl.h"
#include "evaluator.h"
#include "lexer.h"
#include "parser.h"

namespace repl {

const std::string MONKEY_ASCII_ART = R"(
       .-"-.            .-"-.          .-"-.
     _/_-.-_\_        _/.-.-.\_      _/.-.-.\_
    / __} {__ \      /|( > < )|\    ( ( 0 o ) )
   / //  "  \\ \    | //  "  \\ |    |/  "  \|
  / / \'==='/ \ \  / / \'---'/ \ \    \'/^\'/
  \ \_/`"""`\_/ /  \ \_/`"""`\_/ /    /`\ /`\
   \           /    \           /    /  /|\  \
)";

void printParserErrors(std::ostream& out, std::vector<std::string> errors) {
    out << MONKEY_ASCII_ART << std::endl;
    out << "Parser Errors:" << std::endl;

    for (const auto& msg : errors) {
        out << "\t" + msg << std::endl;
    }
}

void Start(std::istream& in, std::ostream& out) {
    std::string line;

    while (true) {
        out << PROMPT;
        if (!std::getline(in, line)) {
            return;
        }

        auto lexer = std::make_unique<lexer::Lexer>(line);
        auto parser = parser::New(std::move(lexer));
        auto program = parser->ParseProgram();

        if (!parser->Errors().empty()) {
            printParserErrors(out, parser->Errors());
            continue;
        }

        auto evaluated = evaluator::Eval(*program);
        if (evaluated != nullptr) {
            out << evaluated->Inspect() << std::endl;
        }
    }
}

}
