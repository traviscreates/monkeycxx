#ifndef LEXER_H
#define LEXER_H

#include <string>
#include "token/token.h"

namespace lexer {

class Lexer {
public:
    Lexer(const std::string &input);
    token::Token nextToken();

private:
    std::string input;
    size_t position;
    size_t readPosition;
    char ch;

    char peekChar();
    bool isLetter(const char ch);
    bool isDigit(const char ch);
    void readChar();
    std::string readIdentifier();
    std::string readNumber();
    void skipWhitespace();
};

}

#endif
