#include "lexer.h"
#include "token.h"

namespace lexer {

Lexer::Lexer(const std::string &input) : input(input), position(0), readPosition(0), ch(0) {
    readChar();
}

char Lexer::peekChar() {
    if (readPosition >= input.size()) {
        return 0;
    } else {
        return input[readPosition];
    }
}

bool Lexer::isLetter(const char ch) {
    return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || (ch == '_');
}

bool Lexer::isDigit(const char ch) {
    return ('0' <= ch) && (ch <= '9');
}

void Lexer::readChar() {
    if (readPosition >= input.size()) {
        ch = 0;
    } else {
        ch = input[readPosition];
    }
    position = readPosition;
    readPosition += 1;
}

std::string Lexer::readIdentifier() {
    size_t startPosition = position;
    while (isLetter(ch)) {
        readChar();
    }
    return input.substr(startPosition, position - startPosition);
}

std::string Lexer::readNumber() {
    size_t startPosition = position;
    while (isDigit(ch)) {
        readChar();
    }
    return input.substr(startPosition, position - startPosition);
}

void Lexer::skipWhitespace() {
    while (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
        readChar();
    }
}

token::Token Lexer::nextToken() {
    token::Token tok;

    skipWhitespace();

    switch (ch) {
        case '=':
            if (peekChar() == '=') {
                readChar();
                tok = {token::EQ, "=="};
            } else {
                tok = {token::ASSIGN, "="};
            }
            break;
        case '+':
            tok = {token::PLUS, "+"};
            break;
        case '-':
            tok = {token::MINUS, "-"};
            break;
        case '!':
            if (peekChar() == '=') {
                readChar();
                tok = {token::NOT_EQ, "!="};
            } else {
                tok = {token::BANG, "!"};
            }
            break;
        case '/':
            tok = {token::SLASH, "/"};
            break;
        case '*':
            tok = {token::ASTERISK, "*"};
            break;
        case '<':
            tok = {token::LT, "<"};
            break;
        case '>':
            tok = {token::GT, ">"};
            break;
        case ';':
            tok = {token::SEMICOLON, ";"};
            break;
        case ',':
            tok = {token::COMMA, ","};
            break;
        case '(':
            tok = {token::LPAREN, "("};
            break;
        case ')':
            tok = {token::RPAREN, ")"};
            break;
        case '{':
            tok = {token::LBRACE, "{"};
            break;
        case '}':
            tok = {token::RBRACE, "}"};
            break;
        case 0:
            tok.Literal = "";
            tok.Type = token::EOF_;
            break;
        default:
            if (isLetter(ch)) {
                const std::string identifier = readIdentifier();
                tok = {token::LookupIdent(identifier), identifier};
                return tok;
            } else if (isDigit(ch)) {
                tok = {token::INT, readNumber()};
                return tok;
            } else {
                tok = {token::ILLEGAL, std::string(1, ch)};
            }
            break;
    }

    readChar();
    return tok;
}

} // namespace lexer
