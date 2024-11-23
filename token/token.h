#ifndef TOKEN_H
#define TOKEN_H

#include <string>

namespace token {

using TokenType = std::string;

struct Token {
    TokenType Type;
    std::string Literal;
};

// Token types
const std::string ILLEGAL = "ILLEGAL";
const std::string EOF_ = "EOF";

// Identifiers + literals
const std::string IDENT = "IDENT";
const std::string INT = "INT";

// Operators
const std::string ASSIGN = "=";
const std::string PLUS = "+";
const std::string MINUS = "-";
const std::string BANG = "!";
const std::string ASTERISK = "*";
const std::string SLASH = "/";
const std::string LT = "<";
const std::string GT = ">";
const std::string EQ = "==";
const std::string NOT_EQ = "!=";

// Delimiters
const std::string COMMA = ",";
const std::string SEMICOLON = ";";
const std::string LPAREN = "(";
const std::string RPAREN = ")";
const std::string LBRACE = "{";
const std::string RBRACE = "}";

// Keywords
const std::string FUNCTION = "FUNCTION";
const std::string LET = "LET";
const std::string TRUE = "TRUE";
const std::string FALSE= "FALSE";
const std::string IF = "IF";
const std::string ELSE = "ELSE";
const std::string RETURN = "RETURN";

// Function declaration
TokenType LookupIdent(const std::string& ident);

}

#endif
