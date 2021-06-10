#ifndef SATISFY_TOKENPARSER_HPP
#define SATISFY_TOKENPARSER_HPP

#include <vector>

#include "AST.hpp"
#include "Token.hpp"

using namespace satisfy::ast;

using satisfy::token::TokenType;

namespace satisfy {
namespace tokParser {

namespace details {
bool isIdentifier(TokenType) noexcept;
}

SafeExprPtr parseStatement(void) noexcept;
SafeExprPtr parseExpression(void) noexcept;

std::vector<VariableAST> parseList(void) noexcept;

void parseContents(CodeBlockAST &) noexcept;

void parseBlock(CodeBlockAST &) noexcept;

void parseRootExpression(CodeAST &) noexcept;

void parseLoop(CodeAST &) noexcept;

} // ns tokParser
} // ns satisfy

#endif // SATISFY_TOKENPARSER_HPP
