#ifndef SATISFY_TOKENPARSER_HPP
#define SATISFY_TOKENPARSER_HPP

#include <vector>

#include "AST.hpp"

using namespace satisfy::ast;

namespace satisfy {
namespace tokParser {

void parseStatement(CodeAST &) noexcept;

std::vector<VariableAST> parseList(CodeAST &) noexcept;

void parseBlock(FunctionAST &, CodeBlockAST &) noexcept;

void parseIdentifier(CodeAST &) noexcept;

void parseLoop(CodeAST &) noexcept;

} // ns tokParser
} // ns satisfy

#endif // SATISFY_TOKENPARSER_HPP
