#ifndef SATISFY_TOKEN_HPP
#define SATISFY_TOKEN_HPP

#include <string>

namespace satisfy {

enum class TokenType {
  tokenEOF = -1,

  Number = -2,

  Identifier = -3,

  // : ::
  DeclType = -4,

  // +-*| or && || ^^ !!
  Operator = -5,

  // /
  Seperator = -6,

  // !
  Supporter = -7,

  // cst/fnl cls -> & <- const dest
  DeclSupporter = -8,

  // imp
  ImportDirective = -9,
};

std::string tokenToStr(TokenType) noexcept;

} // ns satisfy

#endif // SATISFY_TOKEN_HPP
