#ifndef SATISFY_TOKEN_HPP
#define SATISFY_TOKEN_HPP

namespace satisfy {

enum class TokenType {
  EOF = -1,

  Identifier = -2,

  // : ::
  DeclType = -3,

  // +-*| or && || ^^ !!
  Operator = -4,

  // /
  Seperator = -5,

  // !
  Supporter = -6,

  // cst cls -> & <- const dest
  DeclSupporter = -7,
};

} // ns satisfy

#endif // SATISFY_TOKEN_HPP
