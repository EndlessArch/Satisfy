#ifndef SATISFY_TOKENPARSER_HPP
#define SATISFY_TOKENPARSER_HPP

#ifndef NDEBUG
#include <iostream>
#endif
#include <mutex>
#include <string>

#include "Pos.hpp"
#include "Token.hpp"

namespace satisfy {

class GlobalTokenParser {

  static std::mutex read_mutex_;
  
  std::string last_token_string_;
  TokenType last_token_type_;

  Pos file_pos_;

  static int getNextChar(void) noexcept;

  bool isWhitespace(int p) noexcept;

  inline
  bool isAlphabetic(int p) noexcept {
    return std::isalnum(p) || p == '_';
  }

  inline
  TokenType returnToken(TokenType tokTy) noexcept {
    return last_token_type_ = tokTy;
  }

 public:

  ;

  TokenType parseToken(void) noexcept;

  TokenType getLastToken(void) noexcept;

  std::string getIdentifierStr(void) noexcept;

  int asI64(void) noexcept;
  unsigned asU64(void) noexcept;
  double asF64(void) noexcept;

  Pos getPos(void) noexcept;
};

} // ns satisfy

#endif // SATISFY_TOKENPARSER_HPP
