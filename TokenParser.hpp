#ifndef SATISFY_TOKENPARSER_HPP
#define SATISFY_TOKENPARSER_HPP

#include <string>

#include "Token.hpp"

namespace satisfy {

class GlobalTokenParser {
  
  std::string last_token_string_;
  TokenType last_token_type_;

  int getNextChar(void) noexcept;

  inline
  bool isWhitespace(int p) noexcept {
    return isspace(p);
  }

  inline
  bool isAlphabetic(int p) noexcept {
    return std::isalpha(p);
  }

 public:

  ;

  TokenType parseToken(void) noexcept;
};

} // ns satisfy

#endif // SATISFY_TOKENPARSER_HPP
