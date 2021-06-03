#ifndef SATISFY_PARSER_HPP
#define SATISFY_PARSER_HPP

#include <cstdio>
#include <cctype>
#include <string>
#include <variant>

#include "Token.hpp"
#include "Error.hpp"

#include <bitset>
#include <iostream>

namespace satisfy {
namespace parser {

  extern std::string _identifierStr;

  using value_type = typename std::variant<int, unsigned, double>;
  using switch_type = typename std::bitset<4>;

  extern value_type _numVal;

  // 0: crlf, 1: '.', 2: '-'
  extern switch_type _switches;

  // forward declaration
  inline void setCRLF(bool) noexcept;

  void onCRLF(void) noexcept;

  inline
  bool isWhiteSpace(int i) noexcept {
    if(i == '\n' || i == '\r') {
      onCRLF();

      return true;
    }

    return i == ' ' || i == '\f' || i == '\t' || i == '\v';
  }

  inline
  bool isAlpha(int i) noexcept {
    return std::isalpha(i);
  }

  inline
  bool isDigit(int i) noexcept {
    return std::isdigit(i);
  }

  inline
  int getNextChar(void) noexcept {
    int t = std::getchar();
    // std::cout << "NC: " << (char)t << "\n";
    return t;
  }

  inline
  double toDigit(const char * str) noexcept {
    return strtod(str, nullptr);
  }

  satisfy::token::TokenType
  get_token(void) noexcept;

  inline
  satisfy::token::TokenType
  returnToken(satisfy::token::TokenType tokType) noexcept {

    updateRow();

    return tokType;
  }

  extern satisfy::token::TokenType _curTok;

  inline
  std::string getIdentifierStr(void) noexcept {
    return _identifierStr;
  }

  inline
  auto getNumVal(void) noexcept {
    return _numVal;
  }

  satisfy::token::TokenType
  getNextToken(void) noexcept;

  void setCRLF(bool b) noexcept {
    _switches.set(0, b);
  }

  inline
  bool wasCRLF(void) noexcept {
    return _switches[0];
  }

  std::string currentTokenAsString(void) noexcept;

} // ns parser  
} // ns satisfy

// #define getPureValue(void)       \
//   (std::visit(([](auto && x) -> auto && {       \
//     return x;      \
//   }), satisfy::parser::getNumVal())

#endif // SATISFY_PARSER_HPP
