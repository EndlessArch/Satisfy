#ifndef SATISFY_PARSER_HPP
#define SATISFY_PARSER_HPP

#include <cstdio>
#include <cctype>
#include <string>

#include "Token.hpp"
#include "Error.hpp"

#include <iostream>

namespace satisfy {
namespace parser {

  static std::string _identifierStr;
  static double _numVal;

  inline
  bool isWhiteSpace(int i) noexcept {
    if(i == '\n' || i == '\r') {
      satisfy::updateLine();

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
    return std::isdigit(i) || i == '.';
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
    // std::cout << (int)tokType << '\n';

    updateRow();

    return tokType;
  }

  extern satisfy::token::TokenType _curTok;

  inline
  std::string getIdentifierStr(void) noexcept {
    return _identifierStr;
  }

  inline
  double getNumVal(void) noexcept {
    return _numVal;
  }

  satisfy::token::TokenType
  getNextToken(void) noexcept;

} // ns parser  
} // ns satisfy

#endif // SATISFY_PARSER_HPP
