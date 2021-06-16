#include "TokenParser.hpp"

namespace satisfy {

int
GlobalTokenParser::getNextChar(void) noexcept {
  return std::getchar();
}

TokenType
GlobalTokenParser::parseToken(void) noexcept {
  static int lch = (int)' ';

  while(isWhitespace(lch))
    lch = getNextChar();

  if(lch == EOF) {
    lch = getNextChar();
    return TokenType::EOF;
  }

  if(lch == '#')
    while((lch = getNextChar()) != '\n');

  if(lch == ':') {
    if((lch = getNextChar()) == ':') {
      this->last_token_string_ = "::";
      lch = getNextChar();
    } else
      this->last_token_string_ = ":";
    return TokenType::DeclType;
  }

  if(lch == '/') {
    this->last_token_string_ = (char)lch;
    return TokenType::Seperator;
  }

  if(lch == '+' || lch == '-' ||
     lch == '*'/* || lch == '|'*/) {
    this->last_token_string_ = (char)lch;
    lch = getNextChar();
    return TokenType::Operator;
  }

  if(lch == '&') {
    if((lch = getNextChar()) == '&') {
      this->last_token_string_ = "&&";
      lch = getNextChar();
      return TokenType::Operator;
    }
    // skip, IDK
    lch = getNextChar();
  }

  if(lch == '|') {
    if((lch = getNextChar()) == '|') {
      this->last_token_string_ = "||";
      lch = getNextChar();
    } else
      this->last_token_string_ = "|";
    return TokenType::Operator;
  }

  // xor
  if(lch == '^') {
    if((lch = getNextChar()) == '^') {
      this->last_token_string_ = "^^";
      lch = getNextChar();
      return TokenType::Operator;
    }
    // skip
    lch = getNextChar();
  }

  if(lch == '!') {
    if((lch = getNextChar()) == '!') {
      this->last_token_string_ = "!!";
      lch = getNextChar();
      return TokenType::Operator;
    }
    this->last_token_string_ = "!";
    return TokenType::Supporter;
  }

  if(isAlphabetic(lch)) {
    ;
  }
}

} // ns satisfy
