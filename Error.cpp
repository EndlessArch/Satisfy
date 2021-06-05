#include "Error.hpp"

#include <iostream>

#include "Token.hpp"
#include "Parser.hpp"

namespace satisfy {

  // namespace error {

  //   ;

  // }

  void updateRow(void) noexcept {

    int i = 0;

    switch(parser::_curTok) {
    case token::TokenType::tokenAccess:
    case token::TokenType::tokenBlock:
    case token::TokenType::tokenDeclareSupporter:
    case token::TokenType::tokenSeperator:
    case token::TokenType::tokenType:
    case token::TokenType::tokenOperator:
      i = 1;
      break;
    case token::TokenType::tokenClassDecl:
    case token::TokenType::tokenReturnType:
      i = 2;
      break;
    case token::TokenType::tokenClass:
    case token::TokenType::tokenReturn:
      i = 3; // cls
      break;
    case token::TokenType::tokenDestructor:
      i = 10;
      break;
    case token::TokenType::tokenConstructor:
      i = 11;
      break;
    case token::TokenType::tokenIdentifier:
    case token::TokenType::tokenNumber:
      i = parser::getIdentifierStr().length();
      break;
    default:
      i = 1;
    }

    error::_row += i;

    return;
  }

  void updateLine(void) noexcept {

    ++error::_col;
    error::_row = 0;

    return;
  }

  void printErr(const char * str) noexcept {
    std::cerr << "error("
              << (error::_row + 2) << ", " << (error::_col + 2)
              << "): " << str << '\n';
  }

  void printErr(std::string str) noexcept {
    printErr(str.c_str());
  }

  void printErr(const char * fileName,
                int fileLine,
                const char * funcName,
                std::string str) noexcept {

    std::cerr << fileName << ":" << fileLine
              << "(" << funcName << "): ";
    printErr(str);
    
    return;
  }

} // ns satisfy
