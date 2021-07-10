#include "Token.hpp"

namespace satisfy {

std::string
tokenToStr(TokenType tokTy) noexcept {
  switch(tokTy) {
    case TokenType::tokenEOF:
      break;
    case TokenType::Number:
      return "number";
    case TokenType::Identifier:
      return "identifier";
    case TokenType::DeclType:
      return "declare type";
    case TokenType::Operator:
      return "operator";
    case TokenType::Seperator:
      return "seperator";
    case TokenType::Supporter:
      return "supporter";
    case TokenType::DeclSupporter:
      return "declare supporter";
    case TokenType::ImportDirective:
      return "import directive";
    default:
      return "unknown type";
  }

  return "EOF(End-Of-File)";
}

} // ns satisfy
