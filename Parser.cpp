#include "Parser.hpp"

#include <iostream>

namespace satisfy {
namespace parser {

GlobalTokenParser __tokenParser = {};

void
GlobalParser::parseGlobalExpression(void) noexcept {
  
  auto tokTy = __tokenParser.getLastToken();

  if(tokTy == TokenType::tokenEOF)
    return;

  if(tokTy == TokenType::ImportDirective) {
    return;
  }

  if(tokTy == TokenType::Supporter) {
    return;
  }

  if(tokTy == TokenType::Identifier) {
    return;
  }

  std::cerr << "Unexpected " << satisfy::tokenToStr(tokTy)
            << " \'" << __tokenParser.getIdentifierStr() << "\'\n";

  return;
}

void
Parser::parseExpression(void) noexcept {

  auto tokTy = __tokenParser.getLastToken();

  std::cerr << "Unexpected " << satisfy::tokenToStr(tokTy)
            << " \'" << __tokenParser.getIdentifierStr() << "\'\n";

  return;
}

} // ns parser
} // ns satisfy
