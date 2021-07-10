#ifndef SATISFY_PARSER_HPP
#define SATISFY_PARSER_HPP

#include <string>

#include "TokenParser.hpp"

namespace satisfy {
namespace parser {

extern GlobalTokenParser __tokenParser;

class GlobalParser {

  std::string expr_;

 public:

  // global variables, functions, etc...
  void parseGlobalExpression(void) noexcept;
};

class Parser {
  
  std::string expr_;

 public:
  
  // parse block or line of expression
  void parseExpression(void) noexcept;
};

} // ns parser
} // ns satisfy

#endif // SATISFY_PARSER_HPP
