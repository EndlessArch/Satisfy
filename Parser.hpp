#ifndef SATISFY_PARSER_HPP
#define SATISFY_PARSER_HPP

#include <string>

namespace satisfy {
namespace parser {

class GlobalParser {

  std::string expr_;

  ;

 public:

  // parse block or line of expression
  void parseExpression(void) noexcept;
};

} // ns parser
} // ns satisfy

#endif // SATISFY_PARSER_HPP
