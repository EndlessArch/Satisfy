#include <iostream>

#include "Parser.hpp"

int main(int argc,
         char * argv[]) {
  ;

  satisfy::parser::GlobalParser globalParser;

  satisfy::parser::__tokenParser.parseToken();

  globalParser.parseGlobalExpression();

  return 0;
}