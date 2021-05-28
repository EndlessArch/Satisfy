#ifndef SATISFY_TOKEN_HPP
#define SATISFY_TOKEN_HPP

namespace satisfy {
namespace token {

  enum TokenType {
    tokenEOF = -1,

    // ret
    tokenReturn = -2,

    tokenOperator = -3,

    // :
    tokenType = -4,
    // ::
    tokenReturnType = -5,

    tokenIdentifier = -6,

    tokenNumber = -7,

    // /
    tokenSeperator = -8,

    // !
    tokenDeclareSupporter = -9,

    // cls
    tokenClass = -10,
    // ->
    tokenClassStart = -11,
    // <-
    tokenClassEnd = -12,

    tokenConstructor = -13,
    tokenDestructor = -14,

    // .
    tokenAccess = -15,
  };
  
} // ns token
} // ns satisfy

#endif // SATISFY_TOKEN_HPP
