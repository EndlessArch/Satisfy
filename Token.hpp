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
    tokenClassDecl = -11,

    tokenConstructor = -12,
    tokenDestructor = -13,

    // .
    tokenAccess = -14,

    // { }
    tokenBlock = -15,

    tokenChar = -16,
    tokenString = -17
  };
  
} // ns token
} // ns satisfy

#endif // SATISFY_TOKEN_HPP
