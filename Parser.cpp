#include "Parser.hpp"

#include <string>

namespace satisfy {
namespace parser {

  token::TokenType _curTok;

  token::TokenType
  get_token(void) noexcept {
    static int lch = ' ';

    // skip until the next word came
    while(isWhiteSpace(lch))
      lch = getNextChar();

    // comment
    if(lch == '#')
      while((lch = getNextChar()) == '\n')
        return returnToken(get_token());

    if(lch == '/')
      return returnToken(token::TokenType::tokenSeperator);

    if(lch == '!')
      return returnToken(token::TokenType::tokenDeclareSupporter);

    if(lch == '<') {
      if((lch = getNextChar()) == '-')
        return returnToken(token::TokenType::tokenClassEnd);
      return returnToken(token::TokenType::tokenOperator);
    }

    bool flag_dash = lch == '-';

    if(flag_dash) {
      if((lch = getNextChar()) == '>')
        return returnToken(token::TokenType::tokenClassStart);
      return returnToken(token::TokenType::tokenOperator);
    }

    if(flag_dash ||
       lch == '+' || lch == '>' ||
       lch == '*' || lch == '/' ||
       lch == '%')
      return returnToken(token::TokenType::tokenOperator);
      
    if(lch == '.')
      return returnToken(token::TokenType::tokenAccess);

    if(lch == ':') {
      if((lch = getNextChar()) == ':') {
        lch = getNextChar(); // skip :
        return returnToken(token::TokenType::tokenReturnType);
      }
      return returnToken(token::TokenType::tokenType);
    }

    if(isDigit(lch)) {
      _identifierStr.clear();
      do {
        _identifierStr += lch;
      } while(isDigit(lch = getNextChar()));

      _numVal = toDigit(_identifierStr.c_str());
      return returnToken(token::TokenType::tokenNumber);
    }

    if(isAlpha(lch)) {
      _identifierStr = lch;
      for(_identifierStr = lch, lch = getNextChar();
          isAlpha(lch) || isDigit(lch);
          lch = getNextChar());

      std::cout << (std::string)_identifierStr << std::endl;

      if(_identifierStr == "cls")
        return returnToken(token::TokenType::tokenClass);

      if(_identifierStr == "constructor")
        return returnToken(token::TokenType::tokenConstructor);

      if(_identifierStr == "destructor")
        return returnToken(token::TokenType::tokenDestructor);

      if(_identifierStr == "ret")
        return returnToken(token::TokenType::tokenReturn);

      // if(_identifierStr == "ext" ||
      //    _identifierStr == "static")
      //   return token::TokenType::tokenIdentifier;

      return returnToken(token::TokenType::tokenIdentifier);
    }

    if(lch == EOF)
      return returnToken(token::TokenType::tokenEOF);

    int t = lch;
    lch = getNextChar();
    return returnToken((token::TokenType)t);
  }

  satisfy::token::TokenType
  getNextToken(void) noexcept {
    return _curTok = get_token();
  }

} // ns parser
} // ns satisfy
