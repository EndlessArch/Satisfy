#include "Parser.hpp"

#include <string>

namespace satisfy {
namespace parser {

  std::string _identifierStr;
  value_type _numVal;
  switch_type _switches;

  token::TokenType _curTok;

  ///

  void onCRLF(void) noexcept {
    satisfy::updateLine();

    setCRLF(true);
  }

  token::TokenType
  get_token(void) noexcept {
    static int lch = ' ';

    _switches[1] = false;

    // skip until the next word came
    while(isWhiteSpace(lch))
      lch = getNextChar();

    // comment
    if(lch == '#') {
      while((lch = getNextChar()) == '\n') {
        onCRLF();
        return returnToken(get_token());
      }
    }

    if(lch == '/') {
      lch = getNextChar();
      return returnToken(token::TokenType::tokenSeperator);
    }

    if(lch == '!') {
      lch = getNextChar();
      return returnToken(token::TokenType::tokenDeclareSupporter);
    }

    if(lch == '<') {
      if((lch = getNextChar()) == '-') {
        lch = getNextChar();
        return returnToken(token::TokenType::tokenClassEnd);
      }
      return returnToken(token::TokenType::tokenOperator);
    }

    bool flag_dash = lch == '-';
    bool isCurTokDigit = false;
    
    if(flag_dash) {
      _identifierStr = (char)lch;
      if((lch = getNextChar()) == '>') {
        lch = getNextChar();
        return returnToken(token::TokenType::tokenClassStart);
      }

      isCurTokDigit = isDigit(lch);
      if(!isCurTokDigit)
        return returnToken(token::TokenType::tokenOperator);
    } else {
      _identifierStr.clear();
      isCurTokDigit = isDigit(lch);
    }

    if(isCurTokDigit) {
      do {
        _identifierStr += (char)lch;
        isCurTokDigit = isDigit(lch = getNextChar());
        if(!isCurTokDigit && !_switches[1] && lch == '.') {
          _switches[1] = true;
          isCurTokDigit = true;
        }
      } while(isCurTokDigit);

      if(_identifierStr.back() == '.')
        _identifierStr += '0';
      if(!_switches[1]) {
        if(_switches[0])
          _numVal = (unsigned)strtoul(_identifierStr.c_str(), nullptr, 10);
        else
          _numVal = atoi(_identifierStr.c_str());
      } else
        _numVal = strtod(_identifierStr.c_str(), nullptr);
      return token::TokenType::tokenNumber;      
    }

    if(flag_dash ||
       lch == '+' || lch == '>' ||
       lch == '*' || lch == '/' ||
       lch == '%' || lch == '=') {
      _identifierStr = (char)lch;
      lch = getNextChar(); // skip
      return returnToken(token::TokenType::tokenOperator);
    }
      
    if(lch == '.') {
      lch = getNextChar();
      return returnToken(token::TokenType::tokenAccess);
    }

    if(lch == ':') {
      if((lch = getNextChar()) == ':') {
        lch = getNextChar(); // skip :
        return returnToken(token::TokenType::tokenReturnType);
      }
      return returnToken(token::TokenType::tokenType);
    }

    if(isAlpha(lch)) {
      for(_identifierStr = (char)lch, lch = getNextChar();
          !isWhiteSpace(lch) && (isAlpha(lch) || isDigit(lch));
          _identifierStr += (char)lch, lch = getNextChar());

      token::TokenType retTok = token::tokenEOF;

      if(_identifierStr == "cls")
        retTok = token::TokenType::tokenClass;

      if(_identifierStr == "constructor")
        retTok = token::TokenType::tokenConstructor;

      if(_identifierStr == "destructor")
        retTok = token::TokenType::tokenDestructor;

      if(_identifierStr == "ret")
        retTok = token::TokenType::tokenReturn;

      // if(_identifierStr == "ext" ||
      //    _identifierStr == "static")
      //   retTok = token::TokenType::tokenIdentifier;

      if(retTok == token::tokenEOF)
        return returnToken(token::tokenIdentifier);

      lch = getNextChar();

      return returnToken(retTok);
    }

    if(lch == EOF) {
      lch = getNextChar();
      return returnToken(token::TokenType::tokenEOF);
    }

    int t = lch;
    lch = getNextChar();
    return returnToken((token::TokenType)t);
  }

  satisfy::token::TokenType
  getNextToken(void) noexcept {
    setCRLF(false);
    return _curTok = get_token();
  }

} // ns parser
} // ns satisfy
