#include "Parser.hpp"

#include <string>

using satisfy::token::TokenType;

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
      return returnToken(TokenType::tokenSeperator);
    }

    if(lch == '!') {
      lch = getNextChar();
      return returnToken(TokenType::tokenDeclareSupporter);
    }

    if(lch == '{'
       || lch == '}') {
      _identifierStr = (char)lch;
      lch = getNextChar();
      return returnToken(TokenType::tokenBlock);
    }

    if(lch == '<') {
      if((lch = getNextChar()) == '-') {
        _identifierStr = "<-";
        lch = getNextChar();
        return returnToken(TokenType::tokenClassDecl);
      }
      return returnToken(TokenType::tokenOperator);
    }

    bool flag_dash = lch == '-';
    bool isCurTokDigit = false;
    
    if(flag_dash) {
      _identifierStr = (char)lch;
      if((lch = getNextChar()) == '>') {
        _identifierStr = "->";
        lch = getNextChar();
        return returnToken(TokenType::tokenClassDecl);
      }

      isCurTokDigit = isDigit(lch);
      if(!isCurTokDigit)
        return returnToken(TokenType::tokenOperator);
    } else {
      _identifierStr.clear();
      isCurTokDigit = isDigit(lch);
    }

    // Number
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
      return returnToken(TokenType::tokenNumber);
    }

    if(flag_dash ||
       lch == '+' || lch == '>' ||
       lch == '*' || lch == '/' ||
       lch == '%' || lch == '=') {
      _identifierStr = (char)lch;
      lch = getNextChar(); // skip
      return returnToken(TokenType::tokenOperator);
    }
      
    if(lch == '.') {
      lch = getNextChar();
      return returnToken(TokenType::tokenAccess);
    }

    if(lch == ':') {
      if((lch = getNextChar()) == ':') {
        lch = getNextChar(); // skip :
        return returnToken(TokenType::tokenReturnType);
      }
      return returnToken(TokenType::tokenType);
    }

    if(isAlpha(lch)) {
      for(_identifierStr = (char)lch, lch = getNextChar();
          !isWhiteSpace(lch) && (isAlpha(lch) || isDigit(lch));
          _identifierStr += (char)lch, lch = getNextChar());

      TokenType retTok = TokenType::tokenEOF;

      if(_identifierStr == "cls")
        retTok = TokenType::tokenClass;

      if(_identifierStr == "constructor")
        retTok = TokenType::tokenConstructor;

      if(_identifierStr == "destructor")
        retTok = TokenType::tokenDestructor;

      if(_identifierStr == "ret")
        retTok = TokenType::tokenReturn;

      // if(_identifierStr == "ext" ||
      //    _identifierStr == "static")
      //   retTok = token::TokenType::tokenIdentifier;

      if(retTok == TokenType::tokenEOF)
        return returnToken(TokenType::tokenIdentifier);

      lch = getNextChar();

      return returnToken(retTok);
    }

    if(lch == EOF) {
      lch = getNextChar();
      return returnToken(TokenType::tokenEOF);
    }

    int t = lch;
    lch = getNextChar();
    return returnToken((TokenType)t);
  }

  satisfy::token::TokenType
  getNextToken(void) noexcept {
    setCRLF(false);
    return _curTok = get_token();
  }

  std::string currentTokenAsString(void) noexcept {
    switch(static_cast<TokenType>(_curTok)) {
    case TokenType::tokenReturn:
      return "ret";
    case TokenType::tokenType:
      return ":";
    case TokenType::tokenReturnType:
      return "::";
    case TokenType::tokenSeperator:
      return "/";
    case TokenType::tokenDeclareSupporter:
      return "!";
    case TokenType::tokenAccess:
      return ".";

    case TokenType::tokenChar:
      // return "\'" + _identifierStr + "\'";
    case TokenType::tokenString:
      // return "\"" + _identifierStr + "\"";

    case TokenType::tokenOperator:
    case TokenType::tokenIdentifier:
    case TokenType::tokenNumber:
    case TokenType::tokenClass:
    case TokenType::tokenClassDecl:
    case TokenType::tokenConstructor:
    case TokenType::tokenDestructor:
    case TokenType::tokenBlock:
      return _identifierStr;
      
    default:
      ;
    }

    return "";
  }

} // ns parser
} // ns satisfy
