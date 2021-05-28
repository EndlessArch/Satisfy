#include <iostream>

#include "Token.hpp"
#include "Parser.hpp"
#include "Error.hpp"

using satisfy::token::TokenType;
using satisfy::parser::getNextToken;
using satisfy::parser::_curTok;
using satisfy::printErr;



void HandleClass(void) noexcept {

  if(getNextToken() != TokenType::tokenIdentifier) {
    err_idf_expected();
    // skip
    getNextToken();
  }

  ;

  return;
}

void HandleDeclareSupporter(void) noexcept {

  if(getNextToken() != TokenType::tokenIdentifier) {
    err("Expected identifier");

    getNextToken();
  }

  ;

  return;
}

void HandleIdentifier(void) noexcept {

  // -> variable
  if(getNextToken() == TokenType::tokenType) {
    // variable type
    if(getNextToken() == TokenType::tokenIdentifier) {
      // TODO: create Variable AST
      return;
    }
    else {
      err_idf_expected();
      return;
    }
  }

  // -> function
  if(_curTok == TokenType::tokenReturnType) {
    if(getNextToken() == TokenType::tokenIdentifier) {
      ;
      return;
    }
    else {
      err_idf_expected();
      return;
    }
  }

  err_expr_unexpected();

  return;
}

void parseLoop(void) noexcept {

  while(getNextToken() != TokenType::tokenEOF) {

    if(_curTok == TokenType::tokenReturn) {
      err("Return should be occured in a function");
      goto POST_PARSE;
    }

    if(_curTok == TokenType::tokenOperator) {
      err("Operator is at out of block");
      goto POST_PARSE;
    }

    if(_curTok == TokenType::tokenType) {
      err("Expected variable name");
      goto POST_PARSE;
    }

    if(_curTok == TokenType::tokenReturnType) {
      err("Expected function return type");
      goto POST_PARSE;
    }

    if(_curTok == TokenType::tokenIdentifier) {
      HandleIdentifier();
      continue;
    }

    if(_curTok == TokenType::tokenNumber) {
      err("Expected expression");
      goto POST_PARSE;
    }

    if(_curTok == TokenType::tokenSeperator) {
      err("Seperator cannot come before function declaration");
      goto POST_PARSE;
    }
    
    if(_curTok == TokenType::tokenDeclareSupporter) {
      HandleDeclareSupporter();
      continue;
    }

    if(_curTok == TokenType::tokenClass) {
      HandleClass();
      continue;
    }

    if(_curTok == TokenType::tokenClassStart) {
      err("\'->\' should come after the \'cls\' keyword");
      goto POST_PARSE;
    }

    if(_curTok == TokenType::tokenClassEnd) {
      err("\'<-\' should come after the class declaration");
      goto POST_PARSE;
    }

    if(_curTok == TokenType::tokenConstructor) {
      err("Constructor is only be able to be declared inside the class");
      goto POST_PARSE;
    }

    if(_curTok == TokenType::tokenDestructor) {
      err("Destructor is only be able to be declared inside the class");
      goto POST_PARSE;
    }

    if(_curTok == TokenType::tokenAccess) {
      err("Expected expression");
      goto POST_PARSE;
    }

  POST_PARSE:
    // getNextToken();
    ;
  }

  return;
}

int main(int argc, char * argv[]) {

  parseLoop();

  return 0;
}
