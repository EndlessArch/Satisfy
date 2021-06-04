#include "TokenParser.hpp"

#include "Error.hpp"
#include "Parser.hpp"
#include "Token.hpp"

using satisfy::parser::_curTok;
using satisfy::token::TokenType;

using namespace satisfy::parser;

namespace satisfy {
namespace tokParser {

namespace details {

bool isIdentifier(TokenType _tt) noexcept {
  if(_tt != TokenType::tokenIdentifier) {
    err_idf_expected();
    return false;
  }

  return true;
}

} // ns details

void parseStatement(CodeAST _ca) noexcept {
  return;
}

std::vector<VariableAST> parseList(CodeAST & _ca) noexcept {
  std::vector<VariableAST> list;

  // eat /
  getNextToken();

  IdentifierAST type;
  std::string name;

  while(_curTok != TokenType::tokenSeperator) {
    if(!details::isIdentifier(_curTok))
      return {};

    type = IdentifierAST(satisfy::parser::getIdentifierStr());

    if(getNextToken() != TokenType::tokenType) {
      printErr("Expected variable type expression(\':\')");
      return {};
    }
    if(!details::isIdentifier(getNextToken()))
      return {};
    name = satisfy::parser::getIdentifierStr();

    list.emplace_back(type, name);

    getNextToken();
  }

  // eat /
  getNextToken();

  return list;
}

void parseBlock(FunctionAST & _fa, CodeBlockAST & _cb) noexcept {

  // eat {
  getNextToken();

  while(!(_curTok == TokenType::tokenBlock
          && satisfy::parser::getIdentifierStr() == "}")) {

    switch (static_cast<TokenType>(_curTok)) {
      // parse return
    case TokenType::tokenReturn:
      {
      switch (getNextToken()) {
      case TokenType::tokenIdentifier:
        {
          std::string str = satisfy::parser::getIdentifierStr();
          _cb.push(SafeExprPtr(new ReturnAST(VariableAST(str))));
        }
        break;
      case TokenType::tokenNumber:
        {
          // parseBlock is occured inside the function
          // so maybe this would be useless
          // if(!_me.getParent()) {
          //   printErr("Return should be occured inside the function");
          //   getNextToken();
          //   return;
          // }
          NumberAST num(_fa.getFuncRetType(),
                        satisfy::parser::getNumVal());
          _cb.push(SafeExprPtr(new ReturnAST(num)));
        }
        break;
      default:
        printErr("Cannot return \'"
                 + satisfy::parser::currentTokenAsString()
                 + "\'(" + std::to_string(_curTok) + ")");
        }
      getNextToken();
      }
      break;

      // parse identifier
    case TokenType::tokenIdentifier:
      break;
    }

  }

  // eat }
  getNextToken();

  return;
}

void parseIdentifier(CodeAST & _ca) noexcept {
  
  IdentifierAST type(satisfy::parser::getIdentifierStr());

  // -> variable
  if(getNextToken() == TokenType::tokenType) {
    // variable type
    if(!details::isIdentifier(getNextToken()))
      return;

    std::string name(satisfy::parser::getIdentifierStr());
    // TODO: add constructor initializer('(')
    if(getNextToken() == TokenType::tokenOperator) {

      if(satisfy::parser::getIdentifierStr() != "=")
        err((std::string)"Unexpected operator \'" + (char)_curTok + "\'");
      
      getNextToken();
      if(_curTok != TokenType::tokenIdentifier
         && _curTok != TokenType::tokenNumber) {
        err_idf_expected();
        return;
      }

      SafeExprPtr get;
      if(_curTok == TokenType::tokenIdentifier)
        get = SafeExprPtr((ExprAST *)new IdentifierAST(satisfy::parser::getIdentifierStr()));
      else
        get = SafeExprPtr((ExprAST *)new NumberAST(type, satisfy::parser::getNumVal()));

      SafeExprPtr safeAssign =
        SafeExprPtr((ExprAST *)new AssignmentAST(name, get));
      const SafeExprPtr safeBlock =
        SafeExprPtr((ExprAST *)new VariableAST(type, name, safeAssign));

      _ca.push(safeBlock);
      return;
    }
      
    SafeExprPtr null = SafeExprPtr((ExprAST *)new NumberAST(type, 0));

    SafeExprPtr assignNull =
      SafeExprPtr((ExprAST *)new AssignmentAST(name, null));

    const SafeExprPtr safeBlock =
      SafeExprPtr((ExprAST *)new VariableAST(type, name, assignNull));

    _ca.push(safeBlock);

    return;
  }

  // -> function
  if(_curTok == TokenType::tokenReturnType) {
    if(!details::isIdentifier(getNextToken()))
      return;
    std::string funcName = satisfy::parser::getIdentifierStr();

    std::vector<VariableAST> argList;
    CodeBlockAST funcEntryBlock;
      
    // get function parameters
    if(getNextToken() == TokenType::tokenSeperator)
      argList = parseList(_ca);

    FunctionAST preFuncDef(funcName, type, argList);
    SafeExprPtr fSafe(new FunctionAST(preFuncDef));
    _ca.push(fSafe);

    if(!(_curTok == TokenType::tokenBlock
         && satisfy::parser::getIdentifierStr() == "{"))
      return;

    parseBlock(preFuncDef, funcEntryBlock);

    reinterpret_cast<FunctionAST *>(fSafe.get())->setCodeBody(funcEntryBlock);

    return;
  }

  std::cout << "Expr unexpected: "
            << _curTok << " : " << satisfy::parser::getIdentifierStr() << std::endl;
  err_expr_unexpected();


  return;
}

void parseLoop(CodeAST & code) noexcept {

  while(_curTok = TokenType::tokenEOF,
        getNextToken() != TokenType::tokenEOF) {
    ;
    // HandleIdentifier(code);
    goto POST_PARSE;

    // -2
    if(_curTok == TokenType::tokenReturn) {
      err("Return should be occured in a function");
      goto POST_PARSE;
    }

    // -3
    if(_curTok == TokenType::tokenOperator) {
      err("Operator is at out of block");
      goto POST_PARSE;
    }

    // -4
    if(_curTok == TokenType::tokenType) {
      err("Expected variable name");
      goto POST_PARSE;
    }

    // -5
    if(_curTok == TokenType::tokenReturnType) {
      err("Expected function return type");
      goto POST_PARSE;
    }

    // -6
    if(_curTok == TokenType::tokenIdentifier) {
      // HandleIdentifier(code);
      // continue;
      goto POST_PARSE;
    }

    // -7
    if(_curTok == TokenType::tokenNumber) {
      err("Expected expression");
      goto POST_PARSE;
    }

    // -8
    if(_curTok == TokenType::tokenSeperator) {
      err("Seperator cannot come before function declaration");
      goto POST_PARSE;
    }
    
    // -9
    if(_curTok == TokenType::tokenDeclareSupporter) {
      // HandleDeclareSupporter(code);
      continue;
    }

    // -10
    if(_curTok == TokenType::tokenClass) {
      // HandleClass(code);
      continue;
    }

    // -11
    if(_curTok == TokenType::tokenClassDecl) {
      err((std::string)"\'"
          + satisfy::parser::getIdentifierStr()
          + "\' should come after the class declaration");
      goto POST_PARSE;
    }

    // -12
    if(_curTok == TokenType::tokenConstructor) {
      err("Constructor is only be able to be declared inside the class");
      goto POST_PARSE;
    }

    // -13
    if(_curTok == TokenType::tokenDestructor) {
      err("Destructor is only be able to be declared inside the class");
      goto POST_PARSE;
    }

    // -14
    if(_curTok == TokenType::tokenAccess) {
      err("Expected expression");
      goto POST_PARSE;
    }

  POST_PARSE:
    ;
    return;
  }

  return;
}

} // namespace tokParser
} // ns satisfy
