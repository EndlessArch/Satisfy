#include <iostream>
#include <optional>

#include "AST.hpp"
#include "Codegen.hpp"
#include "Token.hpp"
#include "Parser.hpp"
#include "Error.hpp"

using satisfy::token::TokenType;
using satisfy::parser::getNextToken;
using satisfy::parser::_curTok;
using satisfy::printErr;

using namespace satisfy::ast;
using namespace satisfy::codegen;

void HandleIdentifier(CodeBlockAST &) noexcept;

bool isIdentifier(void) noexcept {
  if(_curTok != TokenType::tokenIdentifier) {
    err_idf_expected();
    // skip ill-formed expression
    getNextToken();
    return false;
  }
  return true;
}

bool parseIdentifier(void) noexcept {
  getNextToken();
  return isIdentifier();
}

void HandleClass(CodeBlockAST & _me) noexcept {

  if(getNextToken() != TokenType::tokenIdentifier) {
    err_idf_expected();
    // skip
    getNextToken();
  }

   ;

  return;
}

void HandleDeclareSupporter(CodeBlockAST & _me) noexcept {

  if(getNextToken() != TokenType::tokenIdentifier) {
    err("Expected identifier");

    getNextToken();
  }

  ;

  return;
}

std::vector<VariableAST> parseList(CodeBlockAST & _me) noexcept {
  ;
  std::vector<VariableAST> list;

  // eat /
  getNextToken();

  IdentifierAST type;
  std::string name;

  while(_curTok != TokenType::tokenSeperator) {
    if(!isIdentifier())
      return {};

    type = IdentifierAST(satisfy::parser::getIdentifierStr());

    if(getNextToken() != TokenType::tokenType) {
      printErr("Expected variable type expression(\':\')");
      return {};
    }
    if(!parseIdentifier())
      return {};
    name = satisfy::parser::getIdentifierStr();

    list.emplace_back(type, name);

    getNextToken();
  }

  // eat /
  getNextToken();

  return list;
}

// parse expression inside function
void parseBlock(CodeBlockAST & _me) noexcept {
  
  // eat {
  getNextToken();

  while(!(_curTok == TokenType::tokenBlock
          && satisfy::parser::getIdentifierStr() == "}")) {  
    // parse return
    if(_curTok == TokenType::tokenReturn) {
      switch(getNextToken()) {
      case TokenType::tokenIdentifier:
        {
          std::string str = satisfy::parser::getIdentifierStr();

          _me.push(SafeExprPtr(new ReturnAST(VariableAST(str))));
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
          NumberAST num(_me.getParent()->getReturnType(),
                        satisfy::parser::getNumVal());
          _me.push(SafeExprPtr(new ReturnAST(num)));
        }
        break;
      default:
        printErr("Cannot return \'"
                 + satisfy::parser::currentTokenAsString()
                 + "\'(" + std::to_string(_curTok) + ")");
        getNextToken();
        return;
      }
      continue;
    }

    // identifiers
    if(_curTok == TokenType::tokenIdentifier) {
      HandleIdentifier(_me);
    }
    else {
      std::cout << _curTok << "IDK\n";
      return;
    }
  }

  // eat }
  getNextToken();

  return;
}

void HandleIdentifier(CodeBlockAST & _me) noexcept {

  IdentifierAST type(satisfy::parser::getIdentifierStr());

  // -> variable
  if(getNextToken() == TokenType::tokenType) {
    // variable type
    if(!parseIdentifier())
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

      _me.push(safeBlock);
      return;
    }
      
    SafeExprPtr null = SafeExprPtr((ExprAST *)new NumberAST(type, 0));

    SafeExprPtr assignNull =
      SafeExprPtr((ExprAST *)new AssignmentAST(name, null));

    const SafeExprPtr safeBlock =
      SafeExprPtr((ExprAST *)new VariableAST(type, name, assignNull));

    _me.push(safeBlock);

    return;
  }

  // -> function
  if(_curTok == TokenType::tokenReturnType) {
    if(!parseIdentifier())
      return;
    std::string funcName = satisfy::parser::getIdentifierStr();

    std::vector<VariableAST> argList;
    CodeBlockAST functionEntryBlock;
      
    // get function parameters
    if(getNextToken() == TokenType::tokenSeperator)
      argList = parseList(_me);

    FunctionAST preFuncDef(funcName, type, argList);
    SafeExprPtr fSafe(new FunctionAST(preFuncDef));
    _me.push(fSafe);

    if(!(_curTok == TokenType::tokenBlock
         && satisfy::parser::getIdentifierStr() == "{"))
      return;

    std::cout << "1: " << _me.getExprSize() << std::endl;

    parseBlock(_me);

    std::cout << "2: " << _me.getExprSize() << std::endl;

    reinterpret_cast<FunctionAST *>(fSafe.get())->setCodeBody(functionEntryBlock);

    return;
  }

  std::cout << "Expr unexpected: "
            << _curTok << " : " << satisfy::parser::getIdentifierStr() << std::endl;
  err_expr_unexpected();

  return;
}

void parseLoop(CodeBlockAST & mainEntry) noexcept {

  while(_curTok = TokenType::tokenEOF,
        getNextToken() != TokenType::tokenEOF) {
    ;
    HandleIdentifier(mainEntry);
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
      HandleIdentifier(mainEntry);
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
      HandleDeclareSupporter(mainEntry);
      continue;
    }

    // -10
    if(_curTok == TokenType::tokenClass) {
      HandleClass(mainEntry);
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

int main(int argc, char * argv[]) {

  satisfy::codegen::CodeGenContext ctx;

  // llvm::FunctionType * ft
  //   = llvm::FunctionType::get(satisfy::codegen::getBasicInt(ctx.getLLVMCtx()),
  //                             false);

  // llvm::Function * mainFunction_
  //   = llvm::Function::Create(ft,
  //                            llvm::GlobalValue::ExternalLinkage,
  //                            "main",
  //                            ctx.getModule());
  
  // llvm::BasicBlock * bb = llvm::BasicBlock::Create(ctx.getLLVMCtx(),
  //                                                  "entry",
  //                                                  mainFunction_
  //                                                  );
  
  CodeBlockAST mainEntry;

  // ctx.pushBlock(bb);

  parseLoop(mainEntry);

  ctx.generateCode(mainEntry);

  return 0;
}
