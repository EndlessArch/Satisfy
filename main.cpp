
#include <iostream>

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

// executable block
static satisfy::ast::CodeBlockAST _execBlock;

void HandleClass(satisfy::codegen::CodeGenContext & _cgc) noexcept {

  if(getNextToken() != TokenType::tokenIdentifier) {
    err_idf_expected();
    // skip
    getNextToken();
  }

   ;

  return;
}

void HandleDeclareSupporter(satisfy::codegen::CodeGenContext & _cgc) noexcept {

  if(getNextToken() != TokenType::tokenIdentifier) {
    err("Expected identifier");

    getNextToken();
  }

  ;

  return;
}

void HandleIdentifier(CodeGenContext & _cgc) noexcept {

  IdentifierAST type(satisfy::parser::getIdentifierStr());
  std::cout << "type = \"" << satisfy::parser::getIdentifierStr() << "\"\n";

  // -> variable
  if(getNextToken() == TokenType::tokenType) {
    // variable type
    if(getNextToken() == TokenType::tokenIdentifier) {
      std::string name(satisfy::parser::getIdentifierStr());
      std::cout << "name = \"" << name << "\"\n";

      // TODO: add constructor initializer('(')
      if(getNextToken() == TokenType::tokenOperator) {
        if(satisfy::parser::getIdentifierStr() == "=") {
          getNextToken();
          if(_curTok != TokenType::tokenIdentifier
             && _curTok != TokenType::tokenNumber) {
            std::cout << _curTok << std::endl;
            std::cout << satisfy::parser::getIdentifierStr() << std::endl;
            // std::cout << satisfy::parser::_numVal << std::endl;
            err_idf_expected();
            return;
          }
          SafeExprPtr get;
          if(_curTok == TokenType::tokenIdentifier) {
            // IdentifierAST id(satisfy::parser::getIdentifierStr());
            // get = std::shared_ptr<ExprAST>((ExprAST *)&id);
            get
              = std::shared_ptr<ExprAST>
              ((ExprAST *)new IdentifierAST(satisfy::parser::getIdentifierStr()));
          } else {
            // NumberAST num(satisfy::parser::getNumVal());
            // get = std::shared_ptr<ExprAST>((ExprAST *)&num);

            get = std::shared_ptr<ExprAST>
              ((ExprAST *)new NumberAST(satisfy::parser::getNumVal()));
          }

          SafeExprPtr safeAssign = std::shared_ptr<ExprAST>
            ((ExprAST *)new AssignmentAST(name, get));
          const SafeExprPtr safeBlock = std::shared_ptr<ExprAST>
            ((ExprAST *)new VariableAST(type, name, safeAssign));
          // safeBlock->codegen(_cgc)->print(llvm::errs());
          _execBlock.push(safeBlock);
          return;
        } else
          err((std::string)"Unexpected operator \'" + (char)_curTok + "\'");
      }
      
      const SafeExprPtr safeBlock = std::shared_ptr<ExprAST>
        ((ExprAST *)new VariableAST(type, name));
      _execBlock.push(safeBlock);

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
      std::string funcName = satisfy::parser::getIdentifierStr();
      
      return;
    }
    else {
      err_idf_expected();
      return;
    }
  }

  std::cout << _curTok << std::endl;
  std::cout << satisfy::parser::getIdentifierStr() << std::endl;

  err_expr_unexpected();

  return;
}

void parseLoop(satisfy::codegen::CodeGenContext & _cgc) noexcept {

  while(_curTok = TokenType::tokenEOF,
        getNextToken() != TokenType::tokenEOF) {

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
      HandleIdentifier(_cgc);
      // continue;
      goto POST_PARSE;
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
      HandleDeclareSupporter(_cgc);
      continue;
    }

    if(_curTok == TokenType::tokenClass) {
      HandleClass(_cgc);
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

    return;
  }

  return;
}

int main(int argc, char * argv[]) {

  satisfy::codegen::CodeGenContext ctx;

  parseLoop(ctx);

  ctx.generateCode(_execBlock);

  std::cerr << "Dumping...\n\"";
  ctx.getModule().print(llvm::errs(), nullptr);
  std::cerr << "\"\nDone...\n";

  return 0;
}
