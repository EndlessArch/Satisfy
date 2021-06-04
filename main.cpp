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

void HandleIdentifier(CodeAST &) noexcept;

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

void HandleClass(CodeAST & _me) noexcept {

  if(getNextToken() != TokenType::tokenIdentifier) {
    err_idf_expected();
    // skip
    getNextToken();
  }

   ;

  return;
}

void HandleDeclareSupporter(CodeAST & _code) noexcept {

  if(getNextToken() != TokenType::tokenIdentifier) {
    err("Expected identifier");

    getNextToken();
  }

  ;

  return;
}

// void HandleIdentifier(CodeAST & _code) noexcept {

//   IdentifierAST type(satisfy::parser::getIdentifierStr());

//   // -> variable
//   if(getNextToken() == TokenType::tokenType) {
//     // variable type
//     if(!parseIdentifier())
//       return;

//     std::string name(satisfy::parser::getIdentifierStr());
//     // TODO: add constructor initializer('(')
//     if(getNextToken() == TokenType::tokenOperator) {

//       if(satisfy::parser::getIdentifierStr() != "=")
//         err((std::string)"Unexpected operator \'" + (char)_curTok + "\'");
      
//       getNextToken();
//       if(_curTok != TokenType::tokenIdentifier
//          && _curTok != TokenType::tokenNumber) {
//         err_idf_expected();
//         return;
//       }

//       SafeExprPtr get;
//       if(_curTok == TokenType::tokenIdentifier)
//         get = SafeExprPtr((ExprAST *)new IdentifierAST(satisfy::parser::getIdentifierStr()));
//       else
//         get = SafeExprPtr((ExprAST *)new NumberAST(type, satisfy::parser::getNumVal()));

//       SafeExprPtr safeAssign =
//         SafeExprPtr((ExprAST *)new AssignmentAST(name, get));
//       const SafeExprPtr safeBlock =
//         SafeExprPtr((ExprAST *)new VariableAST(type, name, safeAssign));

//       _code.push(safeBlock);
//       return;
//     }
      
//     SafeExprPtr null = SafeExprPtr((ExprAST *)new NumberAST(type, 0));

//     SafeExprPtr assignNull =
//       SafeExprPtr((ExprAST *)new AssignmentAST(name, null));

//     const SafeExprPtr safeBlock =
//       SafeExprPtr((ExprAST *)new VariableAST(type, name, assignNull));

//     _code.push(safeBlock);

//     return;
//   }

//   // -> function
//   if(_curTok == TokenType::tokenReturnType) {
//     if(!parseIdentifier())
//       return;
//     std::string funcName = satisfy::parser::getIdentifierStr();

//     std::vector<VariableAST> argList;
//     CodeBlockAST funcEntryBlock;
      
//     // get function parameters
//     if(getNextToken() == TokenType::tokenSeperator)
//       argList = parseList(_code);

//     FunctionAST preFuncDef(funcName, type, argList);
//     SafeExprPtr fSafe(new FunctionAST(preFuncDef));
//     _code.push(fSafe);

//     if(!(_curTok == TokenType::tokenBlock
//          && satisfy::parser::getIdentifierStr() == "{"))
//       return;

//     parseBlock(preFuncDef, funcEntryBlock);

//     reinterpret_cast<FunctionAST *>(fSafe.get())->setCodeBody(funcEntryBlock);

//     return;
//   }

//   std::cout << "Expr unexpected: "
//             << _curTok << " : " << satisfy::parser::getIdentifierStr() << std::endl;
//   err_expr_unexpected();

//   return;
// }


int main(int argc, char * argv[]) {

  satisfy::codegen::CodeGenContext ctx;
  
  CodeAST codes;

  parseLoop(codes);

  ctx.generateCode(codes);

  return 0;
}
