#include "TokenParser.hpp"

#include "Error.hpp"
#include "Parser.hpp"

using satisfy::parser::_curTok;
using satisfy::token::TokenType;

using namespace satisfy::parser;

namespace satisfy {
namespace tokParser {

namespace details {

bool isIdentifier(TokenType _tt) noexcept {
  if (_tt != TokenType::tokenIdentifier) {
    err_idf_expected();
    return false;
  }

  return true;
}

}  // namespace details

SafeExprPtr parseStatement(void) noexcept {
  CodeBlockAST block;

  std::string stmt = getIdentifierStr();

  if(stmt == "if") {
    // if ~ {
    getNextToken();
    SafeExprPtr ifStmt = parseExpression();

    CodeBlockAST thenBlk;
    parseBlock(thenBlk);

    IfAST ifast(ifStmt, thenBlk);
    if(getIdentifierStr() == "else") {
      CodeBlockAST elseBlk;
      getNextToken();
      parseBlock(elseBlk);
      ifast.pushElse(elseBlk);
    }

    block.push(SafeExprPtr(new IfAST(ifast)));
  }
  else if(stmt == "for") {
    if(getNextToken() == TokenType::tokenNumber) {
      // number for
      CodeBlockAST forBlock;
      parseBlock(forBlock);
      block.push(
          SafeExprPtr(
              (ExprAST *)new ForAST(
                  parseExpression(),
                  SafeExprPtr((ExprAST *)new CodeBlockAST(forBlock)))));
    } else {
      CodeBlockAST init, forBlock;
      SafeExprPtr sep = parseExpression();
      if(_curTok != TokenType::tokenBlock) {
        init.push(sep);
        while(_curTok != TokenType::tokenSeperator) {
          sep = parseExpression();
          init.push(sep);
        }

        getNextToken();
        
        SafeExprPtr cond = parseExpression();

        if(_curTok == TokenType::tokenSeperator) {
          getNextToken();
          SafeExprPtr upd = parseExpression();
          parseBlock(forBlock);
          std::cout << "Parsed for-block\n";
          block.push(
              SafeExprPtr((ExprAST *)new ForAST(
                  SafeExprPtr((ExprAST *)new CodeBlockAST(init)), cond, upd,
                  SafeExprPtr((ExprAST *)new CodeBlockAST(forBlock)))));
        } else
          printErr("Expected seperator");
      } else {
        parseBlock(forBlock);
        block.push(SafeExprPtr(
            new ForAST(sep,
                       SafeExprPtr((ExprAST *)new CodeBlockAST(forBlock)))));
      }
    }
  } else
    block.push(parseExpression());

  return SafeExprPtr((ExprAST *)new CodeBlockAST(block));
}

SafeExprPtr parseExpression(void) noexcept {
  SafeExprPtr lhs = nullptr;

  do {
    switch (static_cast<TokenType>(_curTok)) {
      case TokenType::tokenIdentifier:
        {
          if (lhs.get()) return lhs;
          std::string name = getIdentifierStr();
          if(getNextToken() == TokenType::tokenSeperator) {
            // call function
            getNextToken();
            std::vector<VariableAST> argList = parseList();
            // eat seperator
            getNextToken();

            return SafeExprPtr(new CallAST(name, argList));
          }
          lhs = SafeExprPtr((ExprAST *)new VariableAST(name));
        }
        break;
      case TokenType::tokenNumber:
        if (lhs.get()) return lhs;
        lhs = SafeExprPtr(
            (ExprAST *)new NumberAST("int", getNumVal()));
        getNextToken();
        break;
      case TokenType::tokenType:
        if(getNextToken() != TokenType::tokenIdentifier) {
          err_idf_expected();
          getNextToken(); // skip
          return lhs;
        }
        {
          std::string varName = getIdentifierStr();
          getNextToken();
          std::cout << "\"" << reinterpret_cast<VariableAST *>(lhs.get())->varName_ << "\"" << std::endl;
          lhs = SafeExprPtr((ExprAST *)new VariableAST(
              reinterpret_cast<VariableAST *>(lhs.get())->varName_, varName));
        }
        break;
      case TokenType::tokenSeperator:
      case TokenType::tokenBlock:
        if(getIdentifierStr() == "{") {
          CodeBlockAST block;
          parseBlock(block);
          return SafeExprPtr((ExprAST *)new CodeBlockAST(block));
        }
#ifndef NDEBUG
        if(!lhs.get()) {
          err_expr_expected();
          assert(false);
        }
#endif
        return lhs;
      case TokenType::tokenOperator:
        {
#ifndef NDEBUG
          if(!lhs.get()) {
            std::string op = getIdentifierStr();
            getNextToken();

            if (_curTok != TokenType::tokenIdentifier) {
              err_idf_expected();
              assert(false);
            }

            lhs = SafeExprPtr((ExprAST *)new UnaryOperatorAST(
                op, VariableAST(getIdentifierStr())));
            break;
          }
#endif
          auto op = getIdentifierStr();

          getNextToken();

          lhs = SafeExprPtr(
              (ExprAST *)new BinaryOperatorAST(op, lhs, parseExpression()));

          ;
        }
        return lhs;
      default:
        if(lhs.get())
          return lhs;
        err_expr_expected();
        assert(false);
    }
  } while (_curTok != TokenType::tokenEOF);

  return lhs;  // IDK
}

std::vector<VariableAST> parseList(void) noexcept {
  std::vector<VariableAST> list;
  
  std::string type;
  std::string name;

  while (!(_curTok == TokenType::tokenBlock && getIdentifierStr() == "{")
         && _curTok != TokenType::tokenSeperator) {
    if (!details::isIdentifier(_curTok)) return {};

    type = satisfy::parser::getIdentifierStr();

    if (getNextToken() != TokenType::tokenType) {
      printErr("Expected variable type expression(\':\')");
      return {};
    }
    if (!details::isIdentifier(getNextToken())) return {};
    name = satisfy::parser::getIdentifierStr();

    list.emplace_back(type, name);

    getNextToken();
  }

  return list;
}

void parseContents(CodeBlockAST & _cb) noexcept {

  while (!(_curTok == TokenType::tokenBlock &&
           satisfy::parser::getIdentifierStr() == "}")) {
    switch (static_cast<TokenType>(_curTok)) {
        // parse return
      case TokenType::tokenReturn:
        getNextToken();
        _cb.push(SafeExprPtr(new ReturnAST(parseExpression())));
        break;

        // parse identifier
      case TokenType::tokenIdentifier:
        _cb.push(parseStatement());
        break;

      case TokenType::tokenBlock:
        {
          CodeBlockAST block;
          parseBlock(block);
          _cb.push(SafeExprPtr(new CodeBlockAST(block)));
          break;
        }
        
      default:
        std::cout << "Unhandled type; \'" << getIdentifierStr() << "\'\n";
        getNextToken();
    }
  }

  return;
}

void parseBlock(CodeBlockAST & _cb) noexcept {
  // eat {
  getNextToken();

  parseContents(_cb);

  // eat }
  getNextToken();

  return;
}

void parseRootExpression(CodeAST & _ca) noexcept {
  std::string type(satisfy::parser::getIdentifierStr());

  // -> global variable
  if (getNextToken() == TokenType::tokenType) {
    // variable type
    if (!details::isIdentifier(getNextToken())) return;

    std::string name(satisfy::parser::getIdentifierStr());
    // TODO: add constructor initializer('(')
    if (getNextToken() == TokenType::tokenOperator) {
      if (satisfy::parser::getIdentifierStr() != "=")
        err((std::string) "Unexpected operator \'" + (char)_curTok + "\'");

      getNextToken();
      if (_curTok != TokenType::tokenIdentifier &&
          _curTok != TokenType::tokenNumber) {
        err_idf_expected();
        return;
      }

      SafeExprPtr get;
      if (_curTok == TokenType::tokenIdentifier)
        get = SafeExprPtr(
            (ExprAST *)new VariableAST(satisfy::parser::getIdentifierStr()));
      else
        get = SafeExprPtr(
            (ExprAST *)new NumberAST(type, satisfy::parser::getNumVal()));

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
  if (_curTok == TokenType::tokenReturnType) {
    if (!details::isIdentifier(getNextToken())) return;
    std::string funcName = satisfy::parser::getIdentifierStr();

    std::vector<VariableAST> argList;

    // get function parameters
    if (getNextToken() != TokenType::tokenBlock
        && _curTok != TokenType::tokenSeperator)
      argList = parseList();
    else {
      FunctionAST funcDecl(funcName, type, {}, {});
      _ca.push(SafeExprPtr(new FunctionAST(funcDecl)));
      return;
    }

    CodeBlockAST funcEntryBlock;

    parseBlock(funcEntryBlock);

    _ca.push(SafeExprPtr(new FunctionAST(funcName,
                                         type,
                                         argList,
                                         funcEntryBlock)));

    return;
  }

  std::cout << "Expr unexpected: " << _curTok << " : "
            << satisfy::parser::getIdentifierStr() << std::endl;
  err_expr_unexpected();

  return;
}

void parseLoop(CodeAST &code) noexcept {
  getNextToken();
  while (_curTok != TokenType::tokenEOF) {
    // -2
    if (_curTok == TokenType::tokenReturn) {
      err("Return should be occured in a function");
      goto POST_PARSE;
    }

    // -3
    if (_curTok == TokenType::tokenOperator) {
      err("Operator is at out of block");
      goto POST_PARSE;
    }

    // -4
    if (_curTok == TokenType::tokenType) {
      err("Expected variable name");
      goto POST_PARSE;
    }

    // -5
    if (_curTok == TokenType::tokenReturnType) {
      err("Expected function return type");
      goto POST_PARSE;
    }

    // -6
    if (_curTok == TokenType::tokenIdentifier) {
      parseRootExpression(code);
      continue;
    }

    // -7
    if (_curTok == TokenType::tokenNumber) {
      err("Expected expression");
      goto POST_PARSE;
    }

    // -8
    if (_curTok == TokenType::tokenSeperator) {
      err("Seperator cannot come before function declaration");
      goto POST_PARSE;
    }

    // -9
    if (_curTok == TokenType::tokenDeclareSupporter) {
      // HandleDeclareSupporter(code);
      continue;
    }

    // -10
    if (_curTok == TokenType::tokenClass) {
      // HandleClass(code);
      continue;
    }

    // -11
    if (_curTok == TokenType::tokenClassDecl) {
      err((std::string) "\'" + satisfy::parser::getIdentifierStr() +
          "\' should come after the class declaration");
      goto POST_PARSE;
    }

    // -12
    if (_curTok == TokenType::tokenConstructor) {
      err("Constructor is only be able to be declared inside the class");
      goto POST_PARSE;
    }

    // -13
    if (_curTok == TokenType::tokenDestructor) {
      err("Destructor is only be able to be declared inside the class");
      goto POST_PARSE;
    }

    // -14
    if (_curTok == TokenType::tokenAccess) {
      err("Expected expression");
      goto POST_PARSE;
    }

  POST_PARSE:;
    return;
  }

  return;
}

}  // namespace tokParser
}  // namespace satisfy
