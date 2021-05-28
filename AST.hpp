#ifndef SATISFY_AST_HPP
#define SATISFY_AST_HPP

#include <initializer_list>
#include <list>
#include <string>
#include <memory>
#include <llvm/IR/Value.h>

#include "Codegen.hpp"

using satisfy::codegen::CodeGenContext;

namespace satisfy {
namespace ast {

  class AST {
  public:
    virtual ~AST() = default;

    // Hate virtual functions,
    // but can't help with CRTP too..
    virtual llvm::Value * codegen(CodeGenContext &) noexcept = 0;
  };

  class IdentifierAST : public AST {
    std::string idName_;
  public:
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  class VariableAST : public AST {
    std::string varName_;
  public:
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  class ExprAST : public AST {
  public:
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  using SafeExprPtr = std::shared_ptr<ExprAST>;

  class CodeBlockAST : public ExprAST {
    std::list<const SafeExprPtr> exprs_;
  public:
    CodeBlockAST(void) = default;
    CodeBlockAST(const SafeExprPtr);
    CodeBlockAST(std::initializer_list<const SafeExprPtr>);

    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };
    
} // ns ast
} // ns satisfy

#endif // SATISFY_AST_HPP
