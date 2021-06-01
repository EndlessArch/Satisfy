#ifndef SATISFY_AST_HPP
#define SATISFY_AST_HPP

#include <initializer_list>
#include <list>
#include <string>
#include <memory>
#include <llvm/IR/Value.h>

#include "Parser.hpp"

namespace satisfy {
  namespace codegen {
    class CodeGenContext;
  } // ns codegen
} // ns satisfy

using satisfy::codegen::CodeGenContext;

namespace satisfy {
namespace ast {

  class AST {
  public:
    virtual ~AST() = default;

    // Hate virtual functions,
    // but can't help with CRTP, so..
    virtual llvm::Value * codegen(CodeGenContext &) noexcept = 0;
  };

  class ExprAST : public AST {};

  /////

  class IdentifierAST;

  inline llvm::Type *
  returnTypeOf(llvm::Type * ty) noexcept {
    return ty;
  }

  llvm::Type *
  getTypeOf(llvm::LLVMContext &, const IdentifierAST &) noexcept;

  /////

  class AssignmentAST;
  class VariableAST;

  bool isFloatingPoint(llvm::LLVMContext &, VariableAST &) noexcept;

  /////

  class ReassignmentAST;
  class UnaryOperatorAST;

  /////

  class IdentifierAST : public AST {
    std::string idName_;
  public:
    IdentifierAST(const std::string &);
    llvm::Value * codegen(CodeGenContext &) noexcept override;

    friend llvm::Type* getTypeOf(llvm::LLVMContext&,
                                 const IdentifierAST&) noexcept;
    friend class AssignmentAST;
    friend class VariableAST;
  };

  using SafeExprPtr = std::shared_ptr<ExprAST>;

  class NumberAST : public AST {
    satisfy::parser::value_type val_;
  public:
    NumberAST(satisfy::parser::value_type);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  class VariableAST : public AST {
    IdentifierAST varType_;
    std::string varName_;
    // assignment expression
    SafeExprPtr varAssign_;
  public:
    // VariableAST() = delete;
    VariableAST(const IdentifierAST &, std::string);
    VariableAST(const IdentifierAST &, std::string, SafeExprPtr);
    llvm::Value * codegen(CodeGenContext &) noexcept override;

    friend bool isFloatingPoint(llvm::LLVMContext&, VariableAST&) noexcept;
    friend class ReassignmentAST;
    friend class UnaryOperatorAST;
  };

  // only for when assigning new fresh variable
  class AssignmentAST : public ExprAST {
    std::string lhs_; // variable
    SafeExprPtr rhs_; // value
  public:
    AssignmentAST(const std::string &, SafeExprPtr);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  class ReassignmentAST : public ExprAST {
    VariableAST lhs_;
    SafeExprPtr rhs_;
  public:
    ReassignmentAST(const VariableAST &, SafeExprPtr);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  class CodeBlockAST : public ExprAST {
    std::list<SafeExprPtr> exprs_ {};
  public:
    CodeBlockAST(void);
    CodeBlockAST(SafeExprPtr);
    CodeBlockAST(std::initializer_list<SafeExprPtr>);

    llvm::Value * codegen(CodeGenContext &) noexcept override;

    void push(SafeExprPtr) noexcept;
    ;
  };

  // ++ --
  enum class UnaryOperator {
    PreAddOperator,
    PreSubOperator,
    PostAddOperator,
    PostSubOperator,
  };

  class UnaryOperatorAST : public ExprAST {
    UnaryOperator op_;
    VariableAST var_;
  public:
    UnaryOperatorAST(UnaryOperator,
                     VariableAST &);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  enum class BinaryOperator {
    Add,
    Sub,
    Mul,
    Div
  };

  class BinaryOperatorAST : public ExprAST {
    BinaryOperator op_;
    SafeExprPtr lhs_, rhs_;
    bool hasFP;
  public:
    BinaryOperatorAST(BinaryOperator, SafeExprPtr, SafeExprPtr, bool);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

} // ns ast
} // ns satisfy

#endif // SATISFY_AST_HPP
