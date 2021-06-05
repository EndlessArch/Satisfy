#ifndef SATISFY_AST_HPP
#define SATISFY_AST_HPP

#include <initializer_list>
#include <list>
#include <string>
#include <memory>
#include <optional>
#include <vector>
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
;
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

  // class AssignmentAST;
  class VariableAST;

  bool isFloatingPoint(llvm::LLVMContext &, VariableAST &) noexcept;

  /////

  class ReassignmentAST;
  class UnaryOperatorAST;
  class FunctionAST;

  /////

  class IdentifierAST : public AST {
  public:
    std::string idName_;
  // public:
    IdentifierAST(void);
    IdentifierAST(const std::string &);
    llvm::Value * codegen(CodeGenContext &) noexcept override;

    // friend llvm::Type* getTypeOf(llvm::LLVMContext&,
    //                              const IdentifierAST&) noexcept;
    // friend class AssignmentAST;
    // friend class VariableAST;
  };

  using SafeExprPtr = std::shared_ptr<ExprAST>;

  class NumberAST : public AST {
    IdentifierAST targetType_;
    llvm::Type * typePtr_ = nullptr;
    satisfy::parser::value_type val_;
  public:
    NumberAST(llvm::Type *, parser::value_type);
    NumberAST(IdentifierAST, parser::value_type);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  class VariableAST : public AST {
  public:
    IdentifierAST varType_;
    std::string varName_;
  private:
    // assignment expression
    SafeExprPtr varAssign_;
  public:
    // VariableAST() = delete;
    VariableAST(std::string);
    VariableAST(const IdentifierAST &, std::string);
    VariableAST(const IdentifierAST &, std::string, SafeExprPtr);
    llvm::Value * codegen(CodeGenContext &) noexcept override;

    void setType(llvm::Type *) noexcept;
   private:
    llvm::Type * compileTimeType_;

    // friend bool isFloatingPoint(llvm::LLVMContext&, VariableAST&) noexcept;
    // friend class ReassignmentAST;
    // friend class UnaryOperatorAST;
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

  class CodeAST : public ExprAST {
   protected:
    std::list<SafeExprPtr> exprs_;
  public:
    CodeAST(void);
    CodeAST(SafeExprPtr);
    CodeAST(std::initializer_list<SafeExprPtr>);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
    void push(SafeExprPtr) noexcept;

    inline
    std::size_t getExprSize(void) noexcept {
      return exprs_.size();
    }
  };
  
  class CodeBlockAST : public CodeAST {
    std::string blockName_;
  public:
    CodeBlockAST(std::string = "");
    CodeBlockAST(SafeExprPtr, std::string = "");
    CodeBlockAST(std::initializer_list<SafeExprPtr>, std::string = "");

    llvm::Value * codegen(CodeGenContext &) noexcept override;

    void push(SafeExprPtr) noexcept;
    void setBlockName(const std::string &) noexcept;
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
    Div,

    Set,
  };

  class BinaryOperatorAST : public ExprAST {
    BinaryOperator op_;
    SafeExprPtr lhs_, rhs_;
  public:
    BinaryOperatorAST(std::string, SafeExprPtr, SafeExprPtr);
    BinaryOperatorAST(BinaryOperator, SafeExprPtr, SafeExprPtr);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  class FunctionAST : public ExprAST {
    std::string funcName_;
    IdentifierAST retType_;
    std::vector<VariableAST> params_;
    std::optional<CodeBlockAST> cb_;

  public:
    FunctionAST(const std::string &,
                const IdentifierAST &,
                const std::vector<VariableAST> &,
                const std::optional<CodeBlockAST> &);
    llvm::Value * codegen(CodeGenContext &) noexcept override;

    // inline IdentifierAST
    // getFuncRetType(void) noexcept {
    //   return retType_;
    // }
  };

  class ReturnAST : public ExprAST {
    SafeExprPtr ret_;

  public:
    ReturnAST(SafeExprPtr);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

class IfAST : public ExprAST {
  SafeExprPtr if_;
  CodeBlockAST then_;
  CodeBlockAST else_;
 public:
  IfAST(SafeExprPtr, CodeBlockAST);
  void pushElse(CodeBlockAST &) noexcept;
  // void pushElse(SafeExprPtr) noexcept;

  llvm::Value * codegen(CodeGenContext &) noexcept;
};



} // ns ast
} // ns satisfy

#endif // SATISFY_AST_HPP
