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

enum class ASTResultType {
  Unknown = -1,
  Number = -2,
  TypeUnspecified = -3,
  Typed = -4
};

  class AST {
  public:
    virtual ~AST() = default;

    // Hate virtual functions,
    // but can't help with CRTP, so..
    virtual llvm::Value * codegen(CodeGenContext &) noexcept = 0;

    virtual ASTResultType expectedType(void) noexcept {
      return ASTResultType::Unknown;
    };
  };

  class ExprAST : public AST {};

  /////

  inline llvm::Type *
  returnTypeOf(llvm::Type * ty) noexcept {
    return ty;
  }

  llvm::Type *
  getTypeOf(llvm::LLVMContext &, const std::string &) noexcept;

  /////

  // class AssignmentAST;
  class VariableAST;

  /////

  class ReassignmentAST;
  class UnaryOperatorAST;
  class FunctionAST;

  /////

  using SafeExprPtr = std::shared_ptr<ExprAST>;

  class NumberAST : public AST {
    std::string targetType_;
    llvm::Type * typePtr_ = nullptr;
    satisfy::parser::value_type val_;
  public:
    NumberAST(llvm::Type *, parser::value_type);
    NumberAST(std::string, parser::value_type);
    llvm::Value * codegen(CodeGenContext &) noexcept override;

    ASTResultType expectedType(void) noexcept override;
  };

  class VariableAST : public AST {
  public:
    std::string varType_;
    std::string varName_;
  private:
    // assignment expression
    SafeExprPtr varAssign_;
  public:
    // VariableAST() = delete;
    VariableAST(const std::string &);
    VariableAST(const std::string &, const std::string &);
    VariableAST(const std::string &, const std::string &, SafeExprPtr);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
    ASTResultType expectedType(void) noexcept override;

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
    IncOperator,
    DecOperator
  };

  class UnaryOperatorAST : public ExprAST {
    UnaryOperator op_;
    VariableAST var_;
  public:
    UnaryOperatorAST(const std::string &, const VariableAST &);
    UnaryOperatorAST(UnaryOperator,
                     const VariableAST &);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  enum class BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,

    Set,

    EQ,
    NE,

    GT,
    GE,

    LT,
    LE,
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
    std::string retType_;
    std::vector<VariableAST> params_;
    std::optional<CodeBlockAST> cb_;

  public:
    FunctionAST(const std::string &,
                const std::string &,
                const std::vector<VariableAST> &,
                const std::optional<CodeBlockAST> &);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  class ReturnAST : public ExprAST {
    SafeExprPtr ret_;

  public:
    ReturnAST(SafeExprPtr);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };
;

class CallAST : public ExprAST {
  std::string func_;
  std::vector<VariableAST> args_;
 public:
  CallAST(std::string, std::vector<VariableAST>);
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

  llvm::Value * codegen(CodeGenContext &) noexcept override;
};

enum class ForE {
  ByNumber,
  ByBoolVar,
  ByExplicit
};

class ForAST : public ExprAST {
  ForE forE_;
  SafeExprPtr comp_[4];
public:
  ForAST(SafeExprPtr, SafeExprPtr);
  ForAST(SafeExprPtr, SafeExprPtr, SafeExprPtr, SafeExprPtr);
  llvm::Value * codegen(CodeGenContext &) noexcept override;
};

} // ns ast
} // ns satisfy

#endif // SATISFY_AST_HPP
