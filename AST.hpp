#ifndef SATISFY_AST_HPP
#define SATISFY_AST_HPP

#include <initializer_list>
#include <list>
#include <string>
#include <memory>
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

  class CodeBlockAST : public ExprAST {
    std::string blockName_;
    llvm::Function * parent_;
    llvm::BasicBlock * bb_;
    std::list<SafeExprPtr> exprs_ {};
  public:
    CodeBlockAST(llvm::BasicBlock *, llvm::Function * = nullptr);
    CodeBlockAST(std::string = "", llvm::Function * = nullptr);
    CodeBlockAST(SafeExprPtr, std::string = "", llvm::Function * = nullptr);
    CodeBlockAST(std::initializer_list<SafeExprPtr>, std::string = "", llvm::Function * = nullptr);

    llvm::Value * codegen(CodeGenContext &) noexcept override;

    void push(SafeExprPtr) noexcept;
    void setBlockName(const std::string &) noexcept;
    void setBlock(llvm::BasicBlock *) noexcept;

    inline
    std::size_t getExprSize() const noexcept {
      return exprs_.size();
    }

    inline
    llvm::Function * getParent() noexcept {
      return parent_;
    }
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

  class FunctionAST : public ExprAST {
    std::string funcName_;
    IdentifierAST retType_;
    std::vector<VariableAST> params_;
    CodeBlockAST cb_;

  public:
    FunctionAST(const std::string &,
                const IdentifierAST &,
                const std::vector<VariableAST> &,
                const CodeBlockAST & = NULL);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
    inline void setCodeBody(const CodeBlockAST & cb) noexcept {
      cb_ = cb;
    }

    inline IdentifierAST
    getFuncRetType(void) noexcept {
      return retType_;
    }
  };

  class ReturnAST : public ExprAST {
    std::variant<VariableAST, NumberAST> ret_;

  public:
    template <typename AST>
    ReturnAST(AST && ast)
      : ret_(std::forward<AST>(ast)) {
    }
    llvm::Value * codegen(CodeGenContext &) noexcept override;
  };

  class CodeAST : public ExprAST {
    std::list<SafeExprPtr> exprs_;
  public:
    CodeAST(void);
    CodeAST(SafeExprPtr);
    CodeAST(std::initializer_list<SafeExprPtr>);
    llvm::Value * codegen(CodeGenContext &) noexcept override;
    void push(SafeExprPtr) noexcept;
  };

} // ns ast
} // ns satisfy

#endif // SATISFY_AST_HPP
