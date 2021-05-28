#include "AST.hpp"

#include "Error.hpp"

#include <llvm/IR/Instructions.h>

namespace satisfy {
namespace ast {

  llvm::Value *
  IdentifierAST::codegen(CodeGenContext & _cgc) noexcept {
    auto & loc = _cgc.getLocal();
    auto cit = loc.find(idName_);
    if(cit == loc.end()) {
      err("Undeclared identifier, \'" + idName_ + "\'");
      return nullptr;
    }
    return new llvm::LoadInst();
  }

  llvm::Value *
  VariableAST::codegen(CodeGenContext & _cgc) noexcept {
    ;
  }

  CodeBlockAST::CodeBlockAST(const SafeExprPtr _sep) {
    exprs_.push_back(_sep);
  }

  CodeBlockAST::CodeBlockAST(std::initializer_list<const SafeExprPtr> _il)
    : exprs_(_il) {
    ;
  }

  llvm::Value * CodeBlockAST::codegen(CodeGenContext & _cgc) noexcept {
    return nullptr;
  }

} // ns ast
} // ns satisfy
