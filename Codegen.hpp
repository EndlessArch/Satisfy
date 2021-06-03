#ifndef SATISFY_CODEGEN_HPP
#define SATISFY_CODEGEN_HPP

#include <deque>
#include <map>
#include <memory>
#include <optional>
#include <string>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Function.h>

#include "AST.hpp"

namespace satisfy {
namespace codegen {
  
  llvm::Type *
  getBasicInt(llvm::LLVMContext &) noexcept;

  struct CodeGenBlock {
    llvm::BasicBlock * bb_;
    // something locals
    std::map<std::string, llvm::Value *> local_;

    CodeGenBlock(llvm::BasicBlock *);
  };

  class CodeGenContext {
    std::deque<std::unique_ptr<struct CodeGenBlock>> blocks_;
    llvm::Function * mainFunction_;
    llvm::LLVMContext llvmCtx_;
    llvm::Module llvmModule_;
    llvm::IRBuilder<> llvmBuilder_;
    llvm::FunctionPassManager theFPM_;
    
  public:

    CodeGenContext();
    ~CodeGenContext();

    void generateCode(ast::CodeAST &) noexcept;

    inline
    bool isOnBaseCode(void) noexcept {
      return blocks_.empty();
    }

    std::map<std::string, llvm::Value *> & getLocal(void) noexcept;
    std::optional<llvm::Value *>
    searchForVariable(const std::string &) noexcept;

    llvm::BasicBlock * currentBlock() noexcept;

    void pushBlock(llvm::BasicBlock *) noexcept;
    void popBlock(void) noexcept;

    llvm::LLVMContext & getLLVMCtx(void) noexcept;
    llvm::Module & getModule(void) noexcept;
    llvm::IRBuilder<> & getBuilder(void) noexcept;
    llvm::FunctionPassManager & getFPM(void) noexcept;
  };

} // ns codegen
} // ns satisfy

#endif // SATISFY_CODEGEN_HPP
