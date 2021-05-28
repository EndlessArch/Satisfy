#ifndef SATISFY_CODEGEN_HPP
#define SATISFY_CODEGEN_HPP

#include <map>
#include <stack>
#include <string>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Function.h>

namespace satisfy {
namespace codegen {
  
  struct CodeGenBlock {
    llvm::BasicBlock * bb_;
    // something locals
    std::map<std::string, llvm::Value *> local_;
  };

  class CodeGenContext {
    std::stack<struct CodeGenBlock *> blocks_;
    llvm::Function * mainFunction_;
  public:

    void generateCode(void) noexcept;

    std::map<std::string, llvm::Value *> & getLocal(void) noexcept;

    llvm::BasicBlock * currentBlock() noexcept;

    void pushBlock(llvm::BasicBlock *) noexcept;
    void popBlock(void) noexcept;
  };

} // ns codegen
} // ns satisfy

#endif // SATISFY_CODEGEN_HPP
