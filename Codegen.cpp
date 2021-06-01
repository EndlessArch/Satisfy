#include "Codegen.hpp"

#include <iostream>
#include <vector>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>

#include <llvm/Transforms/Utils/PromoteMemToReg.h>

namespace satisfy {
namespace codegen {

  llvm::Type *
  getBasicInt(llvm::LLVMContext & _ctx) noexcept {
    switch(sizeof(int)) {
    case 4:
      return llvm::Type::getInt32Ty(_ctx);

    case 8:
      return llvm::Type::getInt64Ty(_ctx);

    }

    return nullptr;
  }

  CodeGenBlock::CodeGenBlock(llvm::BasicBlock * _bb)
    : bb_(_bb) {
    ;
  }

  CodeGenContext::CodeGenContext()
    : llvmModule_("mainModule",
                  llvmCtx_),
      llvmBuilder_(llvmCtx_),
      theFPM_({}) {
    // theFPM_.addPass(llvm::PromoteMemToReg);
    // mac
    llvmModule_.setTargetTriple("x86_64-apple-darwin20.4.0");
  }

  void CodeGenContext::generateCode(satisfy::ast::CodeBlockAST & mainBlock) noexcept {

    std::cout << "Generating code...\n";

    std::vector<llvm::Type *> argTys;
    llvm::FunctionType * ft
      = llvm::FunctionType::get(getBasicInt(llvmCtx_),
                                argTys,
                                false);

    mainFunction_ = llvm::Function::Create(ft,
                                           llvm::GlobalValue::ExternalLinkage,
                                           "main",
                                           llvmModule_);

    llvm::BasicBlock * bb = llvm::BasicBlock::Create(llvmCtx_,
                                                     "entry",
                                                     mainFunction_); // parent

    llvmBuilder_.SetInsertPoint(bb);

    pushBlock(bb);

    mainBlock.codegen(*this);

    popBlock();

    llvmBuilder_.CreateRet(llvm::ConstantInt::get(getBasicInt(llvmCtx_),
                                                  llvm::APInt(sizeof(int) * 8, 0, true)));

    std::cout << "Generated code.\n";

    return;
  }

  std::map<std::string, llvm::Value *> &
  CodeGenContext::getLocal(void) noexcept {
    return blocks_.top()->local_;
  }

  llvm::BasicBlock *
  CodeGenContext::currentBlock(void) noexcept {
    return blocks_.top()->bb_;
  }

  void CodeGenContext::pushBlock(llvm::BasicBlock * _bb) noexcept {
    llvmBuilder_.SetInsertPoint(_bb);
    blocks_.push(std::make_unique<struct CodeGenBlock>(_bb));
  }

  void CodeGenContext::popBlock(void) noexcept {
    // remind it's unique-ptr
    blocks_.pop();
    if(blocks_.size())
      llvmBuilder_.SetInsertPoint(blocks_.top().get()->bb_);
  }

  llvm::LLVMContext &
  CodeGenContext::getLLVMCtx(void) noexcept {
    return llvmCtx_;
  }

  llvm::Module &
  CodeGenContext::getModule(void) noexcept {
    return llvmModule_;
  }

  llvm::IRBuilder<> &
  CodeGenContext::getBuilder(void) noexcept {
    return llvmBuilder_;
  }

} // ns codegen
} // ns satisfy
