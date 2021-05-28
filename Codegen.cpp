#include "Codegen.hpp"

namespace satisfy {
namespace codegen {

  void CodeGenContext::generateCode(void) noexcept {

    ;

    return;
  }

  std::map<std::string, llvm::Value *> &
  CodeGenContext::getLocal(void) noexcept {
    return blocks_.top()->local_;
  }

} // ns codegen
} // ns satisfy
