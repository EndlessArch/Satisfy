#ifndef SATISFY_DECL_DECL_HPP
#define SATISFY_DECL_DECL_HPP

#include <memory>

#include <llvm/IR/Instruction.h>

class Decl;

class Decl {
public:
  virtual ~Decl(void) = default;

  void * getDeclLinkageType(void) noexcept;
};

#endif // SATISFY_DECL_DECL_HPP