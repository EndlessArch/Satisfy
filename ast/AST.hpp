#ifndef SATISFY_AST_AST_HPP
#define SATISFY_AST_AST_HPP

#include <memory>

class AST;

using SafeAST_t = typename std::shared_ptr<AST>;

class AST {
public:
  virtual ~AST(void) = default;

  virtual void * getType(void) noexcept = 0;

  virtual SafeAST_t genCode(void) noexcept = 0;
};

#endif // SATISFY_AST_AST_HPP