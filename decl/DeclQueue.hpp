#ifndef SATISFY_DECLQUEUE_HPP
#define SATISFY_DECLQUEUE_HPP

#include <queue>

#include "decl/Decl.hpp"
class DeclQueue final {

  std::queue<Decl> declQueue_;

public:

  Decl && pop(void) noexcept;

  void clearQueue(void) noexcept;

};

#endif // SATISFY_DECLQUEUE_HPP