#include "decl/DeclQueue.hpp"

Decl &&
DeclQueue::pop(void) noexcept {
  auto first = this->declQueue_.front();
  this->declQueue_.pop();
  return std::move(first);
}

void
DeclQueue::clearQueue(void) noexcept {
  this->declQueue_ = {};
}