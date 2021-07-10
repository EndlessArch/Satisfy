#include "Pos.hpp"

Pos::Pos(void) {
  row = 0;
  col = 1;
}

void Pos::updateRow(unsigned i) noexcept { this->row += i; }

void Pos::updateCol(void) noexcept {
  this->row = 0;
  ++this->col;
}

unsigned Pos::getRow(void) noexcept { return this->row; }

unsigned Pos::getCol(void) noexcept { return this->col; }
