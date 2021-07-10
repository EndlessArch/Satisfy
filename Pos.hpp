#ifndef SATISFY_POS_HPP
#define SATISFY_POS_HPP

class Pos {
  unsigned row, col;

 public:
  
  Pos(void);

  void updateRow(unsigned = 1) noexcept;

  void updateCol(void) noexcept;

  unsigned getRow(void) noexcept;

  unsigned getCol(void) noexcept;
};

#endif // SATISFY_POS_HPP
