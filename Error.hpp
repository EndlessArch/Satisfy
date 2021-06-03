#ifndef SATISFY_ERROR_HPP
#define SATISFY_ERROR_HPP

#include <string>

namespace satisfy {

  namespace error {
    
    static unsigned _row = 0, _col = 0;

  } // ns error

  void updateRow(void) noexcept;
  void updateLine(void) noexcept;

void printErr(const char *) noexcept;

void printErr(std::string) noexcept;

  void printErr(const char *, int, const char *, std::string) noexcept;

} // ns satisfy

#define err(x) printErr(__FILE__, __LINE__, __func__, x)

/*
  as err() is macro, and also uses macros,
  functions which involved to it must be macro.
*/
#define err_expr_unexpected() err("Unexpected expression")
#define err_idf_expected() err("Expected identifier")
#define err_ie_expected() err("Expected identifier or expression")
// #define 

#endif // SATISFY_ERROR_HPP
