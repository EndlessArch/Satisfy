#ifndef SATISFY_TYPE_HPP
#define SATISFY_TYPE_HPP

#include <string>
#include <memory>

class Type;

using SafeType_t = typename std::shared_ptr<Type>;

class Type {
  ;
};

class GlobalTypes {

  ;
  
 public:

  bool isDefinedType(std::string) const noexcept;
  
};

#endif // SATISFY_TYPE_HPP
