#ifndef SATISFY_TMP_HPP
#define SATISFY_TMP_HPP

#include <array>
#include <utility>
#include <variant>

template <std::size_t N>
class __carray {
  std::array<int, N> arr_;
public:
  template <typename... Args>
  constexpr
  __carray(Args... args)
    : arr_{args...} {}

  constexpr
  std::size_t size() const noexcept {
    return N;
  }

  constexpr
  auto get() const noexcept {
    return arr_;
  }
};

template <typename T, std::size_t N, std::size_t... Seqs>
static constexpr
auto __get_array(T (&arr)[N],
                 std::index_sequence<Seqs...>) noexcept {
  return __carray<N>(arr[Seqs]...);
}

template <typename T, std::size_t N>
static constexpr
auto getArray(T (&arr)[N]) noexcept {
  return __get_array<T, N>(arr, std::make_index_sequence<N>());
}

////

template <class... Ts>
struct overload_t : Ts... {
  using Ts::operator()...;
};

template <class... Ts> overload_t(Ts &...) -> overload_t<Ts &...>;

#endif // SATISFY_TMP_HPP
