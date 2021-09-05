# extend
Customisation points to extend the functionality of any class. Automatic UFCS / pipeline syntax.

Single header file.

## Example usage:

See [iter](https://github.com/NasalDaemon/iter) library.

```c++
#include "extend.hpp"

namespace ns {
    XTD_FUNCTION minus; // Define customisation point ns::minus
}

// Default implementation in root namespace
template<class T, class U>
constexpr auto XTD_IMPL(ns::minus) (T t, U u) {
    return t - u;
}
```
All of the following expressions are equivalent:
```c++
using namespace xtd::literals; // import "_" placeholder

ns::minus(5, 3);     // 2
5 | ns::minus(_, 3); // 2
3 | ns::minus(5, _); // 2
5 | ns::minus | 3;   // 2
```
Customisation point can be specialised for classes that you own:
```c++
template<class T>
struct owned_class {
    using this_t = owned_class; // this_t needed for XTD_IMPL_THIS
    T data[10];

    template<class T, class U>
    constexpr auto XTD_IMPL_THIS(ns::minus) (T&& self, owned_class const& other) {
        T result = 0;
        for (size_t i = 0; i < 10; ++i) {
            result += self.data[i] - other.data[i];
        }
        return result;
    }
}
```

Or even, and especially, classes that you don't:
```c++
template<class T, std::size_t N>
constexpr auto XTD_IMPL(ns::minus) (std::array<T, N> const& t, std::array<T, N> const& u) {
    T result = 0;
    for (size_t i = 0; i < N; ++i) {
        result += t[i] - u[i];
    }
    return result;
}
```

## Overload order of precedence

For any expression equivalent to `ns::func(another::namspace::A)`:

1. Applicable `XTD_IMPL_THIS(ns::func)` definition inside class ::another::namspace::A
1. Applicable `XTD_IMPL(ns::func)` definiton inside namespace ::another::namspace
1. Applicable `XTD_IMPL(ns::func)` definiton inside namespace ::another
1. Applicable `XTD_IMPL(ns::func)` definiton inside namespace :: (root namespace)

Which is a much more powerful method of providing customisation points than simple ADL, which only checks 1. and 2.

Due to 3. and 4., it is possible to create generic functions that apply to standard library classes while still having fully qualified functions to remove any ambiguity.
