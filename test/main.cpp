#include "extend.hpp"
#include "extend/pretty_macros/define.hpp"
#include "extend/dollar_macros/define.hpp"

#include <tuple>
#include <cassert>

namespace func {
    xtd_function id;
    xtd_function square;
}

impl_try_method(func::id, id)
impl_try_method(func::square, square)

constexpr auto impl(func::id)(const auto&) { return 0; }
constexpr auto impl(func::square)(const auto&) { return 0; }

namespace numbers {
    struct One {
        using this_t = One;
        template<class T> constexpr auto impl_this(func::id)(T&&) { return 1; }
        template<class T> constexpr auto impl_this(func::square)(T&&) { return 1; }
    };

    struct Two {
        using this_t = Two;
        template<class T> constexpr auto impl_this(func::id)(T&&) { return 2; }
        template<class T> constexpr auto impl_this(func::square)(T&&) { return 4; }
    };

    struct Three {
        using this_t = Three;
        template<class T> constexpr auto impl_this(func::id)(T&&) { return 3; }
    };

    constexpr auto impl(func::id)(const Three&) { return -3; }
    constexpr auto impl(func::square)(const Three&) { return 9; }

    struct Four {};

    template<class T> requires (!std::same_as<std::decay_t<T>, Three>)
    constexpr auto impl(func::square)(T&&) { return 16; }

    struct Five {
        constexpr auto id() const { return 5; }
    };

    struct Six {
        using this_t = Six;
        template<class T> constexpr auto impl_this(func::square)(T&&) { return 36; }
        using_impl_2(func, square)
    };
}

xtd_function size;

template<class... T>
constexpr std::size_t impl(size)(const std::tuple<T...>&) {
    return sizeof...(T);
}

template<class T>
struct index_of_t : xtd::tagged_bindable<index_of_t<T>> {};

template<class T>
static constexpr index_of_t<T> index_of;

template<class...>
struct tt {};

template<class T, class X, class... Xs>
constexpr std::size_t impl_tag(index_of_t<T>) (const std::tuple<X, Xs...>&) {
    return index_of<T>(tt<X, Xs...>{});
}

template<class T, class X, class... Xs>
constexpr std::size_t impl_tag(index_of_t<T>) (tt<X, Xs...>) {
    return 1 + index_of<T>(tt<Xs...>{});
}

template<class T, class... Xs>
constexpr std::size_t impl_tag(index_of_t<T>) (tt<T, Xs...>) {
    return 0;
}

template<class T>
constexpr std::size_t impl_tag(index_of_t<T>) (tt<>) {
    return 0;
}

template<class T>
struct exists_t : xtd::tagged_bindable<exists_t<T>> {};

template<class T>
static constexpr exists_t<T> exists;

template<class T, class U>
constexpr bool impl_tag(exists_t<T>)(U const& u) {
    return index_of<T>(u) < size(u);
}

xtd_invoker(scope_name)

struct interface {
    int apply(int);
    std::string_view apply(double);
};

xtd_function_(scope_name, interface) test_scope;

constexpr int impl_(scope_name, test_scope)(int) {
    return 0;
}
constexpr auto impl_(scope_name, test_scope)(double) {
    return "";
}

static_assert(test_scope(0) == 0);
static_assert(test_scope(1.0).empty());

int main() {
    using namespace xtd::literals;

    static_assert(1 == std::tuple(1, 2.0) $$ index_of<double> (_));
    static_assert(false == std::tuple(1, 2.0) $$ exists<float>(_));

    numbers::One one;
    numbers::Two two;
    numbers::Three three;
    numbers::Four four;
    numbers::Five five;
    numbers::Six six;

    using func::id;
    using func::square;

    // Normal method call
    static_assert(id(one) == 1);

    // UFCS syntax sugar
    static_assert(one $$ id(_) == 1);

    // Other alternatives (look ugly and verbose with zero args)
    static_assert(one $$ id(0_) == 1);
    static_assert(one $(id)() == 1);

    static_assert(two $$ id(_) == 2);
    static_assert(three $$ id(_) == 3);
    static_assert(four $$ id(_) == 0);
    static_assert(five $$ id(_) == 5);
    static_assert(five.id() == 5);
    static_assert(six $$ id(_) == 0);

    static_assert(one $$ square(_) == 1);
    static_assert(two $$ square(_) == 4);
    static_assert(three $$ square(_) == 9);
    static_assert(four $$ square(_) == 16);
    static_assert(five $$ square(_) == 16);
    static_assert(six.square() == 36);
    static_assert(six $$ square(_) == 36);

    constexpr auto times = xtd::bindable { [](auto&& x, auto&& y) { return x $$ id(_) * y $$ id(_); } };
    constexpr auto minus = xtd::bindable { [](auto&& x, auto&& y) { return x $$ id(_) - y $$ id(_); } };

    static_assert(two $$ minus(_, three) == -1);
    // When two becomes the second arg then it's 3 - 2
    static_assert(two $$ minus(three, _) == 1);

    // Infix-style operator looks quite nice for N >= 1 args
    static_assert((two |times| three) == 6);
    static_assert(two  $(times) (three) == 6);
    static_assert(two $1(times) (three) == 6);

    static_assert(two  $(minus) (three) == -1);
    static_assert(two $$ minus(0_, three) == -1);

    static_assert(two $$ minus (1_, three) == 1);
    static_assert(two $1(minus) (three) == 1);

    constexpr auto plus = xtd::bindable { [](auto&& x, auto&& y) { return x + y; } };

    // Can build up a chain of bindables to be invoked later
    auto size_plus_2 = size(_) $$ plus.capture(_, 2);
    // auto size_plus_2 = size(_) $$ plus(_, 2); // compiler error: must capture the 2

    assert(5 == std::tuple(0, 3, 4) $$ size_plus_2);

    return 0;
}
