#ifndef EXTEND_INCLUDE_EXTEND_HPP
#define EXTEND_INCLUDE_EXTEND_HPP

#include <type_traits>
#include <concepts>
#include <functional>
#include <string_view>

#ifndef FWD
#  define FWD(arg) static_cast<decltype(arg)&&>(arg)
#endif

inline namespace xtd_adl {

struct xtd_free_t {};
template<class... T> struct xtd_method_t;
template<> struct xtd_method_t<> {};
template<class T> struct xtd_method_t<T> : xtd_method_t<> {
    constexpr xtd_method_t(xtd_method_t<>){}
    constexpr xtd_method_t() = default;
};

}

namespace {
static constexpr bool xtd_invoker_defined_in_root_namespace = false;
}

namespace xtd {

namespace detail {
    template<auto U = []{}>
    using unique_t = decltype(U);
}

namespace invokers {
    static constexpr bool xtd_invoker_defined_in_root_namespace = true;

    struct main {
        template<class... Ts>
        static constexpr auto invoke(Ts&&... in)
            noexcept(noexcept(xtd_invoke_main(FWD(in)...)))
            -> decltype(xtd_invoke_main(FWD(in)...))
        {
            return xtd_invoke_main(FWD(in)...);
        }
    };
}

template<
    class U = detail::unique_t<>,
    class Invoker = invokers::main,
    class Interface = void,
    class... Fs>
struct bindable;

template<size_t I = 0>
struct bind_placeholder {
    static constexpr std::size_t index = I;
};

template<class F, std::size_t Arity = 0, bool Owner = false>
struct bound;

template<class F, bool O>
struct [[nodiscard]] bound<F, 0, O> : F {
    static constexpr std::size_t arity = 0;
    static constexpr bool owner = O;
};

template<size_t A = 0, bool O = false, class F>
static constexpr bound<std::decay_t<F>, A, O> make_bound(F&& f) {
    return { FWD(f) };
}

namespace concepts {
    template<class>
    static constexpr bool is_bindable = false;

    template<class U, class... Fs>
    inline constexpr bool is_bindable<bindable<U, Fs...>> = true;

    template<class T>
    concept Bindable = is_bindable<typename std::decay_t<T>::bindable_t>;

    template<class T>
    struct check_untagged {
        static constexpr bool value = !std::decay_t<T>::is_tagged();
        static_assert(value, "Must use impl_tag/impl_tag_this");
    };

    template<class T>
    concept UntaggedBindable = Bindable<T> && check_untagged<T>::value;

    template<class T>
    concept SpecializableBindable = Bindable<T> && std::decay_t<T>::is_specializable;

    template<class>
    static constexpr bool is_bind_placeholder = false;
    template<size_t I>
    inline constexpr bool is_bind_placeholder<bind_placeholder<I>> = true;

    template<class T>
    concept BindPlaceholder = is_bind_placeholder<std::decay_t<T>>;

    template<class T, std::size_t I>
    concept BindPlaceholderAt = BindPlaceholder<T> && std::decay_t<T>::index == I;

    template<class CP, class... Ts>
    concept CustomisedFree = requires (const CP& bindable, Ts&&... in) {
        CP::invoker_t::invoke(xtd_free_t{}, bindable.self(), FWD(in)...);
    };
    template<class CP, class... Ts>
    concept CustomisedMethod = requires (const CP& bindable, Ts&&... in) {
        CP::invoker_t::invoke(xtd_method_t<>{}, bindable.self(), FWD(in)...);
    };
    template<class CP, class... Ts>
    concept Customised = CustomisedMethod<CP, Ts...> || CustomisedFree<CP, Ts...>;

    template<class T, class R>
    concept CompatInterface = std::constructible_from<R, T>;

    template<class CP, class... Ts>
    concept AllowedFree = requires (const CP& bindable, Ts&&... in) {
        { CP::invoker_t::invoke(xtd_free_t{}, bindable.self(), FWD(in)...) } ->
            CompatInterface<decltype(typename CP::interface_t{}.apply(FWD(in)...))>;
    };

    template<class CP, class... Ts>
    concept AllowedMethod = requires (const CP& bindable, Ts&&... in) {
        { CP::invoker_t::invoke(xtd_method_t<>{}, bindable.self(), FWD(in)...) } ->
            CompatInterface<decltype(typename CP::interface_t{}.apply(FWD(in)...))>;
    };

    template<class T>
    static constexpr bool is_bound = false;
    template<class F, std::size_t A, bool O>
    inline constexpr bool is_bound<bound<F, A, O>> = true;

    template<class T, std::size_t A = 0>
    concept Bound = is_bound<std::decay_t<T>> && std::decay_t<T>::arity == A;
}

template<auto& obj>
requires (concepts::UntaggedBindable<decltype(obj)> && concepts::SpecializableBindable<decltype(obj)>)
using tag_of = const std::remove_cvref_t<decltype(obj)>&;

template<class L, concepts::Bound R>
requires (!concepts::Bound<L>)
constexpr decltype(auto) operator ->* (L&& l, const R& r) {
    return r(FWD(l));
}

template<class L, concepts::Bindable R>
requires (!concepts::Bound<L>)
constexpr decltype(auto) operator ->* (L&& l, const R& r) {
    return make_bound([&](auto&&... in) {
        return r(FWD(l), FWD(in)...);
    });
}

template<concepts::Bound L, concepts::Bound R>
constexpr auto operator ->* (L&& l, R&& r) {
    static_assert(std::decay_t<L>::owner && std::decay_t<R>::owner,
        "When building a chain of bindables that is not immediately invoked, "
        "please call bindable.capture(...) instead of bindable(...)");
    return make_bound<0, true>(
        [l = FWD(l), r = FWD(r)](auto&& in) {
            return l(FWD(in)) ->* r; });
}

template<concepts::Bound L, class R>
requires (!concepts::Bound<R>)
constexpr decltype(auto) operator ->* (const L& l, R&& r) {
    return l(FWD(r));
}

template<concepts::Bound L>
constexpr decltype(auto) operator ->* (const L& l, bind_placeholder<>) {
    return l();
}

constexpr decltype(auto) operator | (auto&& l, auto&& r) {
    return FWD(l) ->* FWD(r);
}

namespace detail {
    template<class... Fs>
    struct overload : Fs... {
        using Fs::operator()...;
    };
    template<class... Fs>
    overload(Fs...) -> overload<Fs...>;

    template<class R>
    constexpr decltype(auto) select(auto&& l, R&& r) {
        if constexpr (concepts::BindPlaceholder<R>)
            return FWD(l);
        else
            return FWD(r);
    }

    template<std::size_t>
    struct sink {
        constexpr sink(auto&&...) {}
    };

    template<std::size_t... Is>
    constexpr decltype(auto) get_value(sink<Is>..., auto&& value, auto&&...) {
        return FWD(value);
    }

    template<std::size_t I, class... Ts>
    constexpr decltype(auto) get_i(Ts&&... args) {
        return [&]<std::size_t... Is>(std::index_sequence<Is...>) -> decltype(auto) {
            return get_value<(0*Is)...>(FWD(args)...);
        }(std::make_index_sequence<sizeof...(Ts)-1>{});
    }

    template<size_t BI, std::size_t I>
    constexpr decltype(auto) select_i(auto&& l, auto&&... r) {
        if constexpr (I == BI)
            return FWD(l);
        else if constexpr (I > BI)
            return get_i<I - 1>(FWD(r)...);
        else
            return get_i<I>(FWD(r)...);
    }
}

template<class U, class Invoker, class Interface, class... Fs>
struct bindable : detail::overload<Fs...> {
    using bindable_t = bindable;
    using invoker_t = Invoker;
    using interface_t = Interface;
    static constexpr bool is_tagged() { return std::is_base_of_v<bindable, U>; }
    static constexpr bool is_specializable = sizeof...(Fs) == 0;

    template<class... Ts>
    // This call operator is only enabled if it can be specialised
    requires (is_specializable && sizeof...(Ts) > 0)
          && (!concepts::BindPlaceholder<Ts> && ...)
          && concepts::Customised<bindable, Ts...>
    constexpr decltype(auto) operator()(Ts&&... ins) const {
        // Prioritise the method overload
        using dispatcher = std::conditional_t<concepts::CustomisedMethod<bindable, Ts...>, xtd_method_t<>, xtd_free_t>;
        if constexpr (std::is_same_v<void, Interface>)
            return invoker_t::invoke(dispatcher{}, self(), FWD(ins)...);
        else {
            if constexpr (concepts::CustomisedMethod<bindable, Ts...>)
                static_assert(concepts::AllowedMethod<bindable, Ts...>,
                    "No implementation for this parameter set follows the interface");
            else
                static_assert(concepts::AllowedFree<bindable, Ts...>,
                    "No implementation for this parameter set follows the interface");
            using return_t = decltype(Interface{}.apply(FWD(ins)...));
            if constexpr (std::is_same_v<void, return_t>)
                return invoker_t::invoke(dispatcher{}, self(), FWD(ins)...);
            else
                return return_t(invoker_t::invoke(dispatcher{}, self(), FWD(ins)...));
        }
    }

    template<class... Ts>
    // This call operator is only enabled if it has its own function calls
    requires (!is_specializable && sizeof...(Ts) > 0)
          && (!concepts::BindPlaceholder<Ts> && ...)
          && (std::invocable<Fs const, Ts...> || ...)
    constexpr decltype(auto) operator()(Ts&&... ins) const {
        return detail::overload<Fs...>::operator()(FWD(ins)...);
    }

    [[nodiscard]] constexpr auto operator()() const {
        return operator()(bind_placeholder<0>{});
    }

    template<class... Ts>
    requires (sizeof...(Ts) > 0) && (concepts::BindPlaceholderAt<Ts, 0> || ...)
    [[nodiscard]] constexpr auto operator()(Ts&&... in) const {
        // Being an owner is free if there is nothing to bind
        return capture<sizeof...(Ts) == 1>(FWD(in)...);
    }

    // Placeholder as first argument is common case
    template<bool Store = true, class... Ts>
    requires (!concepts::BindPlaceholder<Ts> && ...)
    [[nodiscard]] constexpr auto capture(const bind_placeholder<0>&, Ts&&... in) const {
        if constexpr (Store) {
            return make_bound<0, Store>([this, ...in = FWD(in)](auto&& l) {
                return (*this)(FWD(l), in...);
            });
        } else {
            return make_bound([&, this](auto&& l) {
                return (*this)(FWD(l), FWD(in)...);
            });
        }
    }

    // Placeholder in any position general case
    template<bool Store = true, class... Ts>
    requires (sizeof...(Ts) > 0) && (concepts::BindPlaceholderAt<Ts, 0> || ...)
    [[nodiscard]] constexpr auto capture(Ts&&... in) const {
        constexpr std::size_t count = ((concepts::BindPlaceholder<Ts> ? 1 : 0) + ...);
        static_assert(count == 1, "May specify only one placeholder");
        if constexpr (Store) {
            return make_bound<0, Store>([this, ...in = FWD(in)](auto&& l) {
                return (*this)(detail::select(FWD(l), in)...);
            });
        } else {
            return make_bound([&, this](auto&& l) {
                return (*this)(detail::select(FWD(l), FWD(in))...);
            });
        }
    }

    template<size_t I, class... Ts>
    requires (I > 0) && (!concepts::BindPlaceholder<Ts> && ...)
    [[nodiscard]] constexpr auto operator()(const bind_placeholder<I>&, Ts&&... ins) const {
        static_assert(I <= sizeof...(Ts), "Placeholder index is out of bounds");
        // Being an owner is free if there is nothing to bind
        return bind_n<I, sizeof...(Ts) == 0>(std::make_index_sequence<1 + sizeof...(Ts)>{}, FWD(ins)...);
    }

    template<size_t I, class... Ts>
    requires (I > 0) && (!concepts::BindPlaceholder<Ts> && ...)
    [[nodiscard]] constexpr auto capture(const bind_placeholder<I>&, Ts&&... ins) const {
        static_assert(I <= sizeof...(Ts), "Placeholder index is out of bounds");
        return bind_n<I, true>(std::make_index_sequence<1 + sizeof...(Ts)>{}, FWD(ins)...);
    }

    constexpr auto& self() const {
        if constexpr (is_tagged())
            return static_cast<U const&>(*this);
        else
            return *this;
    }

private:
    template<size_t BI, bool Store, std::size_t... Is>
    constexpr decltype(auto) bind_n(std::index_sequence<Is...>, auto&&... ins) const {
        if constexpr (Store) {
            return make_bound<0, Store>([this, ...ins = FWD(ins)](auto&& l) {
                return (*this)(detail::select_i<BI, Is>(FWD(l), ins...)...);
            });
        } else {
            return make_bound([&, this](auto&& l) {
                return (*this)(detail::select_i<BI, Is>(FWD(l), FWD(ins)...)...);
            });
        }
    }
};

template<class... F>
bindable(F...) -> bindable<void, invokers::main, void, F...>;

template<class Tag, class Invoker = xtd::invokers::main, class Interface = void>
using tagged_bindable = bindable<Tag, Invoker, Interface>;

constexpr auto apply(auto&& func) {
    return [func = FWD(func)](auto&& tuple) mutable {
        return apply(func, FWD(tuple));
    };
}

namespace literals {
    [[maybe_unused]] static constexpr bind_placeholder _;

    template<char... C>
    requires (sizeof...(C) == 1 && ((C >= '0') && ...) && ((C <= '9') && ...))
    constexpr auto operator ""_() {
        return bind_placeholder<(C - '0')...>{};
    }
}

} // namespace xtd

// OVERLOAD MACROS

#define XTD_FUNCTION static constexpr ::xtd::bindable<::xtd::detail::unique_t<>, ::xtd::invokers::main, void>

#define XTD_INVOKER(name) \
    namespace xtd::invokers {\
        static_assert(xtd_invoker_defined_in_root_namespace,\
            "Invokers must be declared in root namespace");\
        struct name {\
            template<class... Ts>\
            static constexpr auto invoke(Ts&&... in)\
                noexcept(noexcept(xtd_invoke_ ## name(FWD(in)...)))\
                -> decltype(xtd_invoke_ ## name(FWD(in)...))\
            {\
                return xtd_invoke_ ## name(FWD(in)...);\
            }\
        };\
    }

#ifdef _MSC_VER
#  define XTD_FUNCTION_(invoker, ... /*interface*/) \
    static constexpr ::xtd::bindable<::xtd::detail::unique_t<>, ::xtd::invokers::invoker, __VA_ARGS__>
#else
#  define XTD_FUNCTION_(invoker, ... /*interface*/) \
    static constexpr ::xtd::bindable<::xtd::detail::unique_t<>, ::xtd::invokers::invoker __VA_OPT__(, __VA_ARGS__)>
#endif

#ifdef _MSC_VER
#  define XTD_IMPL_TAIL(...) , __VA_ARGS__)
#else
#  define XTD_IMPL_TAIL(...) __VA_OPT__(,) __VA_ARGS__)
#endif

namespace xtd::detail {
    template<class T, class Bindable, std::same_as<typename std::remove_cvref_t<Bindable>::invoker_t> Invoker>
    // Ensure that the right invoker is being used for the tag
    using assert_invoker_t = T;
}

#define XTD_IMPL_(scope, ... /*tag*/) \
    extern /* to disable in class */ xtd_invoke_ ## scope(\
        ::xtd::detail::assert_invoker_t<::xtd_free_t, decltype(__VA_ARGS__), ::xtd::invokers::scope>,\
        ::xtd::tag_of<__VA_ARGS__> XTD_IMPL_TAIL
#define XTD_IMPL_TAG_(scope, ... /*tag*/) \
    extern /* to disable in class */ xtd_invoke_ ## scope(\
        ::xtd::detail::assert_invoker_t<::xtd_free_t, __VA_ARGS__, ::xtd::invokers::scope>,\
        const __VA_ARGS__& XTD_IMPL_TAIL
#define XTD_IMPL_THIS_(scope, ... /*tag*/) \
    friend /* allow only in class */ xtd_invoke_ ## scope(\
        ::xtd::detail::assert_invoker_t<::xtd_method_t<this_t>, decltype(__VA_ARGS__), ::xtd::invokers::scope>,\
        ::xtd::tag_of<__VA_ARGS__> XTD_IMPL_TAIL
#define XTD_IMPL_TAG_THIS_(scope, ... /*tag*/) \
    friend /* allow only in class */ xtd_invoke_ ## scope(\
        ::xtd::detail::assert_invoker_t<::xtd_method_t<this_t>, __VA_ARGS__, ::xtd::invokers::scope>,\
        const __VA_ARGS__& XTD_IMPL_TAIL

#define XTD_IMPL(... /*tag*/)          XTD_IMPL_(main,__VA_ARGS__)
#define XTD_IMPL_TAG(... /*tag*/)      XTD_IMPL_TAG_(main,__VA_ARGS__)
#define XTD_IMPL_THIS(... /*tag*/)     XTD_IMPL_THIS_(main,__VA_ARGS__)
#define XTD_IMPL_TAG_THIS(... /*tag*/) XTD_IMPL_TAG_THIS_(main,__VA_ARGS__)

// EVIL MACROS

#define XTD_IMPORT_SPECIALIZED_OVL(t, tag, alias, qualifier) \
    template<class... _T_UGLY_>\
    constexpr auto alias(_T_UGLY_&&... ins) qualifier \
        noexcept(noexcept(xtd_invoke_main(xtd_method_t<t>{}, tag, std::declval<t qualifier>(), std::forward<decltype(ins)>(ins)...)))\
        -> decltype(xtd_invoke_main(xtd_method_t<t>{}, tag, std::declval<t qualifier>(), std::forward<decltype(ins)>(ins)...))\
    {\
        return xtd_invoke_main(xtd_method_t<t>{}, tag, static_cast<t qualifier>(*this), std::forward<decltype(ins)>(ins)...);\
    }\

#define XTD_IMPORT_SPECIALIZED(type, tag, alias) \
    XTD_IMPORT_SPECIALIZED_OVL(type, tag, alias, &)\
    XTD_IMPORT_SPECIALIZED_OVL(type, tag, alias, const &)\
    XTD_IMPORT_SPECIALIZED_OVL(type, tag, alias, &&)\
    XTD_IMPORT_SPECIALIZED_OVL(type, tag, alias, const &&)

#define XTD_USING_IMPL_1(nam)                 XTD_IMPORT_SPECIALIZED(this_t, nam, nam)
#define XTD_USING_IMPL_2(namspace, nam)       XTD_IMPORT_SPECIALIZED(this_t, namspace::nam, nam)
#define XTD_USING_IMPL_3(type, namspace, nam) XTD_IMPORT_SPECIALIZED(type, namspace::nam, nam)

#define XTD_IMPL_TRY_METHOD(tag, func) \
    constexpr auto impl(tag) (auto&& obj, auto&&... in)\
        noexcept(noexcept(FWD(obj).func(FWD(in)...)))\
        -> decltype(FWD(obj).func(FWD(in)...))\
    {\
        return FWD(obj).func(FWD(in)...);\
    }

#define XTD_IMPL_TRY_FORWARD(tag, func) \
    template<class... Ts>\
    constexpr auto impl(tag) (Ts&&... in)\
        noexcept(noexcept(func(FWD(in)...)))\
        -> decltype(func(FWD(in)...))\
    {\
        return func(FWD(in)...);\
    }

#endif /* EXTEND_INCLUDE_EXTEND_HPP */
