#ifndef EXTEND_DOLLAR_MACROS_DEFINE_HPP
#define EXTEND_DOLLAR_MACROS_DEFINE_HPP

#ifdef _MSC_VER
#  define XTD_APPLY_TAIL(...) , __VA_ARGS__)
#else
#  define XTD_APPLY_TAIL(...) __VA_OPT__(,) __VA_ARGS__)
#endif

#define XTD_APPLY_2(f, I) ->* f (::xtd::bind_placeholder<I>{} XTD_APPLY_TAIL
#define XTD_APPLY_1(f) XTD_APPLY_2(f, 0)

#define $$ ->*
#define $_ ::xtd::bind_placeholder{}

#define $ XTD_APPLY_1
#define $1(f) XTD_APPLY_2(f, 1)
#define $2(f) XTD_APPLY_2(f, 2)
#define $3(f) XTD_APPLY_2(f, 3)
#define $4(f) XTD_APPLY_2(f, 4)

#endif /* EXTEND_DOLLAR_MACROS_DEFINE_HPP */
