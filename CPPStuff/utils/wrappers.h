#pragma once
#include "./meta.h"

template <auto fn>
struct functor_from_fn {
    template <typename... Args>
    constexpr decltype(auto) operator()(Args&&... args) const {
        return fn(std::forward<Args>(args)...);
    }
};

template <auto fn>
struct detail::function_type<functor_from_fn<fn>> : detail::function_type<decltype(fn)> {};

template <class F>
struct scope_guard : F {
    ~scope_guard() { F::operator()(); }
};

template <class F>
scope_guard(F) -> scope_guard<F>;

template <class... Fs>
struct polymorphic_functor : Fs... {
    using Fs::operator()...;
};

template <class... Fs>
polymorphic_functor(Fs...) -> polymorphic_functor<Fs...>;
