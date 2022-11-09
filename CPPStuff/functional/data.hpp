#pragma once
#include "../defines/cxx.h"
#include "../detail_api.h"

namespace data {

    template <typename E, typename T>
    struct either : std::variant<E, T> {

        // Helper types
        using left_type = E;
        using right_type = T;

        // Inherit constructors and assign
        using std::variant<E, T>::variant;
        using std::variant<E, T>::operator=;

        DEFAULT_COPY_ASSIGN_AND_MOVE(either);

        // Non-const downcasts
        std::variant<E, T>& as_variant() & { return *this; }
        std::variant<E, T>&& as_variant() && { return nonstd::move(*this); }

        // Const downcasts
        const std::variant<E, T>& as_variant() const& { return *this; }

        // Non-const observers
        left_type* try_left() { return std::get_if<0>(this); }
        right_type* try_right() { return std::get_if<1>(this); }

        // Const observers
        const left_type* try_left() const { return std::get_if<0>(this); }
        const right_type* try_right() const { return std::get_if<1>(this); }

        // Non-const accessors
        left_type& left() & { return *std::get_if<0>(this); }
        left_type&& left() && { return nonstd::move(std::ignore, *std::get_if<0>(this)); }
        right_type& right() & { return *std::get_if<1>(this); }
        right_type&& right() && { return nonstd::move(std::ignore, *std::get_if<1>(this)); }

        // Const accessors
        const left_type& left() const& { return *std::get_if<0>(this); }
        const right_type& right() const& { return *std::get_if<1>(this); }
    };
}

namespace function {

    template <typename F>
    decltype(auto) compose(F&& f) {
        return [f = std::forward<F>(f)](auto&&... args){
            return f(std::forward<decltype(args)>(args)...);
        };
    }

    template <typename F, typename G, typename... Fs>
    decltype(auto) compose(F&& f, G&& g, Fs&&... fs) {
        return compose(
            [f = std::forward<F>(f), g = std::forward<G>(g)](auto&&... args) {
                return g(f(std::forward<decltype(args)>(args)...));
            },
            std::forward<Fs>(fs)...
        );
    }
}
