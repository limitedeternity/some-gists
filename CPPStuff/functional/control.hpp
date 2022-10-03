#pragma once
#include "data.hpp"

namespace detail {

    template <typename From, typename To>
    struct morphism {
        using from = From;
        using to = To;
    };

    struct morph_tag {};
}

namespace helpers {

    template <template <typename...> class Ctor, typename... Args>
    Ctor<Args...> construct_type(std::tuple<Args...>);
}

namespace control {

    template <template <typename...> class Impl, template <typename...> class Ctor, typename... Args>
    struct alternative {

        using fa = Ctor<Args...>;
        using this_type = Impl<Args...>;
        using base_type = alternative<Impl, Ctor, Args...>;

    private:
        struct type_check {
            fa(*empty)();              // empty :: f a
            fa&&(*select)(fa&&, fa&&); // select :: f a -> f a -> f a
        } _ {
            this_type::empty,
            this_type::select
        };

        static void base_check() {
            static_assert(std::is_base_of_v<base_type, this_type>);
        }
    };

    template <template <typename...> class Impl, class Morph, template <typename...> class Ctor, typename... Args>
    struct monad {

        using a = typename Morph::from;
        using b = typename Morph::to;

        static_assert((std::is_same_v<detail::morph_tag, Args> || ...));

        using ma = decltype(
            helpers::construct_type<Ctor>(
                std::tuple<std::conditional_t<std::is_same_v<detail::morph_tag, Args>, a, Args>...>{}
            )
        );

        using mb = decltype(
            helpers::construct_type<Ctor>(
                std::tuple<std::conditional_t<std::is_same_v<detail::morph_tag, Args>, b, Args>...>{}
            )
        );

        using this_type = typename Impl<Args...>::template with_morphism<a, b>;
        using base_type = monad<Impl, Morph, Ctor, Args...>;

    private:
        struct type_check {
            ma(*pure)(a&&);                                            // pure :: a -> m a
            mb(*bind)(const ma&, const std::function<mb(const a&)>&);  // bind :: m a -> (a -> m b) -> m b
        } _ {
            this_type::pure,
            this_type::bind
        };

        static void base_check() {
            static_assert(std::is_base_of_v<base_type, this_type>);
        }
    };
}

namespace alternative {

    template <typename... Args>
    struct for_vector : control::alternative<for_vector, std::vector, Args...> {

        using fa = std::vector<Args...>;

        static fa empty() {
            return {};
        }

        static fa&& select(fa&& lhs, fa&& rhs) {
            return std::move(lhs.empty() ? rhs : lhs);
        }
    };

    template <typename... Args>
    struct for_basic_string : control::alternative<for_basic_string, std::basic_string, Args...> {

        using fa = std::basic_string<Args...>;

        static fa empty() {
            return {};
        }

        static fa&& select(fa&& lhs, fa&& rhs) {
            return std::move(lhs.empty() ? rhs : lhs);
        }
    };

    template <typename... Args>
    struct for_unique_ptr : control::alternative<for_unique_ptr, std::unique_ptr, Args...> {

        using fa = std::unique_ptr<Args...>;

        static fa empty() {
            return {};
        }

        static fa&& select(fa&& lhs, fa&& rhs) {
            return std::move(lhs ? lhs : rhs);
        }
    };

    template <typename... Args>
    struct for_maybe : control::alternative<for_maybe, std::optional, Args...> {

        using fa = std::optional<Args...>;

        static fa empty() {
            return {};
        }

        static fa&& select(fa&& lhs, fa&& rhs) {
            return std::move(lhs ? lhs : rhs);
        }
    };
}

namespace monad {

    template <typename... Args>
    struct for_unique_ptr {

        template <typename From, typename To>
        struct with_morphism : control::monad<monad::for_unique_ptr, detail::morphism<From, To>, std::unique_ptr, Args...> {

            using a = From;
            using b = To;
            using ma = typename with_morphism::base_type::ma;
            using mb = typename with_morphism::base_type::mb;

            static ma pure(a&& arg) {
                return std::make_unique<a>(std::move(arg));
            }

            static mb bind(const ma& arg, const std::function<mb(const a&)>& fun) {
                return arg ? fun(*arg) : nullptr;
            }
        };
    };

    template <typename... Args>
    struct for_maybe {

        template <typename From, typename To>
        struct with_morphism : control::monad<monad::for_maybe, detail::morphism<From, To>, std::optional, Args...> {

            using a = From;
            using b = To;
            using ma = typename with_morphism::base_type::ma;
            using mb = typename with_morphism::base_type::mb;

            static ma pure(a&& arg) {
                return { std::move(arg) };
            }

            static mb bind(const ma& arg, const std::function<mb(const a&)>& fun) {
                return arg ? fun(*arg) : std::nullopt;
            }
        };
    };

    template <typename E, typename T>
    struct for_either;

    template <typename T>
    struct for_either<detail::morph_tag, T> {

        template <typename From, typename To>
        struct with_morphism : control::monad<monad::for_either, detail::morphism<From, To>, data::either, detail::morph_tag, T> {

            using a = From;
            using b = To;
            using ma = typename with_morphism::base_type::ma;
            using mb = typename with_morphism::base_type::mb;

            static ma pure(a&& arg) {
                return { std::move(arg) };
            }

            static mb bind(const ma& arg, const std::function<mb(const a&)>& fun) {
                return arg.try_left() ? fun(arg.left()) : arg.right();
            }
        };
    };

    template <typename E>
    struct for_either<E, detail::morph_tag> {

        template <typename From, typename To>
        struct with_morphism : control::monad<monad::for_either, detail::morphism<From, To>, data::either, E, detail::morph_tag> {

            using a = From;
            using b = To;
            using ma = typename with_morphism::base_type::ma;
            using mb = typename with_morphism::base_type::mb;

            static ma pure(a&& arg) {
                return { std::move(arg) };
            }

            static mb bind(const ma& arg, const std::function<mb(const a&)>& fun) {
                return arg.try_right() ? fun(arg.right()) : arg.left();
            }
        };
    };
}
