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

    template <typename... Ts>
    struct pack {

        template <typename From, typename To>
        using replace = std::tuple<std::conditional_t<std::is_same_v<From, Ts>, To, Ts>...>;
    };

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
                typename helpers::pack<Args...>::template replace<detail::morph_tag, a>{}
            )
        );

        using mb = decltype(
            helpers::construct_type<Ctor>(
                typename helpers::pack<Args...>::template replace<detail::morph_tag, b>{}
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

    /*
     * alternative::for_maybe<int>::select({}, 5) -> 5
     * alternative::for_maybe<int>::select({}, {}) -> std::nullopt
     */
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
    struct for_maybe {

        template <typename From, typename To>
        struct with_morphism : control::monad<for_maybe, detail::morphism<From, To>, std::optional, Args...> {

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

    template <typename... Args>
    struct for_unique_ptr {

        template <typename From, typename To>
        struct with_morphism : control::monad<for_unique_ptr, detail::morphism<From, To>, std::unique_ptr, Args...> {

            using a = From;
            using b = To;
            using ma = typename with_morphism::base_type::ma;
            using mb = typename with_morphism::base_type::mb;

            static ma pure(a&& arg) {
                return std::make_unique<From>(std::move(arg));
            }

            static mb bind(const ma& arg, const std::function<mb(const a&)>& fun) {
                return arg ? fun(*arg) : nullptr;
            }
        };
    };

    /*
     * monad::for_either<std::exception, detail::morph_tag>::with_morphism<int, long>::bind(5, [](const auto& value) { return value * 2; });
     * -> 10
     *
     * monad::for_either<std::exception, detail::morph_tag>::with_morphism<int, int>::bind(std::bad_alloc(), [](const auto& value) { return value * 2; });
     * -> std::exception
     */
    template <typename... Args>
    struct for_either {

        template <typename From, typename To>
        struct with_morphism : control::monad<for_either, detail::morphism<From, To>, data::either, Args...> {

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
