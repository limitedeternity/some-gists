#pragma once
#include "data.hpp"

namespace detail {

    template <typename T, typename Enabled = void>
    struct has_reserve_method : std::false_type {};

    template <typename T>
    struct has_reserve_method<T, std::enable_if_t<std::is_member_function_pointer_v<decltype(&T::reserve)>>> : std::true_type {};

    template <typename... Ts>
    struct parameter_pack {

        static constexpr size_t size = sizeof...(Ts);

        template <template <typename...> class Ctor>
        using unpack = Ctor<Ts...>;

        template <typename What, typename With, typename T>
        struct _replace {
            using type = std::conditional_t<std::is_same_v<What, T>, With, T>;
        };

        template <typename What, typename With, template <typename...> class Ctor, typename... Args>
        struct _replace<What, With, Ctor<Args...>> {
            using type = std::conditional_t<
                std::is_same_v<What, Ctor<Args...>>, With,
                Ctor<typename _replace<What, With, Args>::type...>
            >;
        };

        template <typename What, typename With>
        using replace = parameter_pack<typename _replace<What, With, Ts>::type...>;

        template <typename What, typename T>
        struct _contains {
            using trait = std::is_same<What, T>;
        };

        template <typename What, template <typename...> class Ctor, typename... Args>
        struct _contains<What, Ctor<Args...>> {
            using trait = std::disjunction<std::is_same<What, Ctor<Args...>>, typename _contains<What, Args>::trait...>;
        };

        template <typename What>
        using contains = std::disjunction<typename _contains<What, Ts>::trait...>;

        template <typename Iseq, typename T>
        struct _at;

        template <size_t I, template <typename...> class Ctor, typename... Args>
        struct _at<std::index_sequence<I>, Ctor<Args...>> {
            using type = std::tuple_element_t<I, std::tuple<Args...>>;
        };

        template <size_t I, size_t... Is, template <typename...> class Ctor, typename... Args>
        struct _at<std::index_sequence<I, Is...>, Ctor<Args...>> {
            using type = typename _at<std::index_sequence<Is...>, std::tuple_element_t<I, std::tuple<Args...>>>::type;
        };

        template <size_t... Is>
        using at = typename _at<std::index_sequence<Is...>, parameter_pack<Ts...>>::type;
    };

    template <typename Type>
    struct type_constructor_of;

    template <template <typename...> class Ctor, typename... Args>
    struct type_constructor_of<Ctor<Args...>> {
        template <typename... Nargs>
        using constructor = Ctor<Nargs...>;
        using arguments = parameter_pack<Args...>;
    };
}

namespace utility {
    struct morph_tag {};
}

namespace control {

    template <class ImplType>
    struct alternative {

        using fa =
            typename detail::type_constructor_of<ImplType>
            ::arguments
            ::template at<0>;

        // -------------------

        using this_type = ImplType;
        using base_type = alternative<ImplType>;

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

    template <class ImplFromTo, typename Type>
    struct monad {

        using a =
            typename detail::type_constructor_of<ImplFromTo>
            ::arguments
            ::template at<0>;

        using b =
            typename detail::type_constructor_of<ImplFromTo>
            ::arguments
            ::template at<1>;

        // -------------------

        using _ctor = detail::type_constructor_of<Type>;
        static_assert(_ctor::arguments::template contains<utility::morph_tag>::value);

        using ma =
            typename _ctor::arguments
            ::template replace<utility::morph_tag, a>
            ::template unpack<_ctor::constructor>;

        using mb =
            typename _ctor::arguments
            ::template replace<utility::morph_tag, b>
            ::template unpack<_ctor::constructor>;

        // -------------------

        using this_type = ImplFromTo;
        using base_type = monad<ImplFromTo, Type>;

    private:
        struct type_check {
            ma(*pure)(const a&);                                       // pure :: a -> m a
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

    template <typename Type>
    struct for_container_like : control::alternative<for_container_like<Type>> {

        using fa = typename for_container_like::base_type::fa;

        static fa empty() {
            return {};
        }

        static fa&& select(fa&& lhs, fa&& rhs) {
            return std::forward<fa>(lhs.empty() ? rhs : lhs);
        }
    };

    template <typename Type>
    struct for_bool_like : control::alternative<for_bool_like<Type>> {

        using fa = typename for_bool_like::base_type::fa;

        static fa empty() {
            return {};
        }

        static fa&& select(fa&& lhs, fa&& rhs) {
            return std::forward<fa>(lhs ? lhs : rhs);
        }
    };
}

namespace monad {

    template <typename Type>
    struct for_container_like {

        template <typename From, typename To>
        struct with_morphism : control::monad<with_morphism<From, To>, Type> {

            using a = typename with_morphism::base_type::a;
            using b = typename with_morphism::base_type::b;
            using ma = typename with_morphism::base_type::ma;
            using mb = typename with_morphism::base_type::mb;

            static ma pure(const a& arg) {
                return std::initializer_list<a>{arg};
            }

            static mb bind(const ma& arg, const std::function<mb(const a&)>& fun) {

                mb result;

                for (const a& item : arg) {

                    const mb tmp = fun(item);

                    if constexpr (detail::has_reserve_method<mb>::value) {
                        result.reserve(result.size() + tmp.size());
                    }

                    result.insert(result.cend(), tmp.cbegin(), tmp.cend());
                }

                return result;
            }
        };
    };

    template <typename... Args>
    struct for_unique_ptr {

        template <typename From, typename To>
        struct with_morphism : control::monad<with_morphism<From, To>, std::unique_ptr<Args...>> {

            using a = typename with_morphism::base_type::a;
            using b = typename with_morphism::base_type::b;
            using ma = typename with_morphism::base_type::ma;
            using mb = typename with_morphism::base_type::mb;

            static ma pure(const a& arg) {
                return std::make_unique<a>(arg);
            }

            static mb bind(const ma& arg, const std::function<mb(const a&)>& fun) {
                return arg ? fun(*arg) : nullptr;
            }
        };
    };

    template <typename T>
    struct for_maybe {

        template <typename From, typename To>
        struct with_morphism : control::monad<with_morphism<From, To>, std::optional<T>> {

            using a = typename with_morphism::base_type::a;
            using b = typename with_morphism::base_type::b;
            using ma = typename with_morphism::base_type::ma;
            using mb = typename with_morphism::base_type::mb;

            static ma pure(const a& arg) {
                return arg;
            }

            static mb bind(const ma& arg, const std::function<mb(const a&)>& fun) {
                return arg ? fun(*arg) : std::nullopt;
            }
        };
    };

    template <typename E, typename T>
    struct for_either;

    template <typename T>
    struct for_either<utility::morph_tag, T> {

        template <typename From, typename To>
        struct with_morphism : control::monad<with_morphism<From, To>, data::either<utility::morph_tag, T>> {

            using a = typename with_morphism::base_type::a;
            using b = typename with_morphism::base_type::b;
            using ma = typename with_morphism::base_type::ma;
            using mb = typename with_morphism::base_type::mb;

            static ma pure(const a& arg) {
                return arg;
            }

            static mb bind(const ma& arg, const std::function<mb(const a&)>& fun) {
                return arg.try_left() ? fun(arg.left()) : arg.right();
            }
        };
    };

    template <typename E>
    struct for_either<E, utility::morph_tag> {

        template <typename From, typename To>
        struct with_morphism : control::monad<with_morphism<From, To>, data::either<E, utility::morph_tag>> {

            using a = typename with_morphism::base_type::a;
            using b = typename with_morphism::base_type::b;
            using ma = typename with_morphism::base_type::ma;
            using mb = typename with_morphism::base_type::mb;

            static ma pure(const a& arg) {
                return arg;
            }

            static mb bind(const ma& arg, const std::function<mb(const a&)>& fun) {
                return arg.try_right() ? fun(arg.right()) : arg.left();
            }
        };
    };
}
