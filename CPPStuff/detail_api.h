#pragma once
#define CLEAN_TYPE(_var_) std::decay_t<decltype(_var_)>
#define MOVING_BIND(_var_) CLEAN_TYPE(_var_)(nonstd::move(_var_))
#define DEFAULT(_var_) _var_ = CLEAN_TYPE(_var_){}

namespace nonstd {

    template <bool Cond>
    struct _if;

    template <>
    struct _if<true> {

        template <typename Then, typename Else>
        using select = Then;

        template <typename Then>
        using exec = Then;
    };

    template <>
    struct _if<false> {
        template <typename Then, typename Else>
        using select = Else;
    };

    /*
     * Generates only 2 instances no matter how much it's used
     */
    template <bool Cond, typename Then, typename Else>
    using conditional_t = typename _if<Cond>::template select<Then, Else>;

    /*
     * Produces more informative hint on substitution failure
     * Generates only 1 instance no matter how much it's used
     */
    template <bool Cond, typename Then = void>
    using enable_if_t = typename _if<Cond>::template exec<Then>;

    /*
     * Explodes when used in invalid contexts
     */
    template <typename T>
    [[nodiscard]] constexpr decltype(auto) move(T&& arg) noexcept {
        using no_ref = std::remove_reference_t<decltype(arg)>;
        static_assert(!std::is_rvalue_reference_v<decltype(arg)>, "nonstd::move(rvalue) -> rvalue");
        static_assert(!std::is_trivially_copyable_v<no_ref>, "nonstd::move(trivial) -> trivial");
        static_assert(!std::is_const_v<no_ref>, "const T v; nonstd::move(v); -> T v; nonstd::move(v)");
        return static_cast<no_ref&&>(arg);
    }

    /*
     * Just casts to rvalue (equivalent to std::move)
     */
    template <typename T>
    [[nodiscard]] constexpr decltype(auto) move(decltype(std::ignore), T&& arg) noexcept {
        using no_ref = std::remove_reference_t<decltype(arg)>;
        return static_cast<no_ref&&>(arg);
    }

    template <typename T, size_t I>
    struct _indexed {
        using type = T;
    };

    template <typename Iseq, typename... Ts>
    struct _indexer;

    template <size_t... Is, typename... Ts>
    struct _indexer<std::index_sequence<Is...>, Ts...> : _indexed<Ts, Is>... {};

    template <size_t I, typename T>
    _indexed<T, I> _select_base(_indexed<T, I>);

    /*
     * https://developercommunity.visualstudio.com/t/add-support-for-clangs-type-pack-element-to-improv/1439235
     */
    template <size_t I, typename... Ts>
    using pack_element_t = typename decltype(
        _select_base<I>(
            _indexer<
                std::make_index_sequence<sizeof...(Ts)>,
                Ts...
            >{}
        )
    )::type;

    /*
     * std::make_array was removed in Library Fundamentals TS v3 + had some questionable implementation details
     */
    template <typename T, typename... Elems>
    [[nodiscard]] constexpr std::array<T, sizeof...(Elems)> make_array(Elems&&... elems) {
        return { std::forward<Elems>(elems)... };
    }
}

namespace operators {

    template <typename E>
    constexpr nonstd::enable_if_t<
        std::is_enum_v<E>,
        std::underlying_type_t<E>
    >
    enum_to_integral(E e) {
        return static_cast<std::underlying_type_t<E>>(e);
    }

    template <auto Low, auto High, typename T>
    constexpr nonstd::enable_if_t<
        std::conjunction_v<
            std::is_integral<decltype(Low)>,
            std::is_integral<decltype(High)>,
            std::is_integral<T>
        >,
        bool
    >
    in_range(T value) {
        using low = std::common_type_t<decltype(Low), T>;
        using high = std::common_type_t<decltype(High), T>;
        return static_cast<low>(Low) <= value && value < static_cast<high>(High);
    }
}

namespace detail {

    /*
     * Structures for type operations
     */

    template <typename T>
    struct type_identity {
        using type = T;
    };

    template <typename... Ts>
    struct parameter_pack {

        static constexpr size_t size = sizeof...(Ts);

        template <template <typename...> class Ctor>
        using unpack = Ctor<Ts...>;

        template <typename What, typename With, typename T>
        struct _replace {
            using type = nonstd::conditional_t<std::is_same_v<What, T>, With, T>;
        };

        template <typename What, typename With, template <typename...> class Ctor, typename... Args>
        struct _replace<What, With, Ctor<Args...>> {
            using type = nonstd::conditional_t<
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

        template <typename T>
        struct _at<std::index_sequence<>, T> {
            using type = T;
        };

        template <size_t I, size_t... Is, template <typename...> class Ctor, typename... Args>
        struct _at<std::index_sequence<I, Is...>, Ctor<Args...>> {
            using type = typename _at<std::index_sequence<Is...>, nonstd::pack_element_t<I, Args...>>::type;
        };

        template <size_t... Is>
        using at = typename _at<std::index_sequence<Is...>, parameter_pack<Ts...>>::type;

        template <size_t N, class Take, class Drop>
        struct _select;

        template <class Take, class Drop>
        struct _select<0, Take, Drop> {
            using take = Take;
            using drop = Drop;
        };

        template <size_t N, typename... Acc, typename Head, typename... Tail>
        struct _select<N, parameter_pack<Acc...>, parameter_pack<Head, Tail...>> {
            using take = typename _select<N - 1, parameter_pack<Acc..., Head>, parameter_pack<Tail...>>::take;
            using drop = typename _select<N - 1, parameter_pack<Acc..., Head>, parameter_pack<Tail...>>::drop;
        };

        template <size_t N>
        using take = typename _select<N, parameter_pack<>, parameter_pack<Ts...>>::take;

        template <size_t N>
        using drop = typename _select<N, parameter_pack<>, parameter_pack<Ts...>>::drop;

        template <typename... Us>
        using prepend = parameter_pack<Us..., Ts...>;

        template <typename... Us>
        using append = parameter_pack<Ts..., Us...>;
    };

    template <typename Type>
    struct type_constructor_of;

    template <template <typename...> class Ctor, typename... Args>
    struct type_constructor_of<Ctor<Args...>> {
        template <typename... Nargs>
        using constructor = Ctor<Nargs...>;
        using arguments = parameter_pack<Args...>;
    };

    template <typename Type>
    struct function_type : function_type<decltype(&Type::operator())> {};

    template <typename R, typename... Args>
    struct function_type<R(Args...)> {
        using return_type = R;
        using arguments = parameter_pack<Args...>;
        using as_object = std::function<R(Args...)>;
    };

    template <typename R, typename... Args>
    struct function_type<R(Args...) const> : function_type<R(Args...)> {};

    template <typename R, typename... Args>
    struct function_type<R(*)(Args...)> : function_type<R(Args...)> {};

    template <typename R, typename... Args>
    struct function_type<R(&)(Args...)> : function_type<R(Args...)> {};

    template <typename R, class T, typename... Args>
    struct function_type<R(T::*)(Args...)> : function_type<R(Args...)> {
        using class_type = T;
    };

    template <typename R, class T, typename... Args>
    struct function_type<R(T::*)(Args...) const> : function_type<R(Args...)> {
        using class_type = T;
    };

    /*
     * SFINAE transformers and checkers
     */

    template <typename T>
    struct remove_all_pointers : nonstd::conditional_t<
        std::is_pointer_v<T>,
        detail::remove_all_pointers<
            std::remove_pointer_t<T>
        >,
        detail::type_identity<T>
    > {};

    template <typename T>
    using remove_all_pointers_t = typename detail::remove_all_pointers<T>::type;

    template <typename T, typename... Ts>
    struct is_any_of : std::false_type {};

    template <typename T, typename Head, typename... Tail>
    struct is_any_of<T, Head, Tail...> : nonstd::conditional_t<
        std::is_same_v<T, Head>,
        std::true_type,
        detail::is_any_of<T, Tail...>
    > {};

    template <typename T, typename... Ts>
    inline constexpr bool is_any_of_v = is_any_of<T, Ts...>::value;

    template <typename Test, template <typename...> class Ctor>
    struct is_specialization : std::false_type {};

    template <template <typename...> class Ctor, typename... Args>
    struct is_specialization<Ctor<Args...>, Ctor> : std::true_type {};

    template <typename Test, template <typename...> class Ctor>
    inline constexpr bool is_specialization_v = is_specialization<Test, Ctor>::value;

    /*
     * Type mapper components
     *
     * template <typename T>
     * typename std::disjunction<
     *     on_types_equal<T, std::string, char*>,
     *     on_types_equal<T, std::wstring, wchar_t*>,
     *     default_type<T>
     * >::type
     */

    template <typename V1, typename V2, typename T>
    struct on_types_equal : std::is_same<V1, V2> {
        using type = T;
    };

    template <typename T>
    struct default_type : std::true_type {
        using type = T;
    };
}
