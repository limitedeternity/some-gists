#pragma once

namespace control {

    template <template <typename...> class Impl, template <typename...> class Ctor, typename... Args>
    class alternative {

        struct type_check {
            Ctor<Args...>(*empty)();
            Ctor<Args...>&&(*select)(Ctor<Args...>&&, Ctor<Args...>&&);
        } _ {
            Impl<Args...>::empty,
            Impl<Args...>::select
        };

        static void delayed_base_check() {
            static_assert(std::is_base_of_v<alternative<Impl, Ctor, Args...>, Impl<Args...>>);
        }
    };
}

namespace alternative {

    template <typename... Args>
    struct for_vector : control::alternative<for_vector, std::vector, Args...> {

        static std::vector<Args...> empty() {
            return {};
        }

        static std::vector<Args...>&& select(std::vector<Args...>&& lhs, std::vector<Args...>&& rhs) {
            return std::move(lhs.empty() ? rhs : lhs);
        }
    };

    template <typename... Args>
    struct for_basic_string : control::alternative<for_basic_string, std::basic_string, Args...> {

        static std::basic_string<Args...> empty() {
            return {};
        }

        static std::basic_string<Args...>&& select(std::basic_string<Args...>&& lhs, std::basic_string<Args...>&& rhs) {
            return std::move(lhs.empty() ? rhs : lhs);
        }
    };

    template <typename... Args>
    struct for_unique_ptr : control::alternative<for_unique_ptr, std::unique_ptr, Args...> {

        static std::unique_ptr<Args...> empty() {
            return {};
        }

        static std::unique_ptr<Args...>&& select(std::unique_ptr<Args...>&& lhs, std::unique_ptr<Args...>&& rhs) {
            return std::move(lhs ? lhs : rhs);
        }
    };

    template <typename... Args>
    struct for_maybe : control::alternative<for_maybe, std::optional, Args...> {

        static std::optional<Args...> empty() {
            return {};
        }

        static std::optional<Args...>&& select(std::optional<Args...>&& lhs, std::optional<Args...>&& rhs) {
            return std::move(lhs ? lhs : rhs);
        }
    };
}
