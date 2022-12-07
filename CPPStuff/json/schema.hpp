#pragma once
#include <variant>

#include "./jansson_wrapper.h"
#include "../utils/meta.h"

#define DEFINE_SCHEMA_KEY(_name_, _literal_) struct _name_ { static constexpr const char* str() { return _literal_; } }

namespace json::schema {

    namespace defs {

        struct string {};
        struct integer {};
        struct real {};
        struct boolean {};
        struct null {};
        struct any {};
        struct undefined {};

        template <typename T>
        struct array {};

        template <typename Key, typename T>
        struct object_mapping {};

        template <class... Mappings>
        using object = detail::parameter_pack<Mappings...>;

        template <typename... Ts>
        using sum_type = std::variant<Ts...>;

        template <typename T>
        using maybe = sum_type<T, null>;
    }

    template <typename T>
    bool check_schema(T, const json::Value& value) {

        /* */if constexpr (std::is_same_v<T, defs::string>) {
            return value && value.is_string();
        }
        else if constexpr (std::is_same_v<T, defs::integer>) {
            return value && value.is_integer();
        }
        else if constexpr (std::is_same_v<T, defs::real>) {
            return value && value.is_real();
        }
        else if constexpr (std::is_same_v<T, defs::boolean>) {
            return value && value.is_boolean();
        }
        else if constexpr (std::is_same_v<T, defs::null>) {
            return value && value.is_null();
        }
        else if constexpr (std::is_same_v<T, defs::any>) {
            return value;
        }
        else if constexpr (std::is_same_v<T, defs::undefined>) {
            return !value;
        }
        else {
            static_assert(false, "incorrect type specified");
            return false;
        }
    }

    template <typename T>
    bool check_schema(defs::array<T>, const json::Value& array) {

        bool is_ok = array && array.is_array();

        for (size_t i = 0; i < (is_ok ? array.size() : 0); ++i) {
            is_ok &= check_schema(T{}, array[i]);
        }

        return is_ok;
    }

    template <typename... Keys, typename... Ts>
    bool check_schema(defs::object<defs::object_mapping<Keys, Ts>...>, const json::Value& object) {
        return ((object && object.is_object()) && ... && check_schema(Ts{}, object[Keys::str()]));
    }

    template <typename... Ts>
    bool check_schema(defs::sum_type<Ts...>, const json::Value& value) {
        return (... || check_schema(Ts{}, value));
    }
}
