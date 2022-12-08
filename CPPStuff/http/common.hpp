#pragma once
#include <optional>
#include <string>
#include <unordered_map>

#include "../utils/string.h"

namespace http {

    constexpr static std::string_view LINE_END = "\r\n";
    static_assert(LINE_END.size() == 2);

    struct version {

        enum version_v { HTTP_1_0, HTTP_1_1 };

        version() = default;
        constexpr version(const version_v value) : m_value(value) {}

        // Allow "switch (version)" usage
        constexpr operator version_v() const { return m_value; }

        // Prevent "if (version)" usage
        explicit operator bool() = delete;

        // Allow comparisons
        constexpr bool operator==(const version& other) const { return m_value == other.m_value; }
        constexpr bool operator!=(const version& other) const { return !(*this == other); }

        static std::optional<version> from_string(const std::string_view lookup_arg) {

            static const std::unordered_map<std::string, version_v> table{
                {"HTTP/1.0", version_v::HTTP_1_0},
                {"HTTP/1.1", version_v::HTTP_1_1}
            };

            if (
                const auto it = table.find(string::of<char>::change_case<toupper>(lookup_arg));
                it != table.cend())
            {
                return it->second;
            }

            return std::nullopt;
        }

        static std::string_view to_string(const version& lookup_arg) {

            static const std::unordered_map<version_v, std::string_view> table{
                {version_v::HTTP_1_0, "HTTP/1.0"},
                {version_v::HTTP_1_1, "HTTP/1.1"}
            };

            return table.at(lookup_arg);
        }

    private:
        version_v m_value = version_v::HTTP_1_1;
    };

#pragma push_macro("DELETE")
#undef DELETE

    struct method {

        enum method_v { GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE, PATCH };

        method() = default;
        constexpr method(const method_v value) : m_value(value) {}

        // Allow "switch (method)" usage
        constexpr operator method_v() const { return m_value; }

        // Prevent "if (method)" usage
        explicit operator bool() = delete;

        // Allow comparisons
        constexpr bool operator==(const method& other) const { return m_value == other.m_value; }
        constexpr bool operator!=(const method& other) const { return !(*this == other); }

        static std::optional<method> from_string(const std::string_view lookup_arg) {

            static const std::unordered_map<std::string, method_v> table{
                {"GET", method_v::GET},
                {"HEAD", method_v::HEAD},
                {"POST", method_v::POST},
                {"PUT", method_v::PUT},
                {"DELETE", method_v::DELETE},
                {"CONNECT", method_v::CONNECT},
                {"OPTIONS", method_v::OPTIONS},
                {"TRACE", method_v::TRACE},
                {"PATCH", method_v::PATCH}
            };

            if (
                const auto it = table.find(string::of<char>::change_case<toupper>(lookup_arg));
                it != table.cend())
            {
                return it->second;
            }

            return std::nullopt;
        }

        static std::string_view to_string(const method& lookup_arg) {

            static const std::unordered_map<method_v, std::string_view> table{
                {method_v::GET, "GET"},
                {method_v::HEAD, "HEAD"},
                {method_v::POST, "POST"},
                {method_v::PUT, "PUT"},
                {method_v::DELETE, "DELETE"},
                {method_v::CONNECT, "CONNECT"},
                {method_v::OPTIONS, "OPTIONS"},
                {method_v::TRACE, "TRACE"},
                {method_v::PATCH, "PATCH"}
            };

            return table.at(lookup_arg);
        }

    private:
        method_v m_value = method_v::GET;
    };

#pragma pop_macro("DELETE")
}
