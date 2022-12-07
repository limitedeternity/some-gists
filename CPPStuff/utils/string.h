#pragma once
#include <algorithm>
#include <string>
#include <string_view>
#include <type_traits>

namespace string {

    template <typename CharT>
    struct of {

        using type = std::basic_string<CharT>;

        template <int convert(int)>
        static std::basic_string<CharT> change_case(const std::basic_string_view<CharT> s) {

            std::basic_string<CharT> result;
            result.resize(s.size());

            std::transform(
                s.cbegin(), s.cend(), result.begin(),
                [](std::make_unsigned_t<CharT> c) -> CharT { return convert(c); }
            );

            return result;
        }

        static bool iequals(const std::basic_string_view<CharT> s1, const std::basic_string_view<CharT> s2) {

            return std::equal(
                s1.cbegin(), s1.cend(),
                s2.cbegin(), s2.cend(),
                [](std::make_unsigned_t<CharT> c1, std::make_unsigned_t<CharT> c2) { return tolower(c1) == tolower(c2); }
            );
        }

        static size_t ihash(const std::basic_string_view<CharT> s) {
            return std::hash<std::basic_string<CharT>>{}(change_case<tolower>(s));
        }
    };
}
