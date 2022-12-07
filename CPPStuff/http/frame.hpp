#pragma once
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>

#include "./common.hpp"
#include "../utils/meta.h"
#include "../utils/string.h"
#include "../utils/wrappers.h"

namespace helpers {
    std::string view_join(std::string_view delim, std::initializer_list<std::string_view> views);
}

namespace http {

    struct request {

        http::method method;
        std::string_view uri;
        http::version version;

        std::unordered_map<
            std::string, std::string_view,
            functor_from_fn<string::of<char>::ihash>,
            functor_from_fn<string::of<char>::iequals>
        > headers;

        std::string_view data;
    };

    struct response {

        http::version version;
        uint16_t code = 0;
        std::string_view status_text;

        std::unordered_map<
            std::string, std::string_view,
            functor_from_fn<string::of<char>::ihash>,
            functor_from_fn<string::of<char>::iequals>
        > headers;

        std::string_view data;
    };

    struct frame {
    private:
        frame() = default;

    public:
        template <typename ReprT, typename =
            nonstd::enable_if_t<
                detail::is_any_of_v<ReprT, http::request, http::response>
            >
        >
        static std::optional<ReprT> parse(std::string_view req) {

            if (req.size() <= LINE_END.size() || req[0] == LINE_END[0] && req[1] == LINE_END[1]) {
                return std::nullopt;
            }

            enum class parser_state { INFO, HEADERS, DATA } parser_state = parser_state::INFO;
            ReprT parser_result;
            size_t cur_start = 0;
            size_t cur_end = 0;

            while (parser_state != parser_state::DATA && cur_end < req.size()) {

                enum class line_state { DEFAULT, CR_FOUND, CRLF_FOUND } line_state = line_state::DEFAULT;

                for (; line_state != line_state::CRLF_FOUND && cur_end < req.size(); ++cur_end) {

                    switch (line_state) {

                        case line_state::DEFAULT: {

                            switch (req[cur_end]) {

                                case LINE_END[0]: {
                                    line_state = line_state::CR_FOUND;
                                    break;
                                }

                                default: {
                                    line_state = line_state::DEFAULT;
                                    break;
                                }
                            }

                            break;
                        }

                        case line_state::CR_FOUND: {

                            switch (req[cur_end]) {

                                case LINE_END[1]: {
                                    line_state = line_state::CRLF_FOUND;
                                    break;
                                }

                                case LINE_END[0]: {
                                    line_state = line_state::CR_FOUND;
                                    break;
                                }

                                default: {
                                    line_state = line_state::DEFAULT;
                                    break;
                                }
                            }

                            break;
                        }

                        default: {
                            UNREACHABLE;
                        }
                    }
                }

                if (line_state != line_state::CRLF_FOUND) {
                    return std::nullopt;
                }

                // Fix cursor after loop exit
                --cur_end;

                // Step before CRLF
                cur_end -= LINE_END.size();

                // Automatically step over CRLF
                const auto fix_curs = scope_guard{
                    [&] {
                        cur_end += LINE_END.size() + 1;
                        cur_start = cur_end;
                    }
                };

                // CRLFCRLF case
                if (cur_start > cur_end) {

                    switch (parser_state) {

                        case parser_state::INFO: {
                            return std::nullopt;
                        }

                        case parser_state::HEADERS: {
                            parser_state = parser_state::DATA;
                            continue;
                        }

                        default: {
                            UNREACHABLE;
                        }
                    }
                }

                switch (parser_state) {

                    case parser_state::INFO: {

                        enum class info_state { COMPONENT_1, COMPONENT_2, COMPONENT_3, END } info_state = info_state::COMPONENT_1;
                        size_t cur_boundary = cur_start;

                        for (; info_state != info_state::END && cur_start <= cur_end; ++cur_start) {

                            if (const bool is_space = req[cur_start] == ' ', end_of_line = cur_start == cur_end; is_space || end_of_line) {

                                const auto component_start = req.data() + cur_boundary;
                                const size_t component_size = cur_start - static_cast<size_t>(is_space) - cur_boundary + 1;

                                if (component_size == 0) {
                                    ++cur_boundary;
                                    continue;
                                }

                                switch (info_state) {

                                    case info_state::COMPONENT_1: {

                                        if constexpr (std::is_same_v<ReprT, http::request>) {

                                            const auto match_result = method::from_string({ component_start, component_size });

                                            if (!match_result) {
                                                return std::nullopt;
                                            }

                                            parser_result.method = *match_result;
                                        }
                                        else {

                                            const auto match_result = version::from_string({ component_start, component_size });

                                            if (!match_result) {
                                                return std::nullopt;
                                            }

                                            parser_result.version = *match_result;
                                        }

                                        info_state = info_state::COMPONENT_2;
                                        break;
                                    }

                                    case info_state::COMPONENT_2: {

                                        if constexpr (std::is_same_v<ReprT, http::request>) {
                                            parser_result.uri = { component_start, component_size };
                                        }
                                        else {

                                            if (const auto conv = std::from_chars(component_start, component_start + component_size, parser_result.code); conv.ec != std::errc{}) {
                                                return std::nullopt;
                                            }
                                        }

                                        info_state = info_state::COMPONENT_3;
                                        break;
                                    }

                                    case info_state::COMPONENT_3: {

                                        if constexpr (std::is_same_v<ReprT, http::request>) {

                                            const auto match_result = version::from_string({ component_start, component_size });

                                            if (!match_result) {
                                                return std::nullopt;
                                            }

                                            parser_result.version = *match_result;
                                        }
                                        else {
                                            const size_t remainder_size = cur_end - cur_boundary + 1;
                                            parser_result.status_text = { component_start, remainder_size };
                                        }

                                        info_state = info_state::END;
                                        break;
                                    }

                                    default: {
                                        UNREACHABLE;
                                    }
                                }

                                cur_boundary = cur_start + static_cast<size_t>(is_space);
                            }
                        }

                        if (info_state != info_state::END) {
                            return std::nullopt;
                        }

                        // Fix cursor after loop exit
                        cur_start = cur_end;
                        parser_state = parser_state::HEADERS;
                        break;
                    }

                    case parser_state::HEADERS: {

                        const auto line_start = req.data() + cur_start;
                        const size_t line_size = cur_end - cur_start + 1;

                        size_t key_offset = 0;
                        for (; key_offset < line_size && line_start[key_offset] == ' '; ++key_offset) {}

                        if (key_offset == line_size) {
                            break;
                        }

                        const auto key_start = line_start + key_offset;

                        size_t key_size = 0;
                        for (; key_offset + key_size < line_size && (key_start[key_size] != ' ' && key_start[key_size] != ':'); ++key_size) {}

                        if (key_size == 0 || key_offset + key_size == line_size) {
                            break;
                        }

                        size_t value_offset = key_offset + key_size;
                        for (; value_offset < line_size && (line_start[value_offset] == ' ' || line_start[value_offset] == ':'); ++value_offset) {}

                        if (value_offset == line_size) {
                            break;
                        }

                        const auto value_start = line_start + value_offset;

                        size_t value_size = 0;
                        for (; value_offset + value_size < line_size && value_start[value_size] != ' '; ++value_size) {}

                        if (value_size == 0) {
                            break;
                        }

                        parser_result.headers[{key_start, key_size}] = {value_start, value_size};
                        break;
                    }

                    default: {
                        UNREACHABLE;
                    }
                }
            }

            if (parser_state != parser_state::DATA) {
                return std::nullopt;
            }

            if (const size_t data_size = req.size() - cur_end; data_size > 0) {
                parser_result.data = {req.data() + cur_end, data_size};
            }

            return parser_result;
        }

        template <typename ReprT, typename =
            nonstd::enable_if_t<
                detail::is_any_of_v<ReprT, http::request, http::response>
            >
        >
        static std::string serialize(const ReprT& frame) {

            std::string result;

            if constexpr (std::is_same_v<ReprT, http::request>) {
                result = helpers::view_join(
                    " ",
                    { http::method::to_string(frame.method), frame.uri, http::version::to_string(frame.version) }
                );
            }
            else {
                result = helpers::view_join(
                    " ",
                    { http::version::to_string(frame.version), std::to_string(frame.code), frame.status_text }
                );
            }

            result.append(LINE_END);

            for (const auto& [key, value] : frame.headers) {
                result.append(helpers::view_join(": ", { key, value }));
                result.append(LINE_END);
            }

            result.append(LINE_END);

            if (!frame.data.empty()) {
                result.append(frame.data);
            }

            return result;
        }
    };
}
