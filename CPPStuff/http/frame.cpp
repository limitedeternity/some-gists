#include "./frame.hpp"

std::string helpers::view_join(const std::string_view delim, const std::initializer_list<std::string_view> views) {

    if (views.size() == 0) {
        return {};
    }

    std::string result;
    auto iterator = views.begin();

    switch (enum class space_state { NO_DELIM, DELIM }; space_state::NO_DELIM) {
        case space_state::DELIM:      do { result.append(delim);
        case space_state::NO_DELIM:        result.append(*iterator);
                                      }
                                      while (++iterator != views.end());
    }

    return result;
}
