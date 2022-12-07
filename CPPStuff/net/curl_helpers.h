#pragma once
#include <functional>
#include <iterator>
#include <numeric>
#include <variant>

#define CURL_STATICLIB 1
#define CURL_NO_OLDIES 1
#include "./curl/curl.h"

#include "../utils/memory.h"
#include "../utils/meta.h"

#define HTTP_SUCCEEDED(_code_) ((_code_) >= 200 && (_code_) <= 299)

namespace curl_helpers {

    // Handles
    using curl_unique_hptr = fn_unique_ptr<CURL, curl_easy_cleanup>;
    using curl_mime_unique_hptr = fn_unique_ptr<curl_mime, curl_mime_free>;
    using curl_slist_unique_hptr = fn_unique_ptr<curl_slist, curl_slist_free_all>;
    using curlu_unique_hptr = fn_unique_ptr<CURLU, curl_url_cleanup>;

    // Generic
    using curl_unique_mem = fn_unique_ptr<char, curl_free>;
}

namespace utility {
    struct curl_mimepart_sep {};
}

template <class Iterator>
nonstd::enable_if_t<
    std::is_same_v<
        typename std::iterator_traits<Iterator>::value_type,
        std::function<CURLcode()>
    >,
    CURLcode
>
curl_easy_oper_all(Iterator it_begin, Iterator it_end) {

    return std::accumulate(
        it_begin,
        it_end,
        CURLE_OK,
        [](const CURLcode acc, const std::function<CURLcode()>& next) -> CURLcode {

            if (acc != CURLE_OK) {
                return acc;
            }

            return next();
        }
    );
}

template <class Iterator>
nonstd::enable_if_t<
    std::is_same_v<
        typename std::iterator_traits<Iterator>::value_type,
        std::function<CURLUcode(CURLU*)>
    >,
    CURLUcode
>
curl_url_oper_all(Iterator it_begin, Iterator it_end) {

    const curl_helpers::curlu_unique_hptr curlu(curl_url());

    if (!curlu) {
        return CURLUE_BAD_HANDLE;
    }

    return std::accumulate(
        it_begin,
        it_end,
        CURLUE_OK,
        [curlu = curlu.get()](const CURLUcode acc, const std::function<CURLUcode(CURLU*)>& next) -> CURLUcode {

            if (acc != CURLUE_OK) {
                return acc;
            }

            return next(curlu);
        }
    );
}

template <class Iterator>
nonstd::enable_if_t<
    std::is_same_v<
        typename std::iterator_traits<Iterator>::value_type,
        std::variant<
            std::function<CURLcode(curl_mimepart*)>,
            utility::curl_mimepart_sep
        >
    >,
    std::pair<curl_mime*, CURLcode>
>
curl_mime_oper_all(CURL& curl, Iterator it_begin, Iterator it_end) {

    curl_mime* mimepost = curl_mime_init(&curl);

    if (mimepost == nullptr) {
        return {nullptr, CURLE_FAILED_INIT};
    }

    const auto [_, code] = std::accumulate(
        it_begin,
        it_end,
        std::pair(curl_mime_addpart(mimepost), CURLE_OK),
        [&mimepost](
            const std::pair<curl_mimepart*, CURLcode>& acc,
            const std::variant<std::function<CURLcode(curl_mimepart*)>, utility::curl_mimepart_sep>& next)
        -> std::pair<curl_mimepart*, CURLcode> {

            if (acc.second != CURLE_OK) {
                return acc;
            }

            if (acc.first == nullptr) {
                return {nullptr, CURLE_OUT_OF_MEMORY};
            }

            if (std::holds_alternative<utility::curl_mimepart_sep>(next)) {
                return {curl_mime_addpart(mimepost), CURLE_OK};
            }

            return {acc.first, std::get<0>(next)(acc.first)};
        }
    );

    return {mimepost, code};
}

template <class Iterator>
nonstd::enable_if_t<
    std::is_same_v<
        typename std::iterator_traits<Iterator>::value_type,
        std::function<curl_slist*(curl_slist*)>
    >,
    std::pair<curl_slist*, bool>
>
curl_slist_append_all(Iterator it_begin, Iterator it_end) {

    return std::accumulate(
        it_begin,
        it_end,
        std::pair(static_cast<curl_slist*>(nullptr), false),
        [](const std::pair<curl_slist*, bool>& acc, const std::function<curl_slist* (curl_slist*)>& next) -> std::pair<curl_slist*, bool> {

            if (acc.second) {
                return acc;
            }

            const auto slist_next = next(acc.first);
            return {slist_next == nullptr ? acc.first : slist_next, slist_next == nullptr};
        }
    );
}
