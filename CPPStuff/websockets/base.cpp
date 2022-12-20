#include "./base.hpp"
#include "../http/frame.hpp"
#include "../utils/memory.h"

int websocket_base_t::socket_wait(ws::wait_for wait_for, long timeout_ms) const {

    if (timeout_ms < 0) {
        timeout_ms = 0;
    }

    fd_set rfds;
    fd_set wfds;

    FD_ZERO(&rfds);
    FD_ZERO(&wfds);

    if (operators::enum_to_integral(wait_for & ws::wait_for::RECV)) {
        FD_SET(m_sockfd, &rfds);
    }

    if (operators::enum_to_integral(wait_for & ws::wait_for::SEND)) {
        FD_SET(m_sockfd, &wfds);
    }

    const timeval tv = {timeout_ms / 1000, timeout_ms % 1000 * 1000};
    return select(static_cast<int>(m_sockfd) + 1, &rfds, &wfds, nullptr, &tv);
}

template <class Container>
nonstd::enable_if_t<
    std::conjunction_v<
        detail::is_contiguous_container<Container>,
        detail::is_byte_container<Container>
    >,
    CURLcode
>
websocket_base_t::socket_send(Container& buffer, long timeout_ms) {

    if (m_socket_state == ws::socket_state::CLOSED) {
        return CURLE_COULDNT_CONNECT;
    }

    CURLcode code = CURLE_OK;
    size_t bytes_sent_total = 0;

    while (bytes_sent_total < buffer.size()) {

        size_t bytes_sent = 0;
        code = curl_easy_send(
            m_curl.get(),
            buffer.data() + bytes_sent_total,
            buffer.size() - bytes_sent_total,
            &bytes_sent
        );

        bytes_sent_total += bytes_sent;

        // => zero bytes sent && zero fucks given
        if (code == CURLE_AGAIN && timeout_ms <= 0) {
            break;
        }

        // => zero bytes sent && we ought to wait
        if (code == CURLE_AGAIN && timeout_ms > 0) {

            const int wait_result = socket_wait(ws::wait_for::SEND, timeout_ms);

            // => socket failed
            if (wait_result < 0) {
                code = CURLE_UNRECOVERABLE_POLL;
                socket_close();
                break;
            }

            // => socket isn't writable
            if (wait_result == 0) {
                code = CURLE_OPERATION_TIMEDOUT;
                break;
            }

            // => socket is writable
            continue;
        }

        // => unhandled error condition
        if (code != CURLE_OK) {
            socket_close();
            break;
        }
    }

    if (bytes_sent_total > 0) {
        buffer.erase(buffer.begin(), buffer.begin() + bytes_sent_total);
    }

    return code;
}

template <class Container>
nonstd::enable_if_t<
    std::conjunction_v<
        detail::is_contiguous_container<Container>,
        detail::is_byte_container<Container>
    >,
    CURLcode
>
websocket_base_t::socket_recv(Container& buffer, long timeout_ms) {

    if (m_socket_state == ws::socket_state::CLOSED) {
        return CURLE_COULDNT_CONNECT;
    }

    CURLcode code = CURLE_OK;
    size_t bytes_read_total = 0;

    while (true) {

        constexpr size_t MTU_DEFAULT = 1500;
        const auto init_buffer_size = buffer.size();

        buffer.resize(init_buffer_size + MTU_DEFAULT);

        size_t bytes_read = 0;
        code = curl_easy_recv(
            m_curl.get(),
            buffer.data() + init_buffer_size,
            buffer.size() - init_buffer_size,
            &bytes_read
        );

        bytes_read_total += bytes_read;

        if (init_buffer_size + bytes_read < buffer.size()) {
            buffer.resize(init_buffer_size + bytes_read);
        }

        // => zero bytes read && zero fucks given
        if (code == CURLE_AGAIN && timeout_ms <= 0) {
            break;
        }

        // => zero bytes read && we ought to wait
        if (code == CURLE_AGAIN && timeout_ms > 0) {

            // => we have some data already
            if (bytes_read_total > 0) {
                code = CURLE_OK;
                break;
            }

            const int wait_result = socket_wait(ws::wait_for::RECV, timeout_ms);

            // => socket failed
            if (wait_result < 0) {
                code = CURLE_UNRECOVERABLE_POLL;
                socket_close();
                break;
            }

            // => socket isn't readable
            if (wait_result == 0) {
                code = CURLE_OPERATION_TIMEDOUT;
                break;
            }

            // => socket is readable
            continue;
        }

        // => connection closed
        if (code == CURLE_OK && bytes_read == 0) {
            code = CURLE_GOT_NOTHING;
        }

        // => unhandled error condition
        if (code != CURLE_OK) {
            socket_close();
            break;
        }
    }

    return code;
}

CURLcode websocket_base_t::connect(const char* url, const std::unordered_map<std::string, std::string>& custom_headers, const long timeout_ms) {

    if (m_socket_state != ws::socket_state::CLOSED) {
        return CURLE_OK;
    }

    reset_buffers();
    m_curl.reset(curl_easy_init());

    if (!m_curl) {
        return CURLE_FAILED_INIT;
    }

    curl_helpers::curl_unique_mem url_subst;
    curl_helpers::curl_unique_mem host;
    curl_helpers::curl_unique_mem port;
    curl_helpers::curl_unique_mem path;

    {
        const auto url_parsing_steps = nonstd::make_array<std::function<CURLUcode(CURLU*)>>(
            [url](CURLU* curlu) {
                return curl_url_set(curlu, CURLUPART_URL, url, CURLU_NON_SUPPORT_SCHEME | CURLU_DEFAULT_SCHEME);
            },
            [](CURLU* curlu) {

                curl_helpers::curl_unique_mem scheme;

                if (const auto code = curl_url_get(curlu, CURLUPART_SCHEME, memory::out_ptr(scheme), 0); code != CURLUE_OK) {
                    return code;
                }

                if (_stricmp(scheme.get(), "http") == 0 || _stricmp(scheme.get(), "https") == 0) {
                    return CURLUE_OK;
                }

                if (_stricmp(scheme.get(), "ws") == 0) {
                    return curl_url_set(curlu, CURLUPART_SCHEME, "http", 0);
                }

                if (_stricmp(scheme.get(), "wss") == 0) {
                    return curl_url_set(curlu, CURLUPART_SCHEME, "https", 0);
                }

                return CURLUE_BAD_SCHEME;
            },
            [&url_subst](CURLU* curlu) {
                return curl_url_get(curlu, CURLUPART_URL, memory::out_ptr(url_subst), 0);
            },
            [&host](CURLU* curlu) {
                return curl_url_get(curlu, CURLUPART_HOST, memory::out_ptr(host), 0);
            },
            [&port](CURLU* curlu) {
                return curl_url_get(curlu, CURLUPART_PORT, memory::out_ptr(port), CURLU_DEFAULT_PORT);
            },
            [&path](CURLU* curlu) {
                return curl_url_get(curlu, CURLUPART_PATH, memory::out_ptr(path), 0);
            }
        );

        if (const auto code = curl_url_oper_all(url_parsing_steps.cbegin(), url_parsing_steps.cend()); code != CURLUE_OK) {

            switch (code) {

                case CURLUE_BAD_HANDLE:
                case CURLUE_BAD_PARTPOINTER:
                case CURLUE_OUT_OF_MEMORY:
                    return CURLE_OUT_OF_MEMORY;

                default:
                    return CURLE_URL_MALFORMAT;
            }
        }
    }

    std::string sec_key = ws_generate_key();
    std::string request;

    {
        const std::string owned_path = MOVING_BIND(path).get();
        const std::string host_plus_port = helpers::view_join(":", { MOVING_BIND(host).get(), MOVING_BIND(port).get() });

        http::request request_object{
            http::method::GET,
            owned_path,
            http::version::HTTP_1_1,
            {
                {"Host", host_plus_port},
                {"Connection", "keep-alive, Upgrade"},
                {"Upgrade", "websocket"},
                {"Cache-Control", "no-cache"},
                {"Pragma", "no-cache"},
                {"Sec-WebSocket-Version", "13"},
                {"Sec-WebSocket-Key", sec_key}
            },
            {}
        };

        request_object.headers.insert(custom_headers.cbegin(), custom_headers.cend());
        request = http::frame::serialize(request_object);
    }

    std::string sec_accept_key = ws_derive_accept_key(MOVING_BIND(sec_key));
    std::string response;

    {
        const auto curl_operation_steps = nonstd::make_array<std::function<CURLcode()>>(
            [&url_subst, this] {
                return curl_easy_setopt(m_curl.get(), CURLOPT_URL, MOVING_BIND(url_subst).get());
            },
            [this] {
                return curl_easy_setopt(m_curl.get(), CURLOPT_CONNECT_ONLY, 1L);
            },
            [this] {
                return curl_easy_perform(m_curl.get());
            },
            [this] {

                if (const auto code = curl_easy_getinfo(m_curl.get(), CURLINFO_ACTIVESOCKET, &m_sockfd); code != CURLE_OK) {
                    return code;
                }

                m_socket_state = ws::socket_state::OPEN;
                return CURLE_OK;
            },
            [&request, timeout_ms, this] {
                return socket_send(request, timeout_ms);
            },
            [&response, timeout_ms, this] {
                return socket_recv(response, timeout_ms);
            }
        );

        if (const auto code = curl_easy_oper_all(curl_operation_steps.cbegin(), curl_operation_steps.cend()); code != CURLE_OK) {
            return code;
        }
    }

    const auto response_object = http::frame::parse<http::response>(response);

    if (!response_object || response_object->code != 101) {
        return CURLE_WEIRD_SERVER_REPLY;
    }

    if (const auto sec_accept_header = response_object->headers.find("Sec-WebSocket-Accept");
        sec_accept_header == response_object->headers.cend() || MOVING_BIND(sec_accept_key) != sec_accept_header->second)
    {
        return CURLE_PEER_FAILED_VERIFICATION;
    }

    return CURLE_OK;
}

void websocket_base_t::perform_io(long timeout_ms) {

    if (m_socket_state == ws::socket_state::CLOSED) {
        return;
    }

    const int wait_result = socket_wait(!m_txbuf.empty() ? ws::wait_for::RECV | ws::wait_for::SEND : ws::wait_for::RECV, timeout_ms);

    // => socket failed
    if (wait_result < 0) {
        return socket_close();
    }

    // => socket isn't available
    if (wait_result == 0) {
        return;
    }

    socket_recv(m_rxbuf, 0);
    socket_send(m_txbuf, 0);

    if (m_txbuf.empty() && m_socket_state == ws::socket_state::CLOSING) {
        return socket_close();
    }
}

std::string websocket_base_t::ws_generate_key() {

    std::array<u8, sizeof(u32be) * 4> bytes;

    for (size_t i = 0; i < bytes.size() / sizeof(u32be); ++i) {
        u32be key_part = s_rand();
        std::copy_n(key_part.as_bytes, sizeof(key_part), bytes.begin() + i * sizeof(key_part));
    }

    return hash::base64::encode(bytes);
}

std::string websocket_base_t::ws_derive_accept_key(const std::string_view ws_key) {
    return hash::base64::encode(hash::sha1::bytes(std::string(ws_key) + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"));
}

std::vector<u8> websocket_base_t::ws_build_header(ws::opcode opcode, u64 message_size, const u32be& mask) {

    std::vector<u8> header(static_cast<size_t>(2) + (message_size >= 126 ? 2 : 0) + (message_size >= 65536 ? 6 : 0) + 4, 0);
    header[0] = operators::enum_to_integral(opcode) | 0x80;

    ptrdiff_t mask_offset = 0;

    if (message_size < 126) {
        header[1] = (message_size & 0xff) | 0x80;
        mask_offset = 2;
    }
    else if (message_size < 65536) {
        header[1] = 126 | 0x80;
        header[2] = (message_size >> 8) & 0xff;
        header[3] = (message_size >> 0) & 0xff;
        mask_offset = 4;
    }
    else {
        header[1] = 127 | 0x80;
        header[2] = (message_size >> 56) & 0xff;
        header[3] = (message_size >> 48) & 0xff;
        header[4] = (message_size >> 40) & 0xff;
        header[5] = (message_size >> 32) & 0xff;
        header[6] = (message_size >> 24) & 0xff;
        header[7] = (message_size >> 16) & 0xff;
        header[8] = (message_size >> 8) & 0xff;
        header[9] = (message_size >> 0) & 0xff;
        mask_offset = 10;
    }

    std::copy_n(mask.as_bytes, sizeof(mask), &header[mask_offset]);
    return header;
}

void websocket_base_t::ws_apply_mask(u8* __restrict data, u64 len, const u32be& mask) {
    for (decltype(len) i = 0; i < len; ++i) {
        data[i] ^= mask.as_bytes[i & (sizeof(mask) - 1)];
    }
}
