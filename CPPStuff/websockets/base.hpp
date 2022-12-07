#pragma once
#include <functional>
#include <random>

#include "../net/curl_helpers.h"
#include "../utils/macro.h"
#include "../utils/numeric.h"

namespace ws {

    enum class socket_state { CLOSED, OPEN, CLOSING };
    enum class wait_for : u8 { RECV = 1 << 0, SEND = 1 << 1 };

    DEFINE_ENUM_FLAG_OPERATORS(wait_for)

    // https://www.rfc-editor.org/rfc/rfc6455#section-11.8
    enum class opcode : u8 {

        // Non-control
        CONTINUATION = 0x0,
        TEXT = 0x1,
        BINARY = 0x2,

        // Control
        CLOSE = 0x8,
        PING = 0x9,
        PONG = 0xA
    };

    // https://www.rfc-editor.org/rfc/rfc6455#section-5.2
    #pragma pack(push, 1)
    struct header_t {

        u8 fin : 1;
        u8 rsv1 : 1;
        u8 rsv2 : 1;
        u8 rsv3 : 1;
        ws::opcode opcode : 4;

        u8 mask : 1;
        u8 payload_len : 7;

        u64 ext_payload_len;
        u32be masking_key;
    };
    #pragma pack(pop)
}

namespace detail {

    template <typename T>
    struct is_contiguous_container : std::false_type {};

    template <typename T, size_t N>
    struct is_contiguous_container<std::array<T, N>> : std::true_type {};

    template <typename T>
    struct is_contiguous_container<std::vector<T>> : std::true_type {};

    template <typename T>
    struct is_contiguous_container<std::basic_string<T>> : std::true_type {};

    template <class Container>
    using is_byte_container = std::bool_constant<sizeof(typename Container::value_type) == 1>;

    template <ws::opcode Opcode>
    using is_known_opcode = std::bool_constant<operators::in_range<0x0, 0x10>(operators::enum_to_integral(Opcode))>;
}

class websocket_base_t {

    curl_helpers::curl_unique_hptr m_curl;
    ws::socket_state m_socket_state = ws::socket_state::CLOSED;
    curl_socket_t m_sockfd = CURL_SOCKET_BAD;

    bool m_rx_bad = false;
    std::vector<u8> m_rxbuf;
    std::vector<u8> m_txbuf;
    std::vector<u8> m_recv_data;

    inline static struct mt19937_default : std::mt19937 {
        mt19937_default() : std::mt19937(std::random_device()()) {}
    } s_rand = {};

public:
    websocket_base_t() = default;

    DISALLOW_COPY_AND_ASSIGN(websocket_base_t);

    websocket_base_t(websocket_base_t&& other) noexcept {
        *this = std::move(other);
    }

    websocket_base_t& operator=(websocket_base_t&& other) noexcept {

        if (this != &other) {

            m_curl = std::move(other.m_curl);
            m_socket_state = other.m_socket_state;
            m_sockfd = other.m_sockfd;

            other.socket_close();

            m_rx_bad = other.m_rx_bad;
            m_rxbuf = std::move(other.m_rxbuf);
            m_txbuf = std::move(other.m_txbuf);
            m_recv_data = std::move(other.m_recv_data);

            other.reset_buffers();
        }

        return *this;
    }

    virtual ~websocket_base_t() = default;

    [[nodiscard]] auto get_socket_state() const {
        return m_socket_state;
    }

    virtual CURLcode connect(const char* url, const std::unordered_map<std::string, std::string>& custom_headers = {}, long timeout_ms = 2000L);
    void perform_io(long timeout_ms = 0L);

    template <ws::opcode Opcode, class Container = std::vector<u8>>
    nonstd::enable_if_t<
        std::conjunction_v<
            detail::is_known_opcode<Opcode>,
            detail::is_contiguous_container<Container>,
            detail::is_byte_container<Container>
        >,
        void
    >
    push_message(const Container& message = {}) {

        if (m_socket_state != ws::socket_state::OPEN) {
            return;
        }

        if constexpr (Opcode == ws::opcode::CLOSE) {
            m_socket_state = ws::socket_state::CLOSING;
        }

        const u32be mask = s_rand();
        const size_t message_size = message.size();

        const auto header = ws_build_header(Opcode, message_size, mask);
        m_txbuf.insert(m_txbuf.cend(), header.cbegin(), header.cend());

        if (message_size > 0) {
            m_txbuf.insert(m_txbuf.cend(), message.cbegin(), message.cend());
            ws_apply_mask(m_txbuf.data() + (m_txbuf.size() - message_size), message_size, mask);
        }
    }

    template <class Into>
    nonstd::enable_if_t<
        std::conjunction_v<
            detail::is_contiguous_container<Into>,
            detail::is_byte_container<Into>
        >,
        void
    >
    get_message(const std::function<void(const Into&)>& callback) {

        if (m_rx_bad) {
            return;
        }

        while (true) {

            if (m_rxbuf.size() < 2) {
                return; /* Need at least 2 bytes */
            }

            const auto rx_data = m_rxbuf.data();

            ws::header_t ws{
                IS_FLAG_ON(rx_data[0], 0x80),
                IS_FLAG_ON(rx_data[0], 0x40),
                IS_FLAG_ON(rx_data[0], 0x20),
                IS_FLAG_ON(rx_data[0], 0x10),
                static_cast<ws::opcode>(rx_data[0] & 0x0f),
                IS_FLAG_ON(rx_data[1], 0x80),
                static_cast<u8>(rx_data[1] & 0x7f),
                0,
                {0}
            };

            const size_t header_size = static_cast<size_t>(2) + (ws.payload_len == 126 ? 2 : 0) + (ws.payload_len == 127 ? 8 : 0) + (ws.mask ? 4 : 0);

            if (header_size > m_rxbuf.size()) {
                return; /* Need: rxbuf.size() >= header_size */
            }

            ptrdiff_t mask_offset = 0;

            if (ws.payload_len < 126) {
                ws.ext_payload_len = ws.payload_len;
                mask_offset = 2;
            }
            else if (ws.payload_len == 126) {
                ws.ext_payload_len |= static_cast<u64>(rx_data[2]) << 8;
                ws.ext_payload_len |= static_cast<u64>(rx_data[3]) << 0;
                mask_offset = 4;
            }
            else if (ws.payload_len == 127) {
                ws.ext_payload_len |= static_cast<u64>(rx_data[2]) << 56;
                ws.ext_payload_len |= static_cast<u64>(rx_data[3]) << 48;
                ws.ext_payload_len |= static_cast<u64>(rx_data[4]) << 40;
                ws.ext_payload_len |= static_cast<u64>(rx_data[5]) << 32;
                ws.ext_payload_len |= static_cast<u64>(rx_data[6]) << 24;
                ws.ext_payload_len |= static_cast<u64>(rx_data[7]) << 16;
                ws.ext_payload_len |= static_cast<u64>(rx_data[8]) << 8;
                ws.ext_payload_len |= static_cast<u64>(rx_data[9]) << 0;
                mask_offset = 10;

                if (ws.ext_payload_len & 0x8000000000000000ull) {
                    // https://tools.ietf.org/html/rfc6455 states that
                    // "the most significant bit MUST be 0."
                    //
                    // We can't drop the frame, because (1) we don't know
                    // how much data to skip over to find the next header,
                    // and (2) this would be an impractically long length, even
                    // if it were valid. So just close() and return immediately
                    // for now.
                    m_rx_bad = true;
                    socket_close();
                    return;
                }
            }

            if (ws.mask) {
                std::copy_n(rx_data + mask_offset, sizeof(ws.masking_key), ws.masking_key.as_bytes);
            }

            // Note: The checks above should hopefully ensure this addition
            //       cannot overflow:
            if (header_size + ws.ext_payload_len > m_rxbuf.size()) {
                return; /* Need: rxbuf.size() >= header_size + ws.ext_payload_len */
            }

            // We got a whole message, now do something with it:
            switch (ws.opcode) {

                case ws::opcode::TEXT:
                case ws::opcode::BINARY:
                case ws::opcode::CONTINUATION: {

                    if (ws.mask) {
                        ws_apply_mask(rx_data + header_size, ws.ext_payload_len, ws.masking_key);
                    }

                    m_recv_data.insert(m_recv_data.cend(), rx_data + header_size, rx_data + (header_size + ws.ext_payload_len));

                    if (ws.fin) {

                        if constexpr (std::is_same_v<Into, decltype(m_recv_data)>) {
                            callback(m_recv_data);
                        }
                        else {
                            callback({ m_recv_data.cbegin(), m_recv_data.cend() });
                        }

                        DEFAULT(m_recv_data);
                    }

                    break;
                }

                case ws::opcode::PING: {

                    if (ws.mask) {
                        ws_apply_mask(rx_data + header_size, ws.ext_payload_len, ws.masking_key);
                    }

                    push_message<ws::opcode::PONG>({ rx_data + header_size, rx_data + (header_size + ws.ext_payload_len) });
                    break;
                }

                case ws::opcode::PONG: {
                    DEBUG_INT3();
                    break;
                }

                case ws::opcode::CLOSE:
                default: {
                    socket_close();
                    break;
                }
            }

            m_rxbuf.erase(m_rxbuf.cbegin(), m_rxbuf.cbegin() + header_size + static_cast<size_t>(ws.ext_payload_len));
        }
    }

private:
    static std::string ws_generate_key();
    static std::string ws_derive_accept_key(std::string_view ws_key);
    static std::vector<u8> ws_build_header(ws::opcode opcode, u64 message_size, const u32be& mask);
    static void ws_apply_mask(u8* data, u64 len, const u32be& mask);

    [[nodiscard]] int socket_wait(ws::wait_for wait_for, long timeout_ms) const;

    template <class Container>
    nonstd::enable_if_t<
        std::conjunction_v<
            detail::is_contiguous_container<Container>,
            detail::is_byte_container<Container>
        >,
        CURLcode
    >
    socket_send(Container& buffer, long timeout_ms);

    template <class Container>
    nonstd::enable_if_t<
        std::conjunction_v<
            detail::is_contiguous_container<Container>,
            detail::is_byte_container<Container>
        >,
        CURLcode
    >
    socket_recv(Container& buffer, long timeout_ms);

    void socket_close() {
        DEFAULT(m_curl);
        m_socket_state = ws::socket_state::CLOSED;
        m_sockfd = CURL_SOCKET_BAD;
    }

    void reset_buffers() {
        m_rx_bad = false;
        DEFAULT(m_rxbuf);
        DEFAULT(m_txbuf);
        DEFAULT(m_recv_data);
    }
};
