#pragma once
#include <concurrent_queue.h>
#include <atomic>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>

#include "./base.hpp"
#include "../concurrency/thread.h"
#include "../utils/meta.h"

namespace ws {
    enum class message_type { TEXT, BINARY };
}

template <ws::message_type MsgT>
class websocket_t final : websocket_base_t {
public:
    using container_type = nonstd::conditional_t<
        MsgT == ws::message_type::TEXT,
        std::string,
        std::vector<u8>
    >;

    using opcode_constant = nonstd::conditional_t<
        MsgT == ws::message_type::TEXT,
        std::integral_constant<ws::opcode, ws::opcode::TEXT>,
        std::integral_constant<ws::opcode, ws::opcode::BINARY>
    >;

private:
    concurrency::concurrent_queue<container_type> m_outgoing;
    concurrency::concurrent_queue<container_type> m_incoming;

    std::atomic_bool m_running = false;
    thread::suspended m_io_thread;

public:
    websocket_t() : websocket_base_t(), m_io_thread{
        [this] {
            while (m_running.load(std::memory_order_relaxed)) {

                if (get_socket_state() == ws::socket_state::CLOSED) {
                    m_running.store(false, std::memory_order_release);
                    break;
                }

                if (container_type data; m_outgoing.try_pop(data)) {
                    push_message<opcode_constant::value>(data);
                }

                perform_io();

                get_message<container_type>([this](const auto& message) {
                    m_incoming.push(message);
                });

                std::this_thread::sleep_for(10ms);
            }

            push_message<ws::opcode::CLOSE>();
            perform_io();
        }
    } {}

    DISALLOW_COPY_AND_ASSIGN(websocket_t);
    DEFAULT_MOVE_AND_ASSIGN(websocket_t);

    ~websocket_t() noexcept override {
        m_running.store(false, std::memory_order_relaxed);
    }

    bool alive() const {
        return m_running.load(std::memory_order_acquire);
    }

    CURLcode connect(const char* url, const std::unordered_map<std::string, std::string>& custom_headers = {}, const long timeout_ms = 2000L) override {

        if (const auto code = websocket_base_t::connect(url, custom_headers, timeout_ms); code != CURLE_OK) {
            return code;
        }

        m_running.store(true, std::memory_order_relaxed);
        m_io_thread.start();

        return CURLE_OK;
    }

    void send(const container_type& message) {
        m_outgoing.push(message);
    }

    bool recv(container_type& message) {
        return m_incoming.try_pop(message);
    }
};
