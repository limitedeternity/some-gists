#pragma once
#include <future>
#include <thread>

#include "../utils/macro.h"

namespace thread {

    using basic = std::thread;

    struct scoped : basic {

        INHERIT_CONSTRUCTORS_AND_ASSIGN(basic);
        DEFAULT_COPY_ASSIGN_AND_MOVE(scoped);

        ~scoped() noexcept {

            if (joinable()) {
                join();
            }
        }
    };

    class suspended {

        std::promise<bool> m_promise;
        thread::scoped m_thread;

    public:
        template <typename F, typename... Args>
        explicit suspended(F&& f, Args&&... args) : m_thread{
            [
                future = m_promise.get_future(),
                routine = std::bind(std::forward<F>(f), std::forward<Args>(args)...)
            ]
            () mutable {

                if (future.get()) {
                    routine();
                }
            }
        } {}

        DISALLOW_COPY_AND_ASSIGN(suspended);
        DEFAULT_MOVE_AND_ASSIGN(suspended);

        ~suspended() {
            signal_future(false);
        }

        void start() {
            signal_future(true);
        }

    private:
        void signal_future(const bool value) {

            try {
                m_promise.set_value(value);
            }
            catch (const std::future_error& exc) {

                if (exc.code() != std::future_errc::promise_already_satisfied) {
                    throw;
                }
            }
        }
    };
}
