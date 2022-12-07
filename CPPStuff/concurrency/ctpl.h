#pragma once
#include <atomic>
#include <cassert>
#include <chrono>
#include <condition_variable>
#include <deque>
#include <functional>
#include <future>
#include <queue>
#include <thread>
#include <unordered_map>
#include <vector>

#include "../utils/macro.h"
#include "../utils/meta.h"

namespace ctpl {

    template <typename T>
    struct future_deque : std::deque<T> {

        static_assert(detail::is_specialization_v<T, std::future>);
        using awaited_type = typename detail::type_constructor_of<T>::arguments::template at<0>;

        using std::deque<T>::deque;
        using std::deque<T>::operator=;

        DEFAULT_COPY_ASSIGN_AND_MOVE(future_deque);

        nonstd::conditional_t<std::is_void_v<awaited_type>, void, std::deque<awaited_type>> get_all() {

            if constexpr (std::is_void_v<awaited_type>) {

                while (!this->empty()) {
                    this->front().get();
                    this->pop_front();
                }

                return;
            }
            else {

                std::deque<awaited_type> results;

                while (!this->empty()) {
                    results.emplace_back(this->front.get());
                    this->pop_front();
                }

                return results;
            }
        }

        void wait_all() const {

            for (auto it = this->cbegin(); it != this->cend(); ++it) {
                it->wait();
            }
        }

        template <class Rep, class Period>
        std::future_status wait_all(const std::chrono::duration<Rep, Period>& timeout_duration) const {

            std::chrono::duration<Rep, std::milli> timeout_ms;

            if constexpr (std::is_same_v<Period, std::milli>) {
                timeout_ms = timeout_duration;
            }
            else {
                timeout_ms = std::chrono::duration_cast<decltype(timeout_ms)>(timeout_duration);
            }

            for (auto it = this->cbegin(); it != this->cend(); ++it) {

                const auto start = std::chrono::steady_clock::now();
                const auto status = it->wait_for(timeout_ms);
                const auto delta_ms = std::chrono::duration_cast<decltype(timeout_ms)>(std::chrono::steady_clock::now() - start);

                if (status == std::future_status::timeout) {
                    return status;
                }

                if (delta_ms > timeout_ms) {

                    if (std::next(it) == this->cend()) {
                        break;
                    }

                    return std::future_status::timeout;
                }

                timeout_ms -= delta_ms;
            }

            return std::future_status::ready;
        }
    };

    template <typename Key, typename Value>
    struct future_map : std::unordered_map<Key, Value> {

        static_assert(detail::is_specialization_v<Value, std::future>);
        using awaited_type = typename detail::type_constructor_of<Value>::arguments::template at<0>;

        using std::unordered_map<Key, Value>::unordered_map;
        using std::unordered_map<Key, Value>::operator=;

        DEFAULT_COPY_ASSIGN_AND_MOVE(future_map);

        nonstd::conditional_t<std::is_void_v<awaited_type>, void, std::unordered_map<Key, awaited_type>> get_all() {

            if constexpr (std::is_void_v<awaited_type>) {

                while (!this->empty()) {
                    this->extract(this->cbegin()).mapped().get();
                }

                return;
            }
            else {

                std::unordered_map<Key, awaited_type> results;

                while (!this->empty()) {
                    auto nh = this->extract(this->cbegin());
                    results.try_emplace(nonstd::move(std::ignore, nh.key()), nh.mapped().get());
                }

                return results;
            }
        }

        void wait_all() const {

            for (auto it = this->cbegin(); it != this->cend(); ++it) {
                it->second.wait();
            }
        }

        template <class Rep, class Period>
        std::future_status wait_all(const std::chrono::duration<Rep, Period>& timeout_duration) const {

            std::chrono::duration<Rep, std::milli> timeout_ms;

            if constexpr (std::is_same_v<Period, std::milli>) {
                timeout_ms = timeout_duration;
            }
            else {
                timeout_ms = std::chrono::duration_cast<decltype(timeout_ms)>(timeout_duration);
            }

            for (auto it = this->cbegin(); it != this->cend(); ++it) {

                const auto start = std::chrono::steady_clock::now();
                const auto status = it->second.wait_for(timeout_ms);
                const auto delta_ms = std::chrono::duration_cast<decltype(timeout_ms)>(std::chrono::steady_clock::now() - start);

                if (status == std::future_status::timeout) {
                    return status;
                }

                if (delta_ms > timeout_ms) {

                    if (std::next(it) == this->cend()) {
                        break;
                    }

                    return std::future_status::timeout;
                }

                timeout_ms -= delta_ms;
            }

            return std::future_status::ready;
        }
    };

    enum class congestion_ctrl : uint8_t { OFF, ON };

    class thread_pool {

        std::vector<std::thread> m_threads;
        const congestion_ctrl m_congestion_ctrl;

        std::queue<std::function<void()>> m_tasks;
        volatile size_t m_tasks_total = 0;

        std::atomic_bool m_running = false;
        volatile bool m_waiting = false;

        std::condition_variable m_task_ready;
        std::condition_variable m_task_done;
        mutable std::mutex m_mutex;

    public:
        explicit thread_pool(
            const congestion_ctrl congestion_ctrl = congestion_ctrl::OFF,
            const size_t thread_count = std::thread::hardware_concurrency() > 1 ? std::thread::hardware_concurrency() : 2
        ) :
            m_congestion_ctrl{ congestion_ctrl }
        {
            assert(thread_count > 0);
            create_threads(thread_count);
        }

        DISALLOW_COPY_ASSIGN_AND_MOVE(thread_pool);

        ~thread_pool() {
            wait_for_tasks();
            destroy_threads();
        }

        template <typename F, typename... Args, typename R = std::invoke_result_t<F, Args...>>
        std::future<R> submit(F&& f, Args&&... args) {

            std::function<R()> task;

            if constexpr (sizeof...(Args) == 0) {
                task = std::forward<F>(f);
            }
            else {
                task = std::bind(std::forward<F>(f), std::forward<Args>(args)...);
            }

            std::shared_ptr<std::promise<R>> promise = std::make_shared<std::promise<R>>();

            push_task(
                [task = nonstd::move(task), promise]() mutable {

                    try {

                        if constexpr (std::is_void_v<R>) {
                            task();
                            promise->set_value();
                        }
                        else {
                            promise->set_value(task());
                        }
                    }
                    catch (...) {

                        try {
                            promise->set_exception(std::current_exception());
                        }
                        catch (...) {}
                    }
                }
            );

            return promise->get_future();
        }

        void wait_for_tasks() {

            std::unique_lock lock(m_mutex);

            m_waiting = true;
            m_task_done.wait(lock, [this] { return m_tasks_total == 0; });
            m_waiting = false;
        }

    private:
        void create_threads(const size_t thread_count) {

            m_threads.reserve(thread_count);
            m_running.store(true, std::memory_order_relaxed);

            for (size_t i = 0; i < thread_count; ++i) {
                m_threads.emplace_back(&thread_pool::worker, this);
            }
        }

        void destroy_threads() {

            m_running = false;
            m_task_ready.notify_all();

            for (auto& thread : m_threads) {

                if (thread.joinable()) {
                    thread.join();
                }
            }

            m_threads.clear();
        }

        template <typename F>
        void push_task(F&& task) {

            std::unique_lock lock(m_mutex);

            if (operators::enum_to_integral(m_congestion_ctrl)) {
                m_waiting = true;
                m_task_done.wait(lock, [this] { return m_tasks_total <= m_threads.size(); });
                m_waiting = false;
            }

            m_tasks.push(std::forward<F>(task));
            ++m_tasks_total;

            m_task_ready.notify_one();
        }

        void worker() {

            while (m_running) {

                std::unique_lock lock(m_mutex);
                m_task_ready.wait(lock, [this] { return !m_tasks.empty() || !m_running; });

                if (!m_running) {
                    break;
                }

                decltype(m_tasks)::value_type task = nonstd::move(m_tasks.front());
                m_tasks.pop();

                lock.unlock();
                task();

                lock.lock();
                --m_tasks_total;

                if (m_waiting) {
                    m_task_done.notify_one();
                }
            }
        }
    };

    class task_builder {

        using pool_type = ctpl::thread_pool;

        template <typename F, class T>
        class _member_fn {

            F T::* m_member;
            std::shared_ptr<T> m_object;
            std::reference_wrapper<const std::atomic<pool_type*>> m_pool;

        public:
            _member_fn(
                decltype(m_member) member,
                decltype(m_object) object,
                decltype(m_pool) pool
            ) noexcept :
                m_member{ member },
                m_object{ nonstd::move(object) },
                m_pool{ pool }
            {}

            DISALLOW_COPY_ASSIGN_AND_MOVE(_member_fn);

            ~_member_fn() = default;

            template <typename... Args, typename R = typename detail::function_type<F>::return_type>
            std::future<R> operator()(Args&&... args) {

                auto pool_ptr = m_pool.get().load(std::memory_order_acquire);

                if (!pool_ptr || !m_object) {
                    return {};
                }

                return pool_ptr->submit(m_member, m_object, std::forward<Args>(args)...);
            }
        };

        template <typename F>
        class _local_fn {

            using func_type = typename detail::function_type<F>::as_object;
            std::shared_ptr<func_type> m_function;
            std::reference_wrapper<const std::atomic<pool_type*>> m_pool;

        public:
            _local_fn(
                decltype(m_function) function,
                decltype(m_pool) pool
            ) noexcept :
                m_function{ nonstd::move(function) },
                m_pool{ pool }
            {}

            DISALLOW_COPY_ASSIGN_AND_MOVE(_local_fn);

            ~_local_fn() = default;

            template <typename... Args, typename R = typename detail::function_type<F>::return_type>
            std::future<R> operator()(Args&&... args) {

                auto pool_ptr = m_pool.get().load(std::memory_order_acquire);

                if (!pool_ptr || !m_function) {
                    return {};
                }

                return pool_ptr->submit(&func_type::operator(), m_function, std::forward<Args>(args)...);
            }
        };

    public:
        explicit task_builder(
            const ctpl::congestion_ctrl congestion_ctrl = ctpl::congestion_ctrl::OFF,
            const size_t thread_count = std::thread::hardware_concurrency() > 1 ? std::thread::hardware_concurrency() : 2
        ) {
            m_pool = new (&m_pool_storage) pool_type(congestion_ctrl, thread_count);
        }

        DISALLOW_COPY_ASSIGN_AND_MOVE(task_builder);

        ~task_builder() {
            joining_close();
        }

        void joining_close() {

            std::call_once(m_pool_destroyed, [this] {
                std::destroy_at(m_pool.exchange(nullptr, std::memory_order_acq_rel));
            });
        }

        template <typename F, class T>
        _member_fn<F, T> build(F T::* member, std::shared_ptr<T> object) noexcept {
            return { member, nonstd::move(object), std::cref(m_pool) };
        }

        template <typename F>
        _local_fn<F> build(F&& function) noexcept {

            try {
                using func_type = typename detail::function_type<F>::as_object;
                return { std::make_shared<func_type>(std::forward<F>(function)), std::cref(m_pool) };
            }
            catch (const std::bad_alloc&) {
                return { nullptr, std::cref(m_pool) };
            }
        }

        void wait_for_tasks() const {

            if (const auto pool_ptr = m_pool.load(std::memory_order_relaxed); pool_ptr) {
                pool_ptr->wait_for_tasks();
            }
        }

    private:
        alignas(pool_type) std::byte m_pool_storage[sizeof(pool_type)];
        std::atomic<pool_type*> m_pool = nullptr;
        std::once_flag m_pool_destroyed;
    };
}
