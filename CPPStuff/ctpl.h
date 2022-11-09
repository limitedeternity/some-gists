#pragma once
#include "defines/cxx.h"
#include "detail_api.h"

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
}
