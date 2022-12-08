#pragma once
#include <memory>

#include "./macro.h"
#include "./wrappers.h"

template <typename T, auto fn>
using fn_unique_ptr = std::unique_ptr<T, functor_from_fn<fn>>;

namespace memory {

    template <typename T, class D = std::default_delete<T>>
    class out_ptr {

        std::unique_ptr<T, D>& m_owner;
        T* m_ptr;

    public:
        explicit out_ptr(std::unique_ptr<T, D>& owner) :
            m_owner{ owner },
            m_ptr{ m_owner.release() }
        {}

        DISALLOW_COPY_ASSIGN_AND_MOVE(out_ptr);

        ~out_ptr() {
            m_owner.reset(m_ptr);
        }

        operator T*() const {
            return m_ptr;
        }

        operator T**() {

            if (m_ptr) {
                D deleter;
                deleter(m_ptr);
                m_ptr = nullptr;
            }

            return &m_ptr;
        }
    };
}
