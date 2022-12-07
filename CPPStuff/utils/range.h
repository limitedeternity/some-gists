#pragma once

namespace range {

    template <typename T>
    class of {

        typename T::iterator m_begin;
        typename T::iterator m_end;

    public:
        of(T& object) : m_begin{ object.begin() }, m_end{ object.end() } {}

        auto begin() { return m_begin; }
        auto end() { return m_end; }
    };

    template <typename T>
    class of_const {

        typename T::const_iterator m_begin;
        typename T::const_iterator m_end;

    public:
        of_const(const T& object) : m_begin{ object.cbegin() }, m_end{ object.cend() } {}

        auto begin() const { return m_begin; }
        auto end() const { return m_end; }
    };
}
