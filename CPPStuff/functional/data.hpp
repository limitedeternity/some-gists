#pragma once

namespace data {

    template <typename E, typename T>
    struct either : std::variant<E, T> {

        using left_type = E;
        using right_type = T;

        using std::variant<E, T>::variant;
        using std::variant<E, T>::operator=;

        left_type* try_left() {
            return std::get_if<0>(this);
        }

        const left_type* try_left() const {
            return std::get_if<0>(this);
        }

        right_type* try_right() {
            return std::get_if<1>(this);
        }

        const right_type* try_right() const {
            return std::get_if<1>(this);
        }

        left_type& left() {
            return *std::get_if<0>(this);
        }

        const left_type& left() const {
            return *std::get_if<0>(this);
        }

        right_type& right() {
            return *std::get_if<1>(this);
        }

        const right_type& right() const {
            return *std::get_if<1>(this);
        }
    };
}
