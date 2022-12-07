#pragma once
#include <cstdint>

using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;

template <typename T>
struct little_endian;

template <typename T>
struct big_endian;

#pragma pack(push, 1)
template <typename T>
struct little_endian {

    union {
        u8 as_bytes[sizeof(T)];
        T as_integer;
    };

    little_endian(const T t = {}) {
        *this = t;
    }

    little_endian(const little_endian<T>& other) {
        *this = other;
    }

    little_endian(const big_endian<T>& other) {
        *this = other;
    }

    operator T() const {

        T t = {};
        for (size_t i = 0; i < sizeof(T); ++i) {
            t |= static_cast<T>(as_bytes[i]) << (i << 3);
        }

        return t;
    }

    little_endian& operator=(const T other) {

        for (size_t i = 0; i < sizeof(T); ++i) {
            as_bytes[i] = static_cast<u8>(other >> (i << 3));
        }

        return *this;
    }

    little_endian& operator=(const little_endian<T>& other) {
        as_integer = other.as_integer;
        return *this;
    }

    little_endian& operator=(const big_endian<T>& other) {

        for (size_t i = 0; i < sizeof(T); ++i) {
            as_bytes[i] = other.as_bytes[sizeof(T) - 1 - i];
        }

        return *this;
    }

    T operator+=(const T other) {
        return *this = *this + other;
    }

    T operator-=(const T other) {
        return *this = *this - other;
    }

    T operator*=(const T other) {
        return *this = *this * other;
    }

    T operator/=(const T other) {
        return *this = *this / other;
    }

    T operator%=(const T other) {
        return *this = *this % other;
    }

    little_endian<T>& operator++(int) {
        little_endian<T> old(*this);
        ++*this;
        return old;
    }

    little_endian<T>& operator++() {

        for (size_t i = 0; i < sizeof(T); ++i) {
            if (++as_bytes[i] != 0) {
                break;
            }
        }

        return *this;
    }

    little_endian<T>& operator--(int) {
        little_endian<T> old(*this);
        --*this;
        return old;
    }

    little_endian<T>& operator--() {

        for (size_t i = 0; i < sizeof(T); ++i) {
            if (--as_bytes[i] != 0xFF) {
                break;
            }
        }

        return *this;
    }
};
#pragma pack(pop)

#pragma pack(push, 1)
template <typename T>
struct big_endian {

    union {
        u8 as_bytes[sizeof(T)];
        T as_integer;
    };

    big_endian(T t = {}) {
        *this = t;
    }

    big_endian(const big_endian<T>& other) {
        *this = other;
    }

    big_endian(const little_endian<T>& other) {
        *this = other;
    }

    operator T() const {

        T t = {};
        for (size_t i = 0; i < sizeof(T); ++i) {
            t |= static_cast<T>(as_bytes[sizeof(T) - 1 - i]) << (i << 3);
        }

        return t;
    }

    big_endian& operator=(const T other) {

        for (size_t i = 0; i < sizeof(T); ++i) {
            as_bytes[sizeof(T) - 1 - i] = static_cast<u8>(other >> (i << 3));
        }

        return *this;
    }

    big_endian& operator=(const big_endian<T>& other) {
        as_integer = other.as_integer;
        return *this;
    }

    big_endian& operator=(const little_endian<T>& other) {

        for (size_t i = 0; i < sizeof(T); ++i) {
            as_bytes[i] = other.as_bytes[sizeof(T) - 1 - i];
        }

        return *this;
    }

    T operator+=(const T other) {
        return *this = *this + other;
    }

    T operator-=(const T other) {
        return *this = *this - other;
    }

    T operator*=(const T other) {
        return *this = *this * other;
    }

    T operator/=(const T other) {
        return *this = *this / other;
    }

    T operator%=(const T other) {
        return *this = *this % other;
    }

    big_endian<T>& operator++(int) {
        big_endian<T> old(*this);
        ++*this;
        return old;
    }

    big_endian<T>& operator++() {

        for (size_t i = 0; i < sizeof(T); ++i) {
            if (++as_bytes[sizeof(T) - 1 - i] != 0) {
                break;
            }
        }

        return *this;
    }

    big_endian<T>& operator--(int) {
        big_endian<T> old(*this);
        --*this;
        return old;
    }

    big_endian<T>& operator--() {

        for (size_t i = 0; i < sizeof(T); ++i) {
            if (--as_bytes[sizeof(T) - 1 - i] != 0xFF) {
                break;
            }
        }

        return *this;
    }
};
#pragma pack(pop)

using i8le = little_endian<i8>;
using i16le = little_endian<i16>;
using i32le = little_endian<i32>;
using i64le = little_endian<i64>;

using i8be = big_endian<i8>;
using i16be = big_endian<i16>;
using i32be = big_endian<i32>;
using i64be = big_endian<i64>;

using u8le = little_endian<u8>;
using u16le = little_endian<u16>;
using u32le = little_endian<u32>;
using u64le = little_endian<u64>;

using u8be = big_endian<u8>;
using u16be = big_endian<u16>;
using u32be = big_endian<u32>;
using u64be = big_endian<u64>;
