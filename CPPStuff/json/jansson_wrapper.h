#pragma once
// ReSharper disable CppConstValueFunctionReturnType
// ReSharper disable CppClangTidyModernizeUseNodiscard
// ReSharper disable CppNonExplicitConvertingConstructor
#include "jansson/jansson.h"

namespace json {

    struct Value;
    class Iterator;

    namespace _private {

        class ArrayElementProxy;
        class ObjectPropertyProxy;

        template <typename Base>
        struct ValueBase : Base {

            ValueBase() : Base() {}
            ValueBase(const Base& base) : Base(base) {}
            ValueBase(json_t* json) : Base(json) {}

            ValueBase& operator=(const Value& value);

            bool is_undefined() const;
            bool is_object() const;
            bool is_array() const;
            bool is_string() const;
            bool is_integer() const;
            bool is_real() const;
            bool is_number() const;
            bool is_true() const;
            bool is_false() const;
            bool is_boolean() const;
            bool is_null() const;

            const Value at(size_t index) const;
            const Value operator[](size_t index) const;

            ValueBase<ArrayElementProxy> at(size_t index);
            ValueBase<ArrayElementProxy> operator[](size_t index);

            const Value get(const char* key) const;
            const Value get(const std::string& key) const;
            const Value operator[](const char* key) const;
            const Value operator[](const std::string& key) const;

            ValueBase<ObjectPropertyProxy> get(const char* key);
            ValueBase<ObjectPropertyProxy> get(const std::string& key);
            ValueBase<ObjectPropertyProxy> operator[](const char* key);
            ValueBase<ObjectPropertyProxy> operator[](const std::string& key);

            size_t size() const;
            void clear();

            json_int_t as_integer() const;
            double as_real() const;
            double as_number() const;
            bool as_boolean() const;
            const char* as_cstring() const;
            std::string as_string() const;

            ValueBase<Base>& set_key(const char* key, const Value& value);
            ValueBase<Base>& set_key(const std::string& key, const Value& value);

            ValueBase<Base>& del_key(const char* key);
            ValueBase<Base>& del_key(const std::string& key);

            ValueBase<Base>& set_at(size_t index, const Value& value);
            ValueBase<Base>& del_at(size_t index);
            ValueBase<Base>& insert_at(size_t index, const Value& value);

            std::unique_ptr<char> save_string(size_t flags = 0) const;
            int save_file(const char* path, size_t flags = 0) const;
        };

        struct GenericValue {

            GenericValue() = default;
            GenericValue(const GenericValue& value) : m_value(json_incref(value.m_value)) {}
            explicit GenericValue(json_t* value) : m_value(json_incref(value)) {}

            virtual ~GenericValue();

            GenericValue& operator=(const GenericValue& other);

            json_t* as_json() const;
            operator bool() const;

            static GenericValue take_ownership(json_t* json);

        protected:
            json_t* m_value = nullptr;
        };

        class ArrayElementProxy {

            json_t* m_array;
            size_t m_index;

        public:
            ArrayElementProxy(json_t* arr, size_t idx) : m_array(arr), m_index(idx) {}

            ArrayElementProxy& operator=(const Value& value);

            json_t* as_json() const;
            operator bool() const;
        };

        class ObjectPropertyProxy {

            json_t* m_object;
            const char* m_key;

        public:
            ObjectPropertyProxy(json_t* obj, const char* key) : m_object(obj), m_key(key) {}

            ObjectPropertyProxy& operator=(const Value& value);

            json_t* as_json() const;
            operator bool() const;
        };
    }

    struct Value : _private::ValueBase<_private::GenericValue> {

        explicit Value(const char* value);
        explicit Value(const std::string& value);
        explicit Value(bool value);
        explicit Value(int value);
        explicit Value(unsigned int value);
        explicit Value(short value);
        explicit Value(unsigned short value);
        explicit Value(long value);
        explicit Value(unsigned long value);
        explicit Value(float value);
        explicit Value(double value);

        Value() : _private::ValueBase<_private::GenericValue>() {}
        Value(const Value& value) : _private::ValueBase<_private::GenericValue>(value) {}

        Value(const _private::GenericValue& value) : _private::ValueBase<_private::GenericValue>(value) {}
        Value(const _private::ValueBase<_private::GenericValue>& value) : _private::ValueBase<_private::GenericValue>(value) {}

        explicit Value(json_t* json) : _private::ValueBase<_private::GenericValue>(json) {}
    };

    class Iterator {

        Value m_object;
        void* m_iter;

    public:
        Iterator(const Value& value);
        Iterator(const _private::ValueBase<_private::ObjectPropertyProxy>& value);

        Iterator(const Iterator&) = delete;
        Iterator& operator=(const Iterator&) = delete;

        void next();
        Iterator& operator++();

        operator bool() const;

        const char* ckey() const;
        std::string key() const;

        const Value value() const;
        const Value operator*() const;
    };

    Value object();
    Value array();
    Value null();
    Value load_file(const char* path, size_t flags = 0, json_error_t* error = nullptr);
    Value load_string(const char* string, size_t flags = 0, json_error_t* error = nullptr);
    Value load_buffer(const char* buffer, size_t buflen, size_t flags = 0, json_error_t* error = nullptr);
}

namespace json {

    namespace _private {

        template <typename Base>
        ValueBase<Base>& ValueBase<Base>::operator=(const Value& value) {
            Base::operator=(value);
            return *this;
        }

        template <typename Base>
        bool ValueBase<Base>::is_undefined() const {
            return Base::as_json() == nullptr;
        }

        template <typename Base>
        bool ValueBase<Base>::is_object() const {
            return json_is_object(Base::as_json());
        }

        template <typename Base>
        bool ValueBase<Base>::is_array() const {
            return json_is_array(Base::as_json());
        }

        template <typename Base>
        bool ValueBase<Base>::is_string() const {
            return json_is_string(Base::as_json());
        }

        template <typename Base>
        bool ValueBase<Base>::is_integer() const {
            return json_is_integer(Base::as_json());
        }

        template <typename Base>
        bool ValueBase<Base>::is_real() const {
            return json_is_real(Base::as_json());
        }

        template <typename Base>
        bool ValueBase<Base>::is_number() const {
            return json_is_number(Base::as_json());
        }

        template <typename Base>
        bool ValueBase<Base>::is_true() const {
            return json_is_true(Base::as_json());
        }

        template <typename Base>
        bool ValueBase<Base>::is_false() const {
            return json_is_false(Base::as_json());
        }

        template <typename Base>
        bool ValueBase<Base>::is_boolean() const {
            return json_is_boolean(Base::as_json());
        }

        template <typename Base>
        bool ValueBase<Base>::is_null() const {
            return json_is_null(Base::as_json());
        }

        template <typename Base>
        const Value ValueBase<Base>::at(size_t index) const {
            return Value(json_array_get(Base::as_json(), index));
        }

        template <typename Base>
        const Value ValueBase<Base>::operator[](size_t index) const {
            return at(index);
        }

        template <typename Base>
        ValueBase<ArrayElementProxy> ValueBase<Base>::at(size_t index) {
            return ArrayElementProxy(Base::as_json(), index);
        }

        template <typename Base>
        ValueBase<ArrayElementProxy> ValueBase<Base>::operator[](size_t index) {
            return at(index);
        }

        template <typename Base>
        const Value ValueBase<Base>::get(const char* key) const {
            return Value(json_object_get(Base::as_json(), key));
        }

        template <typename Base>
        const Value ValueBase<Base>::get(const std::string& key) const {
            return get(key.c_str());
        }

        template <typename Base>
        const Value ValueBase<Base>::operator[](const char* key) const {
            return get(key);
        }

        template <typename Base>
        const Value ValueBase<Base>::operator[](const std::string& key) const {
            return get(key.c_str());
        }

        template <typename Base>
        ValueBase<ObjectPropertyProxy> ValueBase<Base>::get(const char* key) {
            return ObjectPropertyProxy(Base::as_json(), key);
        }

        template <typename Base>
        ValueBase<ObjectPropertyProxy> ValueBase<Base>::get(const std::string& key) {
            return get(key.c_str());
        }

        template <typename Base>
        ValueBase<ObjectPropertyProxy> ValueBase<Base>::operator[](const char* key) {
            return get(key);
        }

        template <typename Base>
        ValueBase<ObjectPropertyProxy> ValueBase<Base>::operator[](const std::string& key) {
            return get(key.c_str());
        }

        template <typename Base>
        size_t ValueBase<Base>::size() const {

            if (is_object()) {
                return json_object_size(Base::as_json());
            }

            return json_array_size(Base::as_json());
        }

        template <typename Base>
        void ValueBase<Base>::clear() {
            if (is_object()) {
                json_object_clear(Base::as_json());
            }
            else {
                json_array_clear(Base::as_json());
            }
        }

        template <typename Base>
        json_int_t ValueBase<Base>::as_integer() const {
            return json_integer_value(Base::as_json());
        }

        template <typename Base>
        double ValueBase<Base>::as_real() const {
            return json_real_value(Base::as_json());
        }

        template <typename Base>
        double ValueBase<Base>::as_number() const {
            return json_number_value(Base::as_json());
        }

        template <typename Base>
        bool ValueBase<Base>::as_boolean() const {
            return is_true();
        }

        template <typename Base>
        const char* ValueBase<Base>::as_cstring() const {
            return json_string_value(Base::as_json());
        }

        template <typename Base>
        std::string ValueBase<Base>::as_string() const {
            const char* tmp = as_cstring();
            return tmp == nullptr ? "" : tmp;
        }

        template <typename Base>
        ValueBase<Base>& ValueBase<Base>::set_key(const char* key, const Value& value) {
            json_object_set(Base::as_json(), key, value.Base::as_json());
            return *this;
        }

        template <typename Base>
        ValueBase<Base>& ValueBase<Base>::set_key(const std::string& key, const Value& value) {
            return set_key(key.c_str(), value);
        }

        template <typename Base>
        ValueBase<Base>& ValueBase<Base>::del_key(const char* key) {
            json_object_del(Base::as_json(), key);
            return *this;
        }

        template <typename Base>
        ValueBase<Base>& ValueBase<Base>::del_key(const std::string& key) {
            return del_key(key.c_str());
        }

        template <typename Base>
        ValueBase<Base>& ValueBase<Base>::set_at(size_t index, const Value& value) {
            if (index == size()) {
                json_array_append(Base::as_json(), value.Base::as_json());
            }
            else {
                json_array_set(Base::as_json(), index, value.Base::as_json());
            }

            return *this;
        }

        template <typename Base>
        ValueBase<Base>& ValueBase<Base>::del_at(size_t index) {
            json_array_remove(Base::as_json(), index);
            return *this;
        }

        template <typename Base>
        ValueBase<Base>& ValueBase<Base>::insert_at(size_t index, const Value& value) {
            json_array_insert(Base::as_json(), index, value.Base::as_json());
            return *this;
        }

        template <typename Base>
        std::unique_ptr<char> ValueBase<Base>::save_string(size_t flags) const {
            return std::unique_ptr<char>(json_dumps(Base::as_json(), flags));
        }

        template <typename Base>
        int ValueBase<Base>::save_file(const char* path, size_t flags) const {
            return json_dump_file(Base::as_json(), path, flags);
        }

        inline GenericValue::~GenericValue() {
            json_decref(m_value);
        }

        inline GenericValue& GenericValue::operator=(const GenericValue& other) {

            if (&other != this) {
                json_decref(m_value);
                m_value = json_incref(other.m_value);
            }

            return *this;
        }

        inline json_t* GenericValue::as_json() const {
            return m_value;
        }

        inline GenericValue::operator bool() const {
            return m_value != nullptr;
        }

        inline GenericValue GenericValue::take_ownership(json_t* json) {
            GenericValue v;
            v.m_value = json;
            return v;
        }

        inline ArrayElementProxy& ArrayElementProxy::operator=(const Value& value) {
            json_array_set(m_array, m_index, value.as_json());
            return *this;
        }

        inline json_t* ArrayElementProxy::as_json() const {
            return json_array_get(m_array, m_index);
        }

        inline ArrayElementProxy::operator bool() const {
            return m_array != nullptr;
        }

        inline ObjectPropertyProxy& ObjectPropertyProxy::operator=(const Value& value) {
            json_object_set(m_object, m_key, value.as_json());
            return *this;
        }

        inline json_t* ObjectPropertyProxy::as_json() const {
            return json_object_get(m_object, m_key);
        }

        inline ObjectPropertyProxy::operator bool() const {
            return m_object != nullptr;
        }
    }

    inline Value::Value(const char* value) {
        m_value = json_string(value);
    }

    inline Value::Value(const std::string& value) {
        m_value = json_string(value.c_str());
    }

    inline Value::Value(bool value) {
        m_value = value ? json_true() : json_false();
    }

    inline Value::Value(int value) {
        m_value = json_integer(value);
    }

    inline Value::Value(unsigned int value) {
        m_value = json_integer(value);
    }

    inline Value::Value(short value) {
        m_value = json_integer(value);
    }

    inline Value::Value(unsigned short value) {
        m_value = json_integer(value);
    }

    inline Value::Value(long value) {
        m_value = json_integer(value);
    }

    inline Value::Value(unsigned long value) {
        m_value = json_integer(value);
    }

    inline Value::Value(float value) {
        m_value = json_real(value);
    }

    inline Value::Value(double value) {
        m_value = json_real(value);
    }

    inline Iterator::Iterator(const Value& value) : m_object(value) {
        m_iter = json_object_iter(m_object.as_json());
    }

    inline Iterator::Iterator(const _private::ValueBase<_private::ObjectPropertyProxy>& value) : m_object(value.as_json()) {
        m_iter = json_object_iter(m_object.as_json());
    }

    inline void Iterator::next() {
        m_iter = json_object_iter_next(m_object.as_json(), m_iter);
    }

    inline Iterator& Iterator::operator++() {
        next();
        return *this;
    }

    inline Iterator::operator bool() const {
        return m_iter != nullptr;
    }

    inline const char* Iterator::ckey() const {
        return json_object_iter_key(m_iter);
    }

    inline std::string Iterator::key() const {
        return ckey();
    }

    inline const Value Iterator::value() const {
        return Value(json_object_iter_value(m_iter));
    }

    inline const Value Iterator::operator*() const {
        return value();
    }

    inline Value object() {
        return Value::take_ownership(json_object());
    }

    inline Value array() {
        return Value::take_ownership(json_array());
    }

    inline Value null() {
        return Value::take_ownership(json_null());
    }

    inline Value load_file(const char* path, size_t flags, json_error_t* error) {
        return Value::take_ownership(json_load_file(path, flags, error));
    }

    inline Value load_string(const char* string, size_t flags, json_error_t* error) {
        return Value::take_ownership(json_loads(string, flags, error));
    }

    inline Value load_buffer(const char *buffer, size_t buflen, size_t flags, json_error_t *error) {
        return Value::take_ownership(json_loadb(buffer, buflen, flags, error));
    }
}
