#pragma once

#if defined(DEFAULT_MOVE_AND_ASSIGN)
#undef DEFAULT_MOVE_AND_ASSIGN
#endif

#define DEFAULT_MOVE_AND_ASSIGN(_class_name) \
    _class_name(_class_name&&) = default;    \
    _class_name& operator=(_class_name&&) = default

//

#if defined(DEFAULT_COPY_AND_ASSIGN)
#undef DEFAULT_COPY_AND_ASSIGN
#endif

#define DEFAULT_COPY_AND_ASSIGN(_class_name)   \
    _class_name(const _class_name&) = default; \
    _class_name& operator=(const _class_name&) = default

//

#if defined(DEFAULT_COPY_ASSIGN_AND_MOVE)
#undef DEFAULT_COPY_ASSIGN_AND_MOVE
#endif

#define DEFAULT_COPY_ASSIGN_AND_MOVE(_class_name) \
    DEFAULT_COPY_AND_ASSIGN(_class_name);         \
    DEFAULT_MOVE_AND_ASSIGN(_class_name)

//

#if defined(DISALLOW_MOVE_AND_ASSIGN)
#undef DISALLOW_MOVE_AND_ASSIGN
#endif

#define DISALLOW_MOVE_AND_ASSIGN(_class_name) \
    _class_name(_class_name&&) = delete;      \
    _class_name& operator=(_class_name&&) = delete

//

#if defined(DISALLOW_COPY_AND_ASSIGN)
#undef DISALLOW_COPY_AND_ASSIGN
#endif

#define DISALLOW_COPY_AND_ASSIGN(_class_name) \
    _class_name(const _class_name&) = delete; \
    _class_name& operator=(const _class_name&) = delete

//

#if defined(DISALLOW_COPY_ASSIGN_AND_MOVE)
#undef DISALLOW_COPY_ASSIGN_AND_MOVE
#endif

#define DISALLOW_COPY_ASSIGN_AND_MOVE(_class_name) \
    DISALLOW_COPY_AND_ASSIGN(_class_name);         \
    DISALLOW_MOVE_AND_ASSIGN(_class_name)

//

#if defined(INHERIT_CONSTRUCTORS_AND_ASSIGN)
#undef INHERIT_CONSTRUCTORS_AND_ASSIGN
#endif

#define INHERIT_CONSTRUCTORS_AND_ASSIGN(_base_class) \
    using _base_class::_base_class;                  \
    using _base_class::operator=
