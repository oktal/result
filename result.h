/* 
   Mathieu Stefani, 03 mai 2016
   
   This header provides a Result type that can be used to replace exceptions in code
   that has to handle error.

   Result<T, E> can be used to return and propagate an error to the caller. Result<T, E> is an algebraic
   data type that can either Ok(T) to represent success or Err(E) to represent an error.
*/

#include <iostream>
#include <type_traits>

namespace types {
    template<typename T>
    struct Ok {
        Ok(const T& val) : val(val) { }
        Ok(T&& val) : val(std::move(val)) { }

        T val;
    };

    template<>
    struct Ok<void> { };

    template<typename E>
    struct Err {
        Err(const E& val) : val(val) { }
        Err(E&& val) : val(std::move(val)) { }

        E val;
    };
}

template<typename T, typename CleanT = typename std::decay<T>::type>
types::Ok<CleanT> Ok(T&& val) {
    return types::Ok<CleanT>(std::forward<T>(val));
}

types::Ok<void> Ok() {
    return types::Ok<void>();
}

template<typename E, typename CleanE = typename std::decay<E>::type>
types::Err<CleanE> Err(E&& val) {
    return types::Err<CleanE>(std::forward<E>(val));
}

template<typename T, typename E> struct Result;

namespace details {

template<typename Func>
struct result_of : public result_of<decltype(&Func::operator())> { };

template<typename Ret, typename Cls, typename... Args>
struct result_of<Ret (Cls::*) (Args...) const> {
    typedef Ret type;
};

template<typename R>
struct ResultOkType { typedef typename std::decay<R>::type type; };

template<typename T, typename E>
struct ResultOkType<Result<T, E>> {
    typedef T type;
};

template<typename R>
struct ResultErrType { typedef R type; };

template<typename T, typename E>
struct ResultErrType<Result<T, E>> {
    typedef typename std::remove_reference<E>::type type;
};

namespace ok {

namespace impl {

template<typename T> struct Map;

// General implementation 
template<typename Ret, typename Cls, typename Arg>
struct Map<Ret (Cls::*)(Arg) const> {

    template<typename T, typename E, typename Func>
    static Result<Ret, E> map(const Result<T, E>& result, Func func) {
        if (result.isOk()) {
            auto res = func(result.storage().template get<T>());
            return types::Ok<Ret>(std::move(res));
        }

        return types::Err<E>(result.storage().template get<E>());
    }
};

// Specialization for callback returning void
template<typename Cls, typename Arg>
struct Map<void (Cls::*)(Arg) const> {

    template<typename T, typename E, typename Func>
    static Result<void, E> map(const Result<T, E>& result, Func func) {
        if (result.isOk()) {
            func(result.storage().template get<T>());
            return types::Ok<void>();
        }

        return types::Err<E>(result.storage().template get<E>());
    }
};

// Specialization for a void Result
template<typename Ret, typename Cls>
struct Map<Ret (Cls::*)(void) const> {

    template<typename T, typename E, typename Func>
    static Result<Ret, E> map(const Result<T, E>& result, Func func) {
        if (result.isOk()) {
            auto ret = func();
            return types::Ok<Ret>(std::move(ret));
        }

        return types::Err<E>(result.storage().template get<E>());
    }
};

// Specialization for callback returning void on a void Result
template<typename Cls>
struct Map<void (Cls::*)(void) const> {

    template<typename T, typename E, typename Func>
    static Result<void, E> map(const Result<T, E>& result, Func func) {
        if (result.isOk()) {
            func();
            return types::Ok<void>();
        }

        return types::Err<E>(result.storage().template get<E>());
    }
};

// Specialization for a callback returning a Result
template<typename U, typename E, typename Cls, typename Arg>
struct Map<Result<U, E> (Cls::*)(Arg) const> {

    template<typename T, typename Func>
    static Result<U, E> map(const Result<T, E>& result, Func func) {
        if (result.isOk()) {
            auto res = func(result.storage().template get<T>());
            if (res.isOk()) {
                return types::Ok<U>(res.storage().template get<U>());
            }
        }

        return types::Err<E>(result.storage().template get<E>());
    }
};

// Specialization for a callback returning a void Result
template<typename E, typename Cls, typename Arg>
struct Map<Result<void, E> (Cls::*)(Arg) const> {

    template<typename T, typename Func>
    static Result<void, E> map(const Result<T, E>& result, Func func) {
        if (result.isOk()) {
            auto res = func(result.storage().template get<T>());
            if (res.isOk()) {
                return types::Ok<void>();
            }
        }

        return types::Err<E>(result.storage().template get<E>());
    }
};

// Specialization for a void callback returning a Result
template<typename U, typename E, typename Cls>
struct Map<Result<U, E> (Cls::*)(void) const> {

    template<typename Func>
    static Result<U, E> map(const Result<void, E>& result, Func func) {
        if (result.isOk()) {
            auto res = func();
            if (res.isOk()) {
                return types::Ok<U>(res.storage().template get<U>());
            }
        }

        return types::Err<E>(result.storage().template get<E>());
    }
};

// Specialization for a void callback returning a void Result
template<typename E, typename Cls>
struct Map<Result<void, E> (Cls::*)(void) const> {

    template<typename Func>
    static Result<void, E> map(const Result<void, E>& result, Func func) {
        if (result.isOk()) {
            auto res = func();
            if (res.isOk()) {
                return types::Ok<void>();
            }
        }

        return types::Err<E>(result.storage().template get<E>());
    }
};

} // namespace impl

template<typename Func> struct Map : public impl::Map<decltype(&Func::operator())> { };

} // namespace ok


namespace err {

namespace impl {

template<typename T> struct Map;

template<typename Ret, typename Cls, typename Arg>
struct Map<Ret (Cls::*)(Arg) const> {

    template<typename T, typename E, typename Func>
    static Result<T, Ret> map(const Result<T, E>& result, Func func) {
        if (result.isErr()) {
            auto res = func(result.storage().template get<E>());
            return types::Err<Ret>(res);
        }

        return types::Ok<T>(result.storage().template get<T>());
    }
};

template<typename T, typename F, typename Cls, typename Arg>
struct Map<Result<T, F> (Cls::*)(Arg) const> {

    template<typename E, typename Func>
    static Result<T, F> map(const Result<T, E>& result, Func func) {
        if (result.isErr()) {
            auto res = func(result.storage().template get<E>());
            if (res.isErr()) {
                return types::Err<F>(res.storage().template get<F>());
            }
        }

        return types::Ok<T>(result.storage().template get<T>());
    }
};

} // namespace impl

template<typename Func> struct Map : public impl::Map<decltype(&Func::operator())> { };

} // namespace err;


template<typename T, typename E, typename Func,
         typename Ret =
            Result<
                typename details::ResultOkType<
                    typename details::result_of<Func>::type
                >::type,
            E>
        >
Ret map(const Result<T, E>& result, Func func) {
    return ok::Map<Func>::map(result, func);
}

template<typename T, typename E, typename Func,
         typename Ret =
            Result<T,
                typename details::ResultErrType<
                    typename details::result_of<Func>::type
                >::type
            >
        >
Ret mapErr(const Result<T, E>& result, Func func) {
    return err::Map<Func>::map(result, func);
}

struct ok_tag { };
struct err_tag { };

template<typename T, typename E>
struct Storage {
    static constexpr size_t Size = sizeof(T) > sizeof(E) ? sizeof(T) : sizeof(E);
    static constexpr size_t Align = sizeof(T) > sizeof(E) ? alignof(T) : alignof(E);

    typedef typename std::aligned_storage<Size, Align>::type type;

    void construct(types::Ok<T> ok) {
        new (&storage_) T(ok.val);
    }
    void construct(types::Err<E> err) {
        new (&storage_) E(err.val);
    }

    template<typename U>
    const U& get() const {
        return *reinterpret_cast<const U *>(&storage_);
    }

    template<typename U>
    U& get() {
        return *reinterpret_cast<U *>(&storage_);
    }

    void destroy(ok_tag) {
        get<T>().~T();
    }

    void destroy(err_tag) {
        get<E>().~E();
    }

    type storage_;
};

template<typename E>
struct Storage<void, E> {
    typedef typename std::aligned_storage<sizeof(E), alignof(E)>::type type;

    void construct(types::Ok<void>) { }

    void construct(types::Err<E> err) {
        new (&storage_) E(err.val);
    }

    void destroy(ok_tag) { }
    void destroy(err_tag) { get<E>().~E(); }

    template<typename U>
    const U& get() const {
        return *reinterpret_cast<const U *>(&storage_);
    }

    template<typename U>
    U& get() {
        return *reinterpret_cast<U *>(&storage_);
    }

    type storage_;
};

} // namespace details

namespace concept {

#pragma GCC diagnostic push
// We need to disable the -Wunused-value diagnostic for the underneath SFINAE-expression
// using decltype, otherwise, gcc will warn us that one side of the expression (left-hand
// side of the comma operator , is not used. Guess what gcc, that's exactly what I intend
// to do
#pragma GCC diagnostic ignored "-Wunused-value"

    template<typename T, typename = void> struct EqualityComparable : std::false_type { };

    template<typename T>
    struct EqualityComparable<T,
    typename std::enable_if<
        true, 
        decltype(std::declval<T>() == std::declval<T>(), void())
        >::type
    > : std::true_type
{
};

#pragma GCC diagnostic pop

} // namespace concept

template<typename T, typename E>
struct Result {

    static_assert(!std::is_same<E, void>::value, "void error type is not allowed");

    typedef details::Storage<T, E> storage_type;

    Result(types::Ok<T> ok)
        : ok_(true)
    {
        storage_.construct(std::move(ok));
    }

    Result(types::Err<E> err)
        : ok_(false)
    {
        storage_.construct(std::move(err));
    }

    ~Result() {
        if (ok_)
            storage_.destroy(details::ok_tag());
        else
            storage_.destroy(details::err_tag());
    }

    bool isOk() const {
        return ok_;
    }

    bool isErr() const {
        return !ok_;
    }

    T expect(const char* str) {
        if (!isOk()) {
            std::fprintf(stderr, "%s\n", str);
            std::terminate(); 
        }
        return expect_impl(std::is_same<T, void>());
    }

    template<typename Func,
             typename Ret =
                Result<
                    typename details::ResultOkType<
                        typename details::result_of<Func>::type
                    >::type,
                E>
            >
    Ret map(Func func) {
        return details::map(*this, func);
    }

    template<typename Func,
         typename Ret =
             Result<T,
                typename details::ResultErrType<
                    typename details::result_of<Func>::type
                >::type
            >
    >
    Ret mapErr(Func func) {
        return details::mapErr(*this, func);
    }

    storage_type& storage() {
        return storage_;
    }

    const storage_type& storage() const {
        return storage_;
    }

private:
    T expect_impl(std::true_type) { }
    T expect_impl(std::false_type) { return storage_.template get<T>(); }

    bool ok_;
    storage_type storage_;
};

template<typename T, typename E>
bool operator==(const Result<T, E>& lhs, const Result<T, E>& rhs) {
    static_assert(concept::EqualityComparable<T>::value, "T must be EqualityComparable for Result to be comparable");
    static_assert(concept::EqualityComparable<E>::value, "E must be EqualityComparable for Result to be comparable");

    if (lhs.isOk() && rhs.isOk()) {
        return lhs.storage().template get<T>() == rhs.storage().template get<T>();
    }
    if (lhs.isErr() && rhs.isErr()) {
        return lhs.storage().template get<E>() == rhs.storage().template get<E>();
    }
}

template<typename T, typename E>
bool operator==(const Result<T, E>& lhs, types::Ok<T> ok) {
    static_assert(concept::EqualityComparable<T>::value, "T must be EqualityComparable for Result to be comparable");

    if (!lhs.isOk()) return false;

    return lhs.storage().template get<T>() == ok.val;
}

template<typename E>
bool operator==(const Result<void, E>& lhs, types::Ok<void>) {
    return lhs.isOk();
}

template<typename T, typename E>
bool operator==(const Result<T, E>& lhs, types::Err<E> err) {
    static_assert(concept::EqualityComparable<E>::value, "E must be EqualityComparable for Result to be comparable");
    if (!lhs.isErr()) return false;

    return lhs.storage().template get<E>() == err.val;
}

#define TRY(...)                                                   \
    ({                                                             \
        auto res = __VA_ARGS__;                                    \
        if (!res.isOk()) {                                         \
            typedef details::ResultErrType<decltype(res)>::type E; \
            return types::Err<E>(res.storage().get<E>());          \
        }                                                          \
        typedef details::ResultOkType<decltype(res)>::type T;      \
        res.storage().get<T>();                                    \
    })
