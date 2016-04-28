#include <iostream>
#include <type_traits>
#include <algorithm>

namespace types {
    template<typename T>
    struct Ok {
        template<typename U>
        Ok(U&& val)
            : val(std::forward<U>(val))
        { }

        T val;
    };

    template<>
    struct Ok<void> { };

    template<typename E>
    struct Err {
        template<typename U>
        Err(U&& err)
            : val(std::forward<U>(val))
        { }

        E val;
    };
}

template<typename T>
types::Ok<T> Ok(T&& val) {
    return types::Ok<T>(std::forward<T>(val));
}

types::Ok<void> Ok() {
    return  types::Ok<void>();
}

template<typename E>
types::Err<E> Err(E&& val) {
    return types::Err<E>(std::forward<E>(val));
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
struct ResultOkType { typedef R type; };

template<typename T, typename E>
struct ResultOkType<Result<T, E>> {
    typedef T type;
};

template<typename Func> struct Map : public Map<decltype(&Func::operator())> { };

template<typename Ret, typename Cls, typename... Args>
struct Map<Ret (Cls::*)(Args...) const> {

    template<typename T, typename E, typename Func>
    static Result<Ret, E> map(const Result<T, E>& result, Func func) {
        if (result.isOk()) {
            auto res = func(result.template get<T>());
            return types::Ok<Ret>(std::move(res));
        }

        return types::Err<E>(result.template get<E>());
    }
};

template<typename Cls, typename... Args>
struct Map<void (Cls::*)(Args...) const> {

    template<typename T, typename E, typename Func>
    static Result<void, E> map(const Result<T, E>& result, Func func) {
        if (result.isOk()) {
            func(result.template get<T>());
            return types::Ok<void>();
        }

        return types::Err<E>(result.template get<E>());
    }
};

template<typename U, typename E, typename Cls, typename... Args>
struct Map<Result<U, E> (Cls::*)(Args...) const> {

    template<typename T, typename Func>
    static Result<U, E> map(const Result<T, E>& result, Func func) {
        if (result.isOk()) {
            auto res = func(result.template get<T>());
            if (res.isOk()) {
                return types::Ok<U>(res.template get<U>());
            }
        }

        return types::Err<E>(result.template get<E>());
    }
};

template<typename T, typename E, typename Func,
         typename Ret =
            Result<
                typename details::ResultOkType<
                    typename details::result_of<Func>::type
                >::type,
            E>
        >
Ret map(const Result<T, E>& result, Func func) {
    return Map<Func>::map(result, func);
}

}

template<typename T, typename E>
struct Result {

    static constexpr size_t Size = sizeof(T) > sizeof(E) ? sizeof(T) : sizeof(E);
    static constexpr size_t Align = sizeof(T) > sizeof(E) ? alignof(T) : alignof(E);

    typedef typename std::aligned_storage<Size, Align>::type storage;

    Result(types::Ok<T> ok)
        : ok_(true)
    {
        new (&storage_) T(ok.val);
    }

    Result(types::Err<E> err)
        : ok_(false)
    {
        new (&storage_) E(err.val);
    }

    ~Result() {
        if (ok_)
            get<T>().~T();
        else
            get<E>().~E();
    }

    bool isOk() const {
        return ok_;
    }

    bool isErr() const {
        return !ok_;
    }

    void expect(const char* str) {
        if (!isOk()) {
            std::fprintf(stderr, "%s\n", str);
            std::terminate(); 
        }
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
        return details::Map<Func>::map(*this, func);
    }

    template<typename U>
    const U& get() const {
        return *reinterpret_cast<const U *>(&storage_);
    }

    template<typename U>
    U& get() {
        return *reinterpret_cast<U *>(&storage_);
    }
private:

    bool ok_;
    storage storage_;
};

template<typename E>
struct Result<void, E> {
    typedef typename std::aligned_storage<sizeof(E), alignof(E)>::type storage;

    Result(types::Ok<void>)
        : ok_(true)
    { }

    Result(types::Err<E> err)
        : ok_(false)
    {
        new (&storage_) E(err.val);
    }

    ~Result() {
        if (!ok_)
            get<E>().~E();
    }

    bool isOk() const {
        return ok_;
    }

    bool isErr() const {
        return !ok_;
    }

    void expect(const char* str) {
        if (!isOk()) {
            std::fprintf(stderr, "%s\n", str);
            std::terminate(); 
        }
    }

    template<typename U>
    const U& get() const {
        return *reinterpret_cast<const U *>(&storage_);
    }

    template<typename U>
    U& get() {
        return *reinterpret_cast<U *>(&storage_);
    }

private:
    bool ok_;
    storage storage_;
};

template<typename T, typename E, typename Func,
         typename Ret =
            Result<
                typename details::ResultOkType<
                    typename details::result_of<Func>::type
                >::type,
            E>
        >
Ret result_map(const Result<T, E>& result, Func func) {
    return details::map(result, func);
}

enum class ErrorKind {
    NotFound,
    Invalid
};

template<typename T> using OpResult = Result<T, ErrorKind>;

struct Data {
    Data(std::string value)
        : value(std::move(value))
    { }

    std::string value;
};

int main() {
    OpResult<Data> res1 = Err(ErrorKind::Invalid);   

    Result<uint32_t, ErrorKind> r1 = Ok(8u);
    auto r2 = r1.map(
        [](uint32_t val) -> Result<uint32_t, ErrorKind> {
            if (val % 2 == 0) return Ok(val / 2);
            else return Err(ErrorKind::Invalid);
        });

    auto r3 = r2.map([](uint32_t val) { return val * 2; }).map([](uint32_t val) { std::cout << val << std::endl;});

    r3.expect("Fail");

}
