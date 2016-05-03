# Result

`Result<T, E>` is a template type that can be used to return and propage errors. It can be used to replace
exceptions in context where they are not allowed or too slow to be used. `Result<T, E>` is an algebraic data
type of `Ok(T)` that represents success and `Err(E)` representing an error.

```

struct Request {
};

struct Error {

    enum class Kind {
        Timeout,
        Invalid,
        TooLong
    }

    Error(Kind kind, std::string text);

    Kind kind;
    std::string text;
};

Result<Request, Error> parseRequest(const std::string& payload) {
    if (payload.size() > 512) return Err(Error(Kind::TooLong, "Request exceeded maximum allowed size (513 bytes)"));

    Request request;
    return Ok(request);
}

std::string payload = receivePayload();
auto request = parseRequest(payload).expect("Failed to parse request");
```

To extract the value from a `Result<T, E>` type, you can use the `expect()` function that will yield the value
of an `Ok(T)` or terminate the program with an error message passed as a parameter.

To apply a function to a `Result<T, E>` value, use the `map` function:

```
auto request = parseRequest(payload);
request.map([](const Request& req) { ... });
```

`map` will apply the given callback if the Result contains an `Ok` value, leaving it untouched otherwise.
Note that multiple calls of `map` can be chained together to compose multiple operations:

```
request
  .map([](const Request& req) { return handleRequest(request); })
  .map([]() { std::cout << "Request finished" << std::endl; });
```
