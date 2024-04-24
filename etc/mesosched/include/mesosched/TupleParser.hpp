#pragma once

#include <exception>
#include <utility>
#include <vector>

namespace mesosched {
    enum class ParseContext {
        None,
        First,
        Second
    };

    class BadParse : public std::runtime_error {
    public:
        BadParse() : std::runtime_error("Bad parse") {};
    };

    using PairVector = std::vector<std::pair<size_t, size_t>>;

    class TupleParser {
    public:
        static PairVector parse(const std::string&& aSpec);
        static std::string debugParse(const std::string&& aSpec);
    };
}
