#include "mesosched/TupleParser.hpp"
#include <sstream>
#include <string>

namespace mesosched {
    PairVector TupleParser::parse(const std::string&& aSpec) {
        PairVector mapping;
        auto parseContext = ParseContext::None;
        size_t currentNumber = 0, currentFirst = 0;
        for (auto tok : aSpec) {
            switch (tok) {
                case '(':
                    if (parseContext != ParseContext::None) { throw BadParse{}; }
                    parseContext = ParseContext::First;
                    break;
                case ')':
                    if (parseContext != ParseContext::Second) { throw BadParse{}; }
                    mapping.emplace_back(currentFirst, currentNumber);
                    currentNumber = 0;
                    parseContext = ParseContext::None;
                    break;
                case ' ':
                    if (parseContext == ParseContext::First) {
                        currentFirst = currentNumber;
                        currentNumber = 0;
                        parseContext = ParseContext::Second;
                    } else if (parseContext == ParseContext::Second) {
                        throw BadParse{};
                    }
                    break;
                default: {
                    auto digit = tok - '0';
                    if (digit < 0 || digit > 9) {
                        throw BadParse{};
                    }
                    currentNumber *= 10;
                    currentNumber += digit;
                }
            }
        }
        return mapping;
    }

    std::string TupleParser::debugParse(const std::string&& aSpec) {
        auto parsed = parse(std::move(aSpec));
        std::ostringstream os;
        for (auto pair : parsed) {
            os << pair.first << ":" << pair.second << "|";
        }
        return os.str();
    }
}
