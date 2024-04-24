#include "mesosched/TupleParser.hpp"
#include "mesosched/Sched.hpp"
#include "gtest/gtest.h"

using namespace mesosched;

typedef ::testing::AssertionResult (*AssertionT)(const char *, const char *,
                                                 const char *, const char *);

::testing::AssertionResult
CmpHelperEQ(bool InvertMatch, const char *NeedleExpr, const char *HaystackExpr,
            std::string Needle, std::string Haystack) {
  using namespace ::testing::internal;

  auto OriginalNeedle = Needle;
  auto OriginalHaystack = Haystack;
  auto isWS = [](char Candidate) {
    return Candidate == '\n' || Candidate == ' ' || Candidate == '\t';
  };
  Needle.erase(std::remove_if(Needle.begin(), Needle.end(), isWS),
               Needle.end());
  Haystack.erase(std::remove_if(Haystack.begin(), Haystack.end(), isWS),
                 Haystack.end());

  if ((Haystack.find(Needle) == std::string::npos) == InvertMatch) {
    return ::testing::AssertionSuccess();
  }
  return ::testing::AssertionFailure()
         << "Expected (" << NeedleExpr << ") is:" << std::endl
         << "\"" << OriginalNeedle << "\"" << std::endl
         << "Actual (" << HaystackExpr << ") is:" << std::endl
         << "\"" << OriginalHaystack << "\"";
}

/// Strip whitespace and tell if expected is a substring of actual.
template <bool InvertMatch = false>
::testing::AssertionResult
StripWSisSubstring(const char *expected_expression,
                   const char *actual_expression, std::string expected,
                   std::string actual) {
  return CmpHelperEQ(InvertMatch, expected_expression, actual_expression,
                     expected, actual);
}

void ASSERT_EQ_NOWS(std::string aSideA, std::string aSideB) {
    EXPECT_PRED_FORMAT2(StripWSisSubstring<>, aSideA, aSideB);
}

TEST(Sched, SuccessfulConstruction)
{
  auto nodeToResources = "(2 3) (7 1) (1 23) (8 5) (2 8)";
  auto resourceStepRequirements = "(3 4) (1 4) (4 7) (1 3) (5 4) (9 3)";

  // Can't use braced initialization inside macro.
  ASSERT_NO_THROW(Sched(std::move(nodeToResources), std::move(resourceStepRequirements)));
}

TEST(Sched, ExampleResult)
{
  auto nodeToResources = "(2 3) (7 1) (1 23) (8 5) (2 8)";
  auto resourceStepRequirements = "(3 4) (1 4) (4 7) (1 3) (5 4) (9 3)";
  auto result =
    R"DBG(
        1 >> r#1 -> j#0 || r#2 -> j#1 || r#7 -> j#4 || r#8 -> j#2 ||
        5 >> r#1 -> j#5 || r#2 -> j#3 ||
    )DBG";

  std::ostringstream os;
  Sched sched{std::move(nodeToResources), std::move(resourceStepRequirements)};
  os << sched;
  ASSERT_EQ_NOWS(os.str(), result);
}
