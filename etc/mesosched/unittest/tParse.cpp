#include "mesosched/TupleParser.hpp"
#include "mesosched/Sched.hpp"
#include "gtest/gtest.h"

using namespace mesosched;

TEST(Parse, SuccessfulParse)
{
  auto nodeToResources = "(2 3) (7 1) (1 23) (8 5) (2 8)";
  auto resourceStepRequirements = "(3 4) (1 4) (4 7) (1 3) (5 4) (9 3)";
  ASSERT_NO_THROW(TupleParser::parse(std::move(nodeToResources)));
  ASSERT_NO_THROW(TupleParser::parse(std::move(resourceStepRequirements)));
}

TEST(Parse, DebugParse)
{
  auto nodeToResources = "(2 3) (7 1) (1 23) (8 5) (2 8)";
  auto resourceStepRequirements = "(3 4) (1 4) (4 7) (1 3) (5 4) (9 3)";
  auto expectedA = "2:3|7:1|1:23|8:5|2:8|";
  auto expectedB = "3:4|1:4|4:7|1:3|5:4|9:3|";
  ASSERT_EQ(TupleParser::debugParse(std::move(nodeToResources)), expectedA);
  ASSERT_EQ(TupleParser::debugParse(std::move(resourceStepRequirements)), expectedB);
}
