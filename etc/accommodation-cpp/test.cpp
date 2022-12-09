#include "student-accommodation.hpp"
#include "ut.hpp"

using namespace boost::ut;

int main() {
  std::map<std::pair<int, int>, double> distances{
      {std::make_pair(1, 1), 0},    {std::make_pair(1, 2), 3.2},
      {std::make_pair(1, 3), 2.4},  {std::make_pair(1, 4), 7.2},
      {std::make_pair(2, 1), 3.2},  {std::make_pair(2, 2), 0},
      {std::make_pair(2, 3), 11.2}, {std::make_pair(2, 4), 22.3},
      {std::make_pair(3, 1), 2.4},  {std::make_pair(3, 2), 11.2},
      {std::make_pair(3, 3), 0},    {std::make_pair(3, 4), 1.1},
      {std::make_pair(4, 1), 7.2},  {std::make_pair(4, 2), 22.3},
      {std::make_pair(4, 3), 1.1},  {std::make_pair(4, 4), 0}};

  "FewBackedges"_test = [&] {
    std::vector<std::pair<int, int>> friends{
        std::make_pair(1, 2), std::make_pair(1, 3), std::make_pair(1, 4),
        std::make_pair(2, 3), std::make_pair(2, 4), std::make_pair(3, 4)};
    assert(eq(arrangement(friends, distances),
              // Total cost: 2.4 + 1.1 = 3.5
              std::map<int, int>{{1, 3}, {2, 2}, {3, 4}, {4, 4}}));
  };

  "MoreBackedges"_test = [&] {
    std::vector<std::pair<int, int>> friends{
        std::make_pair(1, 2), std::make_pair(1, 3), std::make_pair(1, 4),
        std::make_pair(2, 1), std::make_pair(2, 3), std::make_pair(2, 4),
        std::make_pair(3, 4), std::make_pair(4, 1), std::make_pair(4, 2),
        std::make_pair(4, 3)};
    assert(eq(arrangement(friends, distances),
              // Total cost: 2.4 + 1.1 + 3.2 = 6.7
              std::map<int, int>{{1, 3}, {2, 1}, {3, 3}, {4, 3}}));
  };

  "FullyConnected"_test = [&] {
    std::vector<std::pair<int, int>> friends{
        std::make_pair(1, 2), std::make_pair(1, 3), std::make_pair(1, 4),
        std::make_pair(2, 1), std::make_pair(2, 3), std::make_pair(2, 4),
        std::make_pair(3, 1), std::make_pair(3, 4), std::make_pair(4, 1),
        std::make_pair(4, 1), std::make_pair(4, 2), std::make_pair(4, 3)};
    assert(eq(arrangement(friends, distances),
              // Total cost: 3.2 + 2.4 + 1.1 = 6.7
              std::map<int, int>{{1, 1}, {2, 1}, {3, 1}, {4, 3}}));
  };
}
