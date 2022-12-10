#include "student-accommodation.hpp"
#include <algorithm>
#include <iterator>
#include <numeric>
#include <set>

// A simple class for generating permutations given N vertices, and legalizing
// them given the possible legal edges
class PermutationGenerator {
  std::vector<std::vector<std::pair<int, int>>> permutations;

public:
  // Heap's algorithm for generating permutations
  // Time complexity: O(V!)
  void generate(std::vector<std::pair<int, int>> arr, size_t sz) {
    if (sz == 1) {
      permutations.push_back(arr);
      return;
    }

    generate(arr, sz - 1);

    for (size_t i = 0; i < sz - 1; i++) {
      if (sz % 2 == 1)
        std::swap(arr[0].second, arr[sz - 1].second);
      else
        std::swap(arr[i].second, arr[sz - 1].second);
      generate(arr, sz - 1);
    }
  }

  // Remove the edges that are not legal and return them
  // Time complexity: O(V!)
  std::vector<std::vector<std::pair<int, int>>>
  legalize(const std::set<std::pair<int, int>> &edges) {
    // Erase unless all of the edges satisfy one of the following conditions:
    // 1. The edge is a self-edge
    // 2. The edge is legal
    std::erase_if(permutations, [&edges](auto permutation) {
      return !std::all_of(
          permutation.begin(), permutation.end(), [&edges](auto edge) {
            return (edge.first == edge.second) || edges.count(edge);
          });
    });
    return permutations;
  }
};

// We have a directed weighted graph (vertices are students, and edges are
// friendship relations), which is combined with a permutation problem (since no
// two students can be assigned the same room). We present an optimal solution
// to the problem, albeit in O(V!)
std::map<int, int>
arrangement(const std::vector<std::pair<int, int>> &friends,
            const std::map<std::pair<int, int>, double> &distances) {
  // Copy out the edges into an std::set for O(1) lookup
  std::set<std::pair<int, int>> edges;
  std::copy(friends.begin(), friends.end(), std::inserter(edges, edges.end()));

  // De-duplicate the vertices
  std::set<int> vertices;
  for (auto [fst, snd] : edges) {
    vertices.insert(fst);
    vertices.insert(snd);
  }

  // Initialize the permutations
  std::vector<std::pair<int, int>> permutationInit;
  for (auto vertex : vertices)
    permutationInit.push_back(std::make_pair(vertex, vertex));

  // Use the permutation generator to generate and legalize permutations
  PermutationGenerator gen;
  gen.generate(permutationInit, permutationInit.size());
  auto legalEdgesPermutation = gen.legalize(edges);

  // We have two cost models
  // displacementCost computes the total cost of displacement, with the intent
  // of minimizing this cost
  auto displacementCost =
      [&distances](const std::vector<std::pair<int, int>> &edgeVector) {
        return std::accumulate(edgeVector.begin(), edgeVector.end(), 0,
                               [&distances](int sum, auto edge) {
                                 return sum + distances.at(edge);
                               });
      };

  // shuffleCost computes the total cost of a non-self-edge, with the intent of
  // maximizing this cost
  auto shuffleCost = [](const std::vector<std::pair<int, int>> &edgeVector) {
    return std::accumulate(
        edgeVector.begin(), edgeVector.end(), 0,
        [](int sum, auto edge) { return sum + (edge.first != edge.second); });
  };

  // Sort the permutations by the minimum total weight
  // Time complexity: O(V)
  std::sort(legalEdgesPermutation.begin(), legalEdgesPermutation.end(),
            [&displacementCost](auto edges1, auto edges2) {
              return displacementCost(edges1) < displacementCost(edges2);
            });

  // Now pick the greatest shuffle in the sorted legal permutations, as defined
  // by the cost model
  // Time complexity: O(V)
  auto maxEl = std::max_element(
      legalEdgesPermutation.begin(), legalEdgesPermutation.end(),
      [&shuffleCost](auto edges1, auto edges2) {
        return shuffleCost(edges1) < shuffleCost(edges2);
      });

  // Prepare to return an std::map
  std::map<int, int> allocation;
  for (auto [k, v] : *maxEl)
    allocation[k] = v;
  return allocation;
}
