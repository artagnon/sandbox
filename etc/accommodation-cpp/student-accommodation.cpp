#include "student-accommodation.hpp"
#include <algorithm>
#include <set>
#include <unordered_map>

// Simple disjoint-set data structure with makeSet, findSet and unionSet member
// functions
class DisjointSet {
  std::unordered_map<int, int> parent;

public:
  void makeSet(std::set<int> &init) {
    for (auto i : init) {
      parent[i] = i;
    }
  }

  // Simple recursive find that terminates at parent
  int findSet(int needle) {
    if (parent[needle] == needle)
      return needle;
    return findSet(parent[needle]);
  }

  void unionSet(int m, int n) { parent[findSet(m)] = findSet(n); }
};

class PermutationGenerator {
  std::vector<std::vector<int>> permutations;

public:
  void generate(std::vector<int> arr, size_t sz) {
    if (sz == 1) {
      permutations.push_back(arr);
      return;
    }

    for (int i = 0; i < sz; i++) {
      generate(arr, sz - 1);

      if (sz % 2 == 1)
        std::swap(arr[0], arr[sz - 1]);

      else
        std::swap(arr[i], arr[sz - 1]);
    }
  }

  operator std::vector<std::vector<int>>() { return permutations; }
};

// A person is a vertex in the graph, and assymmetric friend-relationships are
// directed edges in the graph. The weight of each graph edge is given by
// distances. The objective is to span the graph, minimizing the weight of the
// spanning tree: in other words, we need something like a minimal spanning
// tree. The solution is inspired by Kruskal's algorithm.
//
// Time complexity: O(E log V), where E is the number of edges and V is the
// number of vertices.
std::map<int, int>
arrangement(const std::vector<std::pair<int, int>> &friends,
            const std::map<std::pair<int, int>, double> &distances) {

  // Create a vector of edges, that drops the const of friends, so we can sort
  // the edges by weight
  std::vector<std::pair<int, int>> edges = friends;

  // De-duplicate the vertices
  std::set<int> vertices;
  for (auto [fst, snd] : edges) {
    vertices.insert(fst);
    vertices.insert(snd);
  }

  // Initialize the forests by putting each vertex in its own forest
  DisjointSet forest;
  forest.makeSet(vertices);

  // Use a multimap for the maximal spanning tree, with multiple possible
  // entries for each key
  std::multimap<int, int> mst;

  // Sort the edges by weight
  std::sort(edges.begin(), edges.end(), [&distances](auto edge1, auto edge2) {
    return distances.at(edge1) < distances.at(edge2);
  });

  // For each edge in the graph, use our DisjointSet data structure to perform
  // findSet and unionSet.
  for (auto [fst, snd] : edges) {
    // If the subforest that contains the first node is not the same subforest
    // that contains the second node, add to minimal spanning tree, and merge
    // the two forests
    if (forest.findSet(fst) != forest.findSet(snd)) {
      mst.insert(std::make_pair(fst, snd));
      forest.unionSet(fst, snd);
    }
  }

  // First, copy the non-duplicates in the multimap into allocation to return
  std::map<int, int> allocation;
  std::copy_if(mst.begin(), mst.end(),
               std::inserter(allocation, allocation.end()),
               [&mst](auto kv) { return mst.count(kv.first) == 1; });

  // Find the duplicates in the multimap
  std::vector<std::pair<int, int>> duplicates;
  std::copy_if(mst.begin(), mst.end(), std::back_inserter(duplicates),
               [&mst](auto kv) { return mst.count(kv.first) > 1; });

  // Now, sort the duplicates by weight of edges
  std::sort(
      duplicates.begin(), duplicates.end(),
      [&edges, &distances](auto kv1, auto kv2) {
        bool backEdge1 =
            std::find(edges.begin(), edges.end(),
                      std::make_pair(kv1.second, kv1.first)) == edges.end();
        bool backEdge2 =
            std::find(edges.begin(), edges.end(),
                      std::make_pair(kv2.second, kv2.first)) == edges.end();

        if (backEdge1 && !backEdge2)
          return true;
        else if (backEdge2 && !backEdge1)
          return false;

        return distances.at(kv1) < distances.at(kv2);
      });

  // Fill the allocation map with the lowest weight duplicates
  for (auto [k, v] : duplicates) {
    if (!allocation.count(k))
      allocation[k] = v;
  }

  // To finish the problem, let unallocated students sit in place
  for (auto v : vertices) {
    if (!allocation.count(v))
      allocation[v] = v;
  }

  return allocation;
}
