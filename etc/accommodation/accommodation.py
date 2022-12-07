from typing import Dict, List, Tuple, Set
from collections import defaultdict

# Variation of Kruskal's algorithm for directed graph: time complexity O(E log E)
# where E is the number of edges in the graph (the number of friendships)
def arrangement(friends: List[Tuple[int, int]],
                distances: Dict[Tuple[int, int], float]) -> Dict[int, int]:
  # Adjacency list to represent the graph
  adj_list : Dict[int, List[int]] = defaultdict(list)

  # Dictionary to store the minimum distances from each node to the tree
  min_distances : Dict[int, float] = {}

  for f1, f2 in friends:
    # Friendships are assymmetric
    adj_list[f1].append(f2)
    min_distances[f1] = float('inf')
    min_distances[f2] = float('inf')

  # Set of visited nodes to keep track of which nodes have been added to the tree
  visited : Set[int] = set()

  # Start with some root
  first_key : int = list(adj_list.keys())[0]
  min_distances[first_key] = 0

  # Minimum spanning tree dictionary
  mst : Dict[int, int] = {}

  # While there are still unvisited nodes in the graph
  while len(visited) < len(adj_list.keys()):
    # Find the unvisited node with the minimum distance to the tree
    min_node = None
    min_distance = float('inf')
    for node, distance in min_distances.items():
        if node not in visited and distance < min_distance:
            min_node = node
            min_distance = distance

    # This case should never be hit
    assert(min_node)

    # Update the minimum distances for all the node's neighbors
    for neighbor in adj_list[min_node]:
      if neighbor not in visited:
        min_distances[neighbor] = min(min_distances[neighbor], distances[(min_node, neighbor)])

    # Update the minimum spanning tree with the neighbor node lying at minimum distance
    min_distance = float('inf')
    for neighbor in adj_list[min_node]:
      if min_distances[neighbor] < min_distance:
        mst[min_node] = neighbor
        min_distance = min_distances[neighbor]

    # Mark min_node as visited
    visited.add(min_node)

  return mst
