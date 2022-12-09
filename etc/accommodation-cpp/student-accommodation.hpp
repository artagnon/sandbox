#include <map>
#include <utility>
#include <vector>

/**
 * Find an efficient arrangement of the students in the building.
 *
 * @param friends vector of all friendship relations between students
 * @param distances map taking each pair of distinct rooms to the distance
 * between them
 *
 * @return mapping of rooms to students
 */
std::map<int, int>
arrangement(const std::vector<std::pair<int, int>> &friends,
            const std::map<std::pair<int, int>, double> &distances);
