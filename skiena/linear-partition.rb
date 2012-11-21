def min_cost numbers, i, k_partitions
  if i == 0
    return numbers[0]
  end
  if k_partitions == 0
    return numbers[0 .. i].reduce(:+)
  end
  return (0 .. i - 1).map { |j|
    [min_cost(numbers, j, k_partitions - 1), numbers[j + 1 .. i].reduce(:+)].max
  }.min
end

def main
  numbers = STDIN.gets.split(' ').map { |x| Integer x }
  k_partitions = Integer STDIN.gets
  puts min_cost numbers, numbers.size - 1, k_partitions
end

main
