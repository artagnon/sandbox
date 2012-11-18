def main
  n_var, k_var = STDIN.gets.split(' ').map { |x| Integer(x) }
  numbers = STDIN.gets.split(' ').map { |x| Integer (x) }
  p numbers.map { |first|
    (numbers[numbers.index(first)..numbers.length - 1]).select { |second|
      (first - second).abs == k_var
    }.length
  }.reduce(:+)
end

main
