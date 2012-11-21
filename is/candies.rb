def main
  n_cases = Integer STDIN.gets
  ratings = []
  candies = []
  (1..n_cases).each {
    ratings << Integer(STDIN.gets)
    candies << 1
  }
  while (true)
    modified = false
    (0..ratings.size - 2).each { |i|
      if ratings[i] > ratings[i + 1]
        if candies[i] <= candies[i + 1]
          candies[i] = candies[i + 1] + 1
          modified = true
        end
      elsif ratings[i] < ratings[i + 1]
        if candies [i] >= candies [i + 1]
          candies[i + 1] = candies[i] + 1
          modified = true
        end
      end
    }
    if not modified
      break
    end
  end
  puts candies.reduce(:+)
end

main
