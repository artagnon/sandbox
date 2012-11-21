def lis numbers, start, predecessor
  candidates = []
  if start > numbers.length - 1
    return []
  end
  (start .. numbers.length - 1).each { |i|
    if not predecessor or numbers[i] > predecessor
      candidates << [i, numbers[i]]
    end
  }
  return candidates.map { |index, bigger_num|
    [bigger_num] + lis(numbers, index + 1, bigger_num)
  }.max_by(&:length)
end

def main
  n_sequences = Integer STDIN.gets
  sequences = []
  (1..n_sequences).each {
    sequences << STDIN.gets.split(' ').map { |x| Integer x }
  }
  sequences.each { |sequence|
    puts lis(sequence, 0, nil).join(' ')
  }
end

main
