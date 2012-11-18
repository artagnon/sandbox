def compute (score_list)
end

def main
  n_cases = Integer STDIN.gets
  scores = []
  (1..n_cases).each do
    n_players = Integer STDIN.gets
    scores << STDIN.gets.split(' ').map { |x| Integer x }
  end
  scores.each { |score_list|
    score_list.length
  }
end
