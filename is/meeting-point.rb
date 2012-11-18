class House
  attr_accessor :x, :y
  def compute_dist (candidate)
    x_dist = (self.x - candidate.x).abs
    y_dist = (self.y - candidate.y).abs
    [x_dist, y_dist].min + (x_dist - y_dist).abs
  end
end

def main
  n_houses = Integer(STDIN.gets)
  houses = []
  (1..n_houses).each {
    this_house = House.new
    this_house.x, this_house.y = STDIN.gets.split(' ').map { |x| Integer(x) }
    houses << this_house
  }
  p houses.map { |house|
    houses.map { |candidate|
      house.compute_dist(candidate)
    }.reduce(:+)
  }.min
end

main
