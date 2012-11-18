class Point
  attr_accessor :x, :y

  def reflect_x
    @y = -@y
  end
  def reflect_y
    @x = -@x
  end
end

class Query
  attr_accessor :type, :i, :j
end

def find_quadrant points
  quad_vec = [0, 0, 0, 0]
  points.each { |point|
    if point.x > 0 and point.y > 0
      quad_vec[0] += 1
    elsif point.x < 0 and point.y > 0
      quad_vec[1] += 1
    elsif point.x < 0 and point.y < 0
      quad_vec[2] += 1
    else
      quad_vec[3] += 1
    end
  }
  quad_vec
end

def main
  n_points = Integer(STDIN.gets)
  points = []
  (1..n_points).each {
    this_point = Point.new
    this_point.x, this_point.y = STDIN.gets.split(' ').map { |x| Integer x }
    points << this_point
  }
  n_queries = Integer(STDIN.gets)
  queries = []
  (1..n_queries).each {
    this_query = Query.new
    q_str = STDIN.gets.split(' ')
    this_query.type = q_str[0]
    this_query.i = Integer q_str[1]
    this_query.j = Integer q_str[2]
    queries << this_query
  }
  queries.each { |query|
    slice = points[query.i - 1 .. query.j - 1]
    case query.type
    when "X"
      slice.each { |point| point.reflect_x }
    when "Y"
      slice.each { |point| point.reflect_y }
    when "C"
      quad_vec = find_quadrant slice
      puts "#{quad_vec[0]} #{quad_vec[1]} #{quad_vec[2]} #{quad_vec[3]}"
    end
  }
end

main
