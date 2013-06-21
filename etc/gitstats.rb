def main
  majors = [7, 8]
  minors = (0..11).to_a
  majors.each do |major|
    minors.each do |minor|
      if major == 8 and minor == 3
        rev_range = "v1.#{major}.#{minor}..master"
      else
        next if not system("git rev-parse v1.#{major}.#{minor + 1} >/dev/null")
        rev_range = "v1.#{major}.#{minor}..v1.#{major}.#{minor + 1}"
      end
      sl_nums = `git shortlog -ns --no-merges #{rev_range} | cut -f 1 | head -n 10`.split("\n").map { |x| Integer x }
      sl_names = `git shortlog -ns --no-merges #{rev_range} | cut -f 2 | head -n 10`.split("\n").map do |name|
        name.split(" ", 2).map { |x| x[0].downcase }.join
      end
      sl_str = (sl_names.zip sl_nums).map { |zipped| "#{zipped[0]} (#{zipped[1]})" }.join(", ")
      rl_str = `git rev-list --count #{rev_range}`
      next if not rl_str.length > 0
      puts "#{rev_range}:: #{rl_str} #{sl_str}"
    end
  end
end

main
