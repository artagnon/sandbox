def num_author rev_range, pattern
  num = Integer `git rev-list --count --author=\"#{pattern}\" #{rev_range}`
  puts "#{rev_range}:: #{num}"
end

def sl_summary rev_range
  sl_nums = `git shortlog -ns --no-merges #{rev_range} | cut -f 1 | head -n 10`.split("\n").map { |x| Integer x }
  sl_names = `git shortlog -ns --no-merges #{rev_range} | cut -f 2 | head -n 10`.split("\n").map do |name|
    name.split(" ", 2).map { |x| x[0].downcase }.join
  end
  sl_str = (sl_names.zip sl_nums).map { |zipped| "#{zipped[0]} (#{zipped[1]})" }.join(", ")
  rl_str = `git rev-list --count #{rev_range}`
  return if not rl_str.length > 0
  puts "#{rev_range}:: #{rl_str} #{sl_str}"
end

def main
  lower_bound = "v1.7.0"
  tag_list = `git for-each-ref --sort=taggerdate --format="%(refname:short)" refs/tags`.split("\n")
  major_versions = tag_list.select { |version| /^v\d+.\d+.\d+$/ =~ version }
  (major_versions.drop_while { |x| x != lower_bound } + ["master"]).each_cons(2) do |pair|
    rev_range = "#{pair[0]}..#{pair[1]}"
    sl_summary rev_range
  end
end

main
