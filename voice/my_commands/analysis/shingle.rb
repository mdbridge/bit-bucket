fail "shingle.rb: usage: <shingle_size>" unless ARGV.length == 1

size = ARGV[0].to_i

window = Array.new(size) { nil }


$stdin.each do |line|
  line.chomp!

  if window[0] != nil then
    puts window.collect { |x| x }.join("\tTHEN\t")
  end
  
  window.shift
  window.push(line)
end

