$words = 1
$words = ARGV[0].to_i unless ARGV.length<1

def get_key(line)
  line.chomp().split(/\t/)[0..($words-1)].join("|")
end

def get_description(line)
  line.chomp().split(/\t/)[0..($words-1)].join("|")
end


last             = $stdin.readline()
last_key         = get_key(last)
last_description = get_description(last)
count            = 1


$stdin.each do |line|
  new = line

  if last_key == get_key(new) then
    count += 1
  else
    puts "#{count}\t#{last_description}"
    last             = new
    last_key         = get_key(new)
    last_description = get_description(new)
    count            = 1
  end
end

puts "#{count}\t#{last_description}"
