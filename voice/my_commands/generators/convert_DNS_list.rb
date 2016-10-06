## 
## Convert each given legacy DNS list to a Vocola list:
## 

require '~/voice/my_commands/generators/priority_list'


# 
# Convert a legacy DNS list to a corresponding PriorityList.
#
# The DNS format is lines of the form: written\spoken where \spoken is 
# optional.
#
# We extend this by allowing blank lines and written but not spoken to
# contain backslashes.
# 
def convert_DNS_list list_text
  list = PriorityList.new

  list_text.split(/\n/).each do |line|
    line.chomp!
    next if line =~ /^\s*$/

    line = "#{line}\\#{line}" if line !~ /\\/
    line =~ /(.*)\\([^\\]*)/

    list.add($2, $1)
  end

  return list
end



fail "usage: list_name < DNS_list" unless ARGV.length == 1
list_name = ARGV[0]

converted = convert_DNS_list($stdin.read())

puts ListGenerator.banner
puts converted.generate(list_name)
