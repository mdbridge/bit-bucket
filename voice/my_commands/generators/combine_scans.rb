### 
### Combine one or more scan results (cf. scan_Unix.rb) into a single Vocola list
### 

require '~/voice/my_commands/generators/priority_list'


## 
## Extend PriorityList to somewhat better handle converting written to
## spoken forms:
## 

Numbers = %w{one two three four five six seven eight nine}

def spoken written
  s = " " + written + " "

  # Break apart into pieces:
  s = s.gsub(/_/, " ").gsub(/-/, " ").
    gsub(/([a-zA-Z])([A-Z][a-z])/) { |x| "#$1 #$2" }.
    gsub(/([a-z])([0-9])/i)        { |x| "#$1 #$2" }.
    gsub(/([0-9])([a-z])/i)        { |x| "#$1 #$2" }

  # Remove leading zeros:
  s = s.gsub(/ 0+([1-9])/) { |x| " #$1" }

  # Spell out single digits (1-9):
  1.upto(Numbers.length) do |i|
    s = s.gsub(/ #{i} /, " #{Numbers[i-1]} ")
  end

  s.sub(/^ +/, "").sub(/ +$/, "")
end


class DragonList < PriorityList
  def add(key, value, priority=0)
    #puts "#{key}[#{spoken(key)}] = #{value} @#{priority}"
    super(spoken(key), value, priority)
  end
end


## 
## Main routine:
## 

fail unless ARGV.length >= 2

use_components = ARGV[0]=="-components"
ARGV.shift if use_components

list_name = ARGV[0]
scans     = ARGV[1..-1]


def filename directory
  directory[%r_[^/]*$_]
end

list = DragonList.new

scans.each do |scan_file|
  raw  = File.read(scan_file)
  scan = raw.gsub(/\s*(\#.*)?$/, "").sub(/^\s*\n/, "").
    split(/\n/).collect { |x| x.split(/,\s*/) }

  scan.each do |pathname, name, priority|
    pathname = pathname.gsub(%r_^/home/mdl_, "~")
    if use_components then
      # skip places as components:
      next if pathname =~ %r<^~/Tmp/places/>
      # don't move down a top level point:
      next if pathname =~ %r<^~/[^/]*$>

      pathname = filename(pathname) 
      next if pathname =~ /^(~|\.)?$/
    end
    
    name     = filename(pathname) unless name and name != ""
    priority = priority.to_i

    list.add(name, pathname, priority)
  end
end

puts ListGenerator.banner
puts list.generate(list_name)
