##
## The new rolodex data type
##

require 'yaml'
require './patch'
# backward compatibility with 1.9.3 for now:
YAML::ENGINE.yamler = "syck"


class Rolodex
  include Enumerable

  #
  # basic operations
  #

  def initialize
    @names = []
    @entries = {}
  end

  def add entry
    name = entry["name"]
    fail "entry without name: #{entry.inspect}" if !name
    fail "duplicate name: #{name}" if @names.include?(name)
    
    @names << name.dup
    @entries[name] = entry.dup
    self
  end
  

  def keys
    @names.sort!.dup
  end
  
  def [](name)
    entry = @entries[name]
    entry = entry.dup if entry
    entry
  end
  

  #
  # derived operations
  #

  def each
    keys.each { |name| yield self[name] }
    nil
  end

  def map
    result = Rolodex.new
    keys.each do |name|
      output = yield(self[name])
      result.add(output) if output
    end
    result
  end

  def reject
    result = Rolodex.new
    keys.each do |name|
      entry = self[name]
      result.add(entry) if !yield(entry)
    end
    result
  end

  
  def dump(sink=$stdout)
    result = {}

    each do |entry|
      name = entry["name"]
      entry.delete("name")

      result[name] = entry
    end

    YAML.dump(result, sink)
  end

  def Rolodex.load source
    result = Rolodex.new
    YAML.load(source.read).each_pair do |k,v|
      v["name"] = k
      result.add v
    end
    result
  end


  def partner_of name
    entry = self[name]
    fail "no such person: #{name}" if !entry

    for field in %w{SO fiance fiancee spouse}
      pointer = entry[field]
      pointer = reverse_name(pointer) if pointer && pointer !~ /,/

      return self[pointer] if pointer
    end

    nil
  end

end


# turn "(a) b c (d) e (f) (g)" into "e (f) (g), (a) b c (d)"...
def reverse_name full_name
  first = full_name
  last = ""
  
  first = first.sub(/\s*([\w.\-]+\s*(\([^\)]*\)\s*)*)\z/) do |suffix|
    last = $1
    ""
  end
  
  return "#{last}, #{first}" if last && first != ""
  return first + last
end


# turn "a b, c d" into "c d a b"
def restore_name name
  return name if name !~ /(.*), *(.*)/

  "#{$2} #{$1}"
end



#
# Return all known email addresses for entry in order by most desirable to use
#
def get_email entry
  email = []

  for field in %w{email home-email work-email}
    value = entry[field]
    next if !value

    value = [value] if !value.respond_to?(:to_ary)

    email += value
  end

  email
end
