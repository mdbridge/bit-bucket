### 
### A simple implementation of a Rolodex class
### 

require 'set'


class RolodexError < StandardError
end



## 
## Parsing routines
## 

  # requires: separator not match newlines
def numbered_split(string, separator, start_line=1)
  n = start_line
  string.split(separator).collect do |piece|
    old = n
    n += piece.count("\n")
    [piece, old]
  end
end

def numbered_partition(string, separator, start_line=1)
  head, match, tail = string.partition(separator)
  tail = nil if string !~ separator
  n = start_line + head.count("\n") + match.count("\n")
  [head, tail, n]
end



## 
## A Rolodex entry: a key plus zero or more fields, each of which
##                  contains a list of values
## 

class RolodexEntry
  include Enumerable


  @@known_fields = %w{type home-email work-email phone-comment}.to_set

  def initialize(key, fields=[])
    raise RolodexError, "bad key: '#{key}'" unless RolodexEntry.valid_key?(key)
    @key    = key.dup.freeze
    @fields = {}
    fields.to_a.each do |field, values|
      unless RolodexEntry.valid_field_name?(field)
        raise RolodexError, "bad field name: '#{field}'" 
      end
      if @fields[field]
        raise RolodexError,"duplicate field name '#{field}' in entry for '#{key}'" 
      end
      field = field.dup.freeze
      @@known_fields << field
      values = values.collect do |value|
        raise RolodexError, "bad value: '#{value}'" if 
          value =~ /(^\s) | \n | \r | (\s$)/x or not String === value
        value.dup
      end
      @fields[field] = values
    end
  end

  def clone
    RolodexEntry.new(@key, @fields)
  end


  def key   # returns frozen string
    @key
  end

  def each  # yields frozen field names
    @fields.sort.each { |field,values| yield field,values }
  end

  # return list of values or nil if field not present
  def [](field)
    @fields[field]
  end


  # return first value or nil if zero values or field not present
  def first(field)
      (@fields[field] || [])[0]
  end

  # return list of values, empty if field not present
  def all(field)
    @fields[field] || []
  end

  # return a single string containing the values separated by new lines,
  # or nil if field not present
  def multiline(field)
    values = @fields[field]
    values = values.join("\n")  if values
    values
  end

  def method_missing(method_ID, *args)
    method_name = method_ID.to_s.gsub(/_/, "-")
    # .field_name => .first("field-name")
    if @@known_fields.member?(method_name) then
      first(method_name)
    # .field_names => .all("field-name")
    elsif method_name =~ /s$/ and @@known_fields.member?(method_name[0..-2]) then
      all(method_name[0..-2])
    else
      super
    end
  end


  # Return all known email addresses in order by most desirable to use
  def get_all_email 
    self.emails + self.home_emails + self.work_emails
  end


  # show these fields first if present then remaining fields alphabetically:
  @@priority_fields = %w{
type
full 
nickname
nickname-1
nickname-2
spoken
spouse
SO
fiance
fiancee
cell
phone
phone-comment
email
email-comment
home
home-email
home-address
work
work-fax
work2
job-title
company
department
work-email
work-address
post-number
assistant
home2
home2-address
child
birthday
}

  def to_s
    result = @key + ":\n"
    order = @@priority_fields +
      self.collect { |f,v| f unless @@priority_fields.include?(f) }
    order.each do |field|
      values = self[field]
      next unless values
      result += "  #{field}:" + if values.size == 1 then
                                  " #{values[0]}\n"
                                else
                                  "\n" + values.collect { |v| "    #{v}\n" }.join
                                end
    end
    result
  end


  def RolodexEntry.valid_key? key
    # a key can be any string without leading or trailing whitespace
    # that does not contain a colon, newline, or carriage return:
    key == key.strip and key !~ /[:\n\r]/
  end

  def RolodexEntry.valid_field_name? field_name
    # field names are one or more words seperated by hyphens.
    # by convention, singular.
    field_name =~ /\A \w+ (-+\w+)* \z/x
  end

  def RolodexEntry.parse(text, start_line=1)
    key, rest, n = numbered_partition(text, /:\s*\n/, start_line)
    unless rest 
      raise RolodexError, 
              "line #{start_line}: bad start of entry (missing ':' at line end?)"
    end
    unless valid_key?(key)
      raise RolodexError, "line #{start_line}: invalid key: '#{key}'"
    end
    # fields start with lines indented less than four spaces:
    fields = numbered_split(rest, /^(?= ? ? ?[^\s])/, n).
      collect { |t,n| RolodexEntry.parse_field(t,n) } 
    RolodexEntry.new(key, fields)
  end

  def RolodexEntry.parse_field(text, start_line=1)
    first, rest, n = numbered_partition(text, /:[ \t]*\n?/, start_line)
    unless rest 
      raise RolodexError, "line #{start_line}: bad start of field (missing ':'?)"
    end
    field_name = first.lstrip
    unless valid_field_name?(field_name)
      raise RolodexError, "line #{start_line}: invalid field name: '#{field_name}'"
    end
    values = rest.split(/\n/).collect(&:strip)
    return field_name, values
  end
end



## 
## The Rolodex class itself: a map from keys to RolodexEntry's
## 

class Rolodex
  include Enumerable


  def initialize(entries=[])
    @entries = {}
    entries.each { |x| add(x) }
  end

  def add entry
    key = entry.key
    raise RolodexError, "duplicate key: '#{key}'" if @entries.keys.include?(key)
    
    @entries[key] = entry.clone
    self
  end

  def restrict(keep_partners=false)
    result = Rolodex.new
    self.each do |entry|
      if yield entry then
        result.add entry unless result[entry.key]
        next unless keep_partners
        partner = partner_of entry.key
        result.add partner unless !partner or result[partner.key]
      end
    end
    result
  end


  def keys
    @entries.keys.sort
  end

  def each
    @entries.keys.sort.each { |key| yield @entries[key] }
  end

  def [](key)
    @entries[key]
  end

  def partner_of name
    entry = self[name]
    raise RolodexError, "no such person: '#{name}'" if !entry

    for field in %w{SO fiance fiancee spouse}
      pointer = entry.first(field)
      pointer = reverse_name(pointer) if pointer && pointer !~ /,/

      return self[pointer] if pointer
    end

    nil
  end


  def dump(sink=$stdout)
    each { |entry| sink.puts entry.to_s }
  end

  def Rolodex.load source
    text = source.read().gsub(/\r/, "")
    # An un-indented line starts each entry:
    entries = numbered_split(text, /^(?![ \t])/).
      collect { |t,n| RolodexEntry.parse(t, n) }
    Rolodex.new entries
  end
end



## 
## Utility functions useful for dealing with contacts
## 

# turn "(a) b c (d) e (f) (g)" into "e (f) (g), (a) b c (d)"...
def reverse_name full_name
  first = full_name
  last  = ""
  
  first = first.sub(/\s*([\w.\-]+\s*(\([^\)]*\)\s*)*)\z/) do |suffix|
    last = $1
    ""
  end
  
  return "#{last}, #{first}" if last != "" and first != ""
  return first + last
end


# turn "a b, c d" into "c d a b"
def restore_name name
  return name if name !~ /(.*), *(.*)/

  "#{$2} #{$1}"
end
