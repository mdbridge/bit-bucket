### 
### Attempt to merge two Rolodexes
### 

require "./code/rolodex"


fail unless ARGV.size == 2

First_Rolodex  = Rolodex.load(File.open(ARGV[0]))
Second_Rolodex = Rolodex.load(File.open(ARGV[1]))


def merge_entries(first_entry, second_entry)
  key = first_entry.key

  first_hash  = Hash[first_entry.to_a]    # .to_h in Ruby 2.1.0...
  second_hash = Hash[second_entry.to_a]
  fields = first_hash.merge(second_hash) { |field,o,n| merge_field(key,field,o,n) }

  RolodexEntry.new(key, fields)
end

def merge_field(key, field, first_values, second_values)
  return first_values if first_values == second_values

  $stderr.puts "***** Conflict on #{field} field of #{key}"
  #first_values
  fail  # <<<>>>
end


R = Rolodex.new

First_Rolodex.each do |first_entry|
  if second_entry = Second_Rolodex[first_entry.key] then
    R.add merge_entries(first_entry, second_entry)
  else
    R.add first_entry
  end
end

Second_Rolodex.each do |second_entry|
  R.add(second_entry) unless R[second_entry.key]
end

R.dump
