##
## Generate a convenient concise list of phone numbers from a Rolodex
##

require "./code/rolodex"


$width = 79

def display(left, fill, right)
  result = left

  while (result.length + right.length < $width)
    result += fill
  end

  puts result + right
end



def display_entry entry
  name  = entry.key
  first = true

  for field in %w{cell home work home2 work2 phone}
    number = entry.first(field)  # ignore all but first number of each field
    next unless number

    number += " " + case field
                      when "cell"  then "   "
                      when "home2" then "[2]"
                      when "work2" then "[W]"
                      else              "[#{field[0..0]}]"
                    end
    
    if first
      display(name+" ", ".", " "+number)
      first = false
    else
      display("", " ", number)
    end
  end

  if entry.phone_comment then
    comment = entry.multiline("phone-comment")
    display(name+" ", ".", "                   ") if first
    puts "    #{comment}".gsub(/\n/, "\n    ")
  end
end



def display_entries(rolodex)
  rolodex.each { |x| display_entry(x) }
end

def phone_list rolodex
  display_entries(rolodex.reject { |x|  x.type })
  puts "\n"*4
  display_entries(rolodex.reject { |x| !x.type })
end


R = Rolodex.load($<)

phone_list(R)
