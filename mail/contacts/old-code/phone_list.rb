##
## 
##

require "./rolodex"


$width = 79

def display(left, fill, right)
  result = left

  while (result.length + right.length < $width)
    result += fill
  end

  puts result + right
end



def display_entry entry
  name  = entry["name"]
  first = true

  for field in %w{cell home work home2 phone}
    number = entry[field]
    number = number.to_s  # <<<>>>
    next unless number
    next unless number !~ /^\s*$/

    if field == "cell"
      number += "    "
    elsif field == "home2"
      number += " [2]"
    else
      number += " [#{field[0..0]}]"
    end
    
    if first
      display(name+" ", ".", " "+number)
      first = false
    else
      display("", " ", number)
    end
  end

  if comment = entry["phone-comment"]
    display(name+" ", ".", "                   ") if first
    puts "    #{comment}".sub(/\n$/, "").gsub(/\n/, "\n    ")
  end
end



def display_entries(rolodex)
  rolodex.map { |x| display_entry(x) }
end

def phone_list rolodex
  display_entries(rolodex.reject { |x| x["type"] })
  puts "\n"*4
  display_entries(rolodex.reject { |x| !x["type"] })
end


#R = Rolodex.load(File.open("out.txt"))
R = Rolodex.load($<)

phone_list(R)
