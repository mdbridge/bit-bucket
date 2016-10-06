def pad(number)
  ("00" + number.to_s)[-2..-1]
end

def spoken number
  if number < 10
    %w{zero one two three four five six seven eight nine}[number]
  else
    number
  end
end

def pattern(from, to)
  list = []
  from.upto(to) {|x| list << pad(x)}
  list = list.join(" ")

  list.gsub!(/(\d)(\d)( \1\d)* \1(\d)/) { "#$1[#$2-#$4]" }
  list.gsub!(/ /, '\|')
  list
end




File.open("mini-lists/this-day.list", "w") do |file|
  1.upto(31) do |day|
    file.puts "#{pattern(day,31)}\\#{spoken(day)}"
  end
end

File.open("mini-lists/next-day.list", "w") do |file|
  1.upto(31) do |day|
    file.puts "#{pad(day)},#{pattern(day+1,32)}\\#{spoken(day)}"
  end
end
