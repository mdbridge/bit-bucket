#COUNT = 1000
COUNT = 3000

words = File.read("/usr/share/dict/american-english-huge").scan(/^[A-Za-z]+$/)

samples = 0.upto(COUNT).collect { |x| words.sample(3) }

samples.each do |first, second, third|
  puts "command #{first} #{second} #{third} = '#{first} #{second} #{third}';"
  #puts "command #{first} #{second} #{third} (#{first}|#{second}|#{third}) = '#{first} #{second} #{third}';"
end
