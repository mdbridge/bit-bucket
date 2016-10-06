letters = File.open("VoiceCode_letters").readlines().reject { |x| x =~ /^\#/ }
  .collect { |x| x.split }

letters.each do |w1, s1|
  letters.each do |w2, s2|
    #puts ".#{w1}#{w2}: #{s1} #{s2}"
    puts "#{w1}#{w2}: #{s1} #{s2}"
    #puts "#{w1}#{w2}: engrave #{s1} #{s2}"
  end
end

letters.each do |w1, s1|
  puts "#{w1.upcase}: sky #{s1}"
#  puts "#{w1}: engrave #{s1}"
  puts "#{w1}: #{s1}"
end

letters.each do |w1, s1|
  puts "#{w1}: #{s1} ling"
end
