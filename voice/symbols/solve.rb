def correction(english_text)
  corrected = {}
  File.read("history").scan(/^([^,]+),(.*)$/) do |symbol, english|
    corrected[english.downcase] = symbol
  end

  corrected[english_text.downcase] 
end

def invert(symbol_text)
  corrected = {}
  File.read("history").scan(/^([^,]+),(.*)$/) do |symbol, english|
    corrected[symbol] = english
  end
  
  corrected[symbol_text] 
end


def match(english)
  words = english.split(/\s+/)

  symbols = File.read("symbols").split(/\n/)

  pattern = words.collect { |x| x[0] + x[1..-1].gsub(/./, "\\&?") }.join("")
  regexp = /^#{pattern}$/i

  matching = symbols.select { |x| x.gsub(/_/, "") =~ regexp  }
  matching = matching.sort_by { |x| -x.length  }

  if correction = correction(english) then
    matching.unshift(correction)
  end

  matching
end



if ARGV.length < 2 or (ARGV[0] =~ /^\?/ and ARGV[1] =~ /\?/) then
  $stderr.puts "usage: ??       <English>+"
  $stderr.puts "usage: ?<0..>   <English>+"
  $stderr.puts "usage: <symbol> ?"
  $stderr.puts "usage: <symbol> <English>+"
  $stderr.puts
  $stderr.puts "  '=' for English uses the previous value for English"
  exit 1
end

symbol  = ARGV[0]
english = ARGV[1..-1].join(" ")


if english =~ /^\?/ then
  print invert(symbol)
  exit 0
elsif english == "=" then
  english = File.read("English")
end
File.open("English", "w") { |x| x.write(english) }

if symbol =~ /^\?(\?|\d+)/ then
  matches = match(english)
  if $1 == '?' then
    puts matches
  else
    symbol = matches[$1.to_i]
    (print "UNKNOWN"; exit 1) unless symbol

    File.open("history", "a") { |x| x.write("#{symbol},#{english}\n") }
    print symbol
  end
else
  File.open("history", "a") { |x| x.write("#{symbol},#{english}\n") }
end
