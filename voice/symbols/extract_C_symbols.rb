code = ARGF.read()

code.gsub!(%r< \'(\\.|[^\\\'])*\' | \"(\\.|[^\\\"])*\" | //[^\n]*\n 
             | /\* .*? \*/ >mx, " ")

# deal with 0.e13[fFlL] if needed later

symbols = code.scan(/\w+/).reject { |x| x =~ /^\d/ || x.length==1 }

puts symbols.sort_by { |x| x.downcase }.uniq
