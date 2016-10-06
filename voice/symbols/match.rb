symbols = File.read("symbols").split(/\n/)

english = ARGV.join(" ")
words   = english.split(/\s+/)

pattern = "^" + words.collect { |x| x[0] + x[1..-1].gsub(/./, "\\&?") }.join("") +
          "$"

regexp = /#{pattern}/i

matching = symbols.select { |x| x.gsub(/_/, "") =~ regexp  }

matching = matching.sort_by { |x| -x.length  }

puts matching
