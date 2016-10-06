### 
### 
### 

def lookup_symbol(spoken)
  load_symbols() unless $symbols

  symbol = $exceptions[spoken.downcase]
  return symbol if symbol

  words   = spoken.scan(/_|[[:alnum:]]+/)
  pattern = words.collect { |x| x[0] + x[1..-1].gsub(/./, "\\&?") }.join("_*")
  regexp  = /^_*#{pattern}_*$/i

  matching = $symbols.select  { |x| x =~ regexp  }
  matching = matching.sort_by { |x| -symbol_weight(x) }
  #puts matching.inspect

  matching[0]
end

def symbol_weight(symbol)
  # slightly penalize leading underscores, count non-leading
  # underscores less than letters:
  symbol.count("^_") - symbol[/^_*/].length*0.02 + symbol[/[^_].*/].count("_")*0.01
end

$symbols    = nil
$exceptions = {}
def load_symbols
  $symbols = File.read("symbols.txt").split(/\n/)
  File.read("overrides.txt").scan(/^([^,]+),\s*(.*)$/) do |symbol, spoken|
    $exceptions[spoken.downcase] = symbol
    $symbols.push(symbol)
  end
end










def gsub_symbols(text)
  starter        = /\b(?:symbol|[Cc]amel|CAMEL|tiger|upper|[Mm]onster)/
  word           = %r_(?:I/O\b|B-tree\b|[\w[[:alnum:]]]+)_
  stop_exclusive = /(?:if|then|end|while|else|elsif|#{starter})\b/
  stop_inclusive = /stop\b/

  inner_word = /(?!#{stop_exclusive}) (?!#{stop_inclusive}) #{word}/x
  payload    = /#{word}(?: +#{inner_word})*/
  text.gsub(/(#{starter})( +|(?=_))(#{payload})(?: +#{stop_inclusive})?/) do
    yield $1, $3
  end
end


def convert_symbol(starter, payload)
  case starter
  when /camel/       # firstSecondThird
    return ("x"+payload).split(/\s+/).collect { |x| cap_first(x) }.join("")[1..-1]

  when /Camel|tiger/ # FirstSecondThird
    return payload.split(/\s+/).collect { |x| cap_first(x) }.join("")

  when /CAMEL|upper/ # FIRSTSECONDTHIRD
    return payload.split(/\s+/).join("").upcase()
    
  when /symbol|monster/i
    result = lookup_symbol(payload)
    #result = nil  # <<<>>>

    #default = payload.split(/\s+/).join("_")    # first_second_third

    # firstSecondThird:
    default = ("x"+payload).split(/\s+/).collect {|x|cap_first(x) }.join("")[1..-1]

    result = default unless result
    return result

  else
    return "<#{starter}=>#{payload.upcase}>"
  end
end

def cap_first(string)
  string[0].capitalize() + string[1..-1]
end



text   = $stdin.read()
result = gsub_symbols(text) do |starter, payload|
  #puts payload + " -> " + convert_symbol(starter, payload)
  convert_symbol(starter, payload)
end

print result
