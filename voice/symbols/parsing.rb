###
### Functions for lexing programming files
###

# allows newlines in quotations
def quotation_pattern(character, allow_backslash_escaping=false)
  character = Regexp.escape(character)
  if allow_backslash_escaping
    %r{ #{character} ( (?: \\. | [^\\#{character}] )* ) #{character} }x
  else
    %r{ #{character} ( [^#{character}]* ) #{character} }x
  end
end

# e.g., "#" or "//" or "%"
def eol_comment_pattern(characters)
  characters = Regexp.escape(characters)
    %r{ ( #{characters} [^\n]* \n ) }x
end

def join(*patterns)
  fail unless patterns.length > 0
  %r{ #{ "(" + patterns.join(') | (') + ")" } }x
end



def generic_strip(text, eol_comment_start=nil, allow_backslash_escaping=false)
  patterns = []
  patterns.push(quotation_pattern("'", allow_backslash_escaping))
  patterns.push(quotation_pattern('"', allow_backslash_escaping))
  if eol_comment_start
    patterns.push(eol_comment_pattern(eol_comment_start))
  end

  pattern = join(*patterns)
  puts pattern

  text.gsub(pattern) do |matched|
    puts matched.inspect
    if matched[0] =~ /[\'\"]/
      matched[0]*2
    else
      matched.gsub(/[^\n]/, "")
    end
#    "FRED"
  end
end

#puts generic_strip(ARGF.read)
puts generic_strip(ARGF.read, "#")
