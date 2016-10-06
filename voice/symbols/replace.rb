text = $stdin.read()

keywords = /if|then|end|while|else|elsif/

text.gsub!(/\bsymbol\s+( \b\w+ (\s+ \b(?!symbol|#{keywords})\w+)* )/x) do |words|
  english = $1
  try = `ruby solve.rb '?0' #{english}`
  try != "UNKNOWN" ? try : english.gsub(/\s+/, "_")
end

print text
