### 
### Attempt to separate out commands into their constituent parts/kinds
### 


## 
## Patterns:
## 

# 
# Numbers:
# 
irregular = %W[zero one two three four five six seven eight nine ten
               eleven twelve thirteen fourteen fifteen sixteen seventeen
               eighteen nineteen]

tens = %W[twenty thirty forty fifty sixty seventy eighty ninety]

number_word = /(#{tens.join("|")}|#{irregular.join("|")}|[0-9]+)/i
Number      = /(#{number_word}([- \t]#{number_word})*)/


# 
# Letters:
# 
Letter = /
             |Alpha
             |Bravo
             |Charlie
             |Delta
             |echo
             |foxtrot
             |golf
             |Hotel
             |India
             |Juliett
             |kilo
             |Lima
             |Mike
             |November
             |Oscar
             |Papa
             |Quebec
             |Romeo
             |Sierra
             |tango
             |uniform
             |Victor
             |whiskey
             |x-ray
             |Yankee
             |Zulu
             /x

# 
# Keys (1 <prn>):
# 
Key        = /
             | [-!\"\#\%&\'\(\)*+,.\/;<=>?@\[\]^_\`{|}~:] | \$
             | [0-9] | \#\#
             | #{Letter}

             |space
             |bang
             |open\ quote
             |quote
             |pound
             |dollar
             |percent
             |apostrophe
             |single\ quote
             |paren
             |close\ paren
             |asterisk
             |star
             |plus
             |minus
             |dot
             |semi
             |bend
             |equal
             |equals
             |close\ bend
             |question
             |bracket
             |backslash
             |close\ bracket
             |brace
             |vertical\ bar
             |bar
             |close\ brace
             /x

# 
# Leap commands:
# 
Leap       = /(?:leap|retreat|erase\ until|back\ until)(?: after)?(?:\t(?:second|third|fourth))?/



# 
# *most* short commands:
# 
repeatable = /soar|down|left|right|back|erase|tab|space|enter
             |jump|skip|flee|kill|zap|zortch|toast|start-word|pull-word
             |(?:copy|fetch)[\ \t](?:word|password|identifier|template)
             |lower-a-word|cap-a-word|upper-a-word/x

Short      = /first|last|yank|pull[ ]print|escape
             |(?:copy|destroy|fetch)[\ \t](?:start|rest|region)
             |#{repeatable}\t\#\#|#{repeatable}
             |<row>\t\#\#
             |(?:advance|fallback|dictate)\t[^\t]+
             |key\t<prn>|#{Leap}\t<prn>/x


## 
## Do the actual folding:
## 

  # reading commands, convert to UNIX line-ending format:
text = $stdin.read().gsub(/\r/, "")
$stderr.print "."

  # remove obvious dictation:
text.gsub!(/^.*\\.*\n/, "")
$stderr.print "."


  # add tabs at front and end of each line to make later pattern matching simpler:
text = "\t" + text
text.gsub!(/\n/, "\t\n\t")
text = text[0..-2]
$stderr.print "."


  # fold numbers, even inside a DNS 'word':
text.gsub!(/[ \t](#{Number})[ \t]/) { |x| x[0..0]+"##"+x[-1..-1] }
$stderr.print "."

  # fold <row>:
text.gsub!(/[ \t](row|line|go)\t##[ \t]/) { |x| x[0..0]+"<row>\t##"+x[-1..-1] }
$stderr.print "."


# fold <prn> when (mostly) safe:
  # key <prn>:
text.gsub!(/\tkey\t#{Key}\t/, "\tkey\t<prn>\t")
  # <leap> <prn>
text.gsub!(/\t#{Leap}\t#{Key}\t/) do |x| 
  x.sub(/\t#{Key}\t/, "\t<prn>\t")
end
$stderr.print "."


  # fold some buffer commands when (mostly) safe:
text.gsub!(/^\t#{Letter}\tbuffer\t$/, "\t<Letter>\tbuffer\t")
text.gsub!(/^\t#{Letter}\t#{Letter}\tbuffer\t$/, "\t<letter>\t<Letter>\tbuffer\t")
text.gsub!(/^\tletter\t#{Letter}\tbuffer\t$/, "\tletter\t<Letter>\tbuffer\t")
$stderr.print "."
text.gsub!(/^\tnot that\t#{Letter}\tbuffer\t$/, 
           "\tnot that\t<letter>\tbuffer\t")
$stderr.print "."
text.gsub!(/^\tbuffer\t[^\t]+\tcommands\t$/, "\tbuffer\t...\tcommands\t")
text.gsub!(/^\tbuffer\t[^\t]+\tcommands on PC\t$/, 
           "\tbuffer\t...\tcommands on PC\t")
$stderr.print "."


  # Move to a directory:
text.gsub!(/^\tchange directory\t.*/, "\tchange directory\t...\t")
text.gsub!(/^\tmove to\t.*/, "\tmove to\t...\t")
$stderr.print "."
text.gsub!(/^\tfolder\t.*/, "\tfolder\t...\t")
$stderr.print "."

  # fold Web/Work <web_site> when (mostly) safe:
text.gsub!(/^\tWeb\t[^\t\\]+\t\n/, "\tWeb\t<web_site>\t\n")
text.gsub!(/^\tWork\t[^\t\\]+\t\n/, "\tWork\t<work_site>\t\n")
$stderr.print "."


  # fold DNS's correct <_anything>:
text.gsub!(/^\tcorrect\t([^\n]+)\t\n/) do |x|
  if $1 != "that" then
    "\tcorrect\t<_anything>\t\n"
  else
    x
  end
end
$stderr.print "."


  # fold file <folder> when (mostly) safe:
text.gsub!(/(^\t(?:prefix\t##\t)?file\t)[^\t\\]+\t\n/) { "#$1<folder>\t\n" }
text.gsub!(/(^\t(?:prefix\t##\t)?file\t)[^\t\\]+\tand\t[^\t\\]+\t\n/) {
  "#$1<folder>\tand\t<folder>\t\n" }
$stderr.print "."


$stderr.puts

  # split apart sequences of short commands:
count = 0
text.gsub!(/^\t(#{Short})\t((?:#{Short}\t)+)\n/) do |commands|
  result = ""
  #result = "ORIGINAL: #{commands}"
  #result += "SHORT:\t#$1\t\n"
  result += "\t#$1\t\n"
  commands = "\t" + $2 + "\n"
  while commands =~ /^\t(#{Short})\t((?:#{Short}\t)*)\n/ do
    #result += "SHORT:\t#$1\t\n"
    result += "\t#$1\t\n"
    commands = "\t" + $2 + "\n"
  end
  count = count + 1
  $stderr.print "." if count%1000 == 0
  result
end
$stderr.puts "!"

  # fold target of advance/fallback/dictate:
text.gsub!(/^\t(advance|fallback|dictate)\t[^\t\n]*\t\n/) {"#$1\t<_anything>\t\n"}
$stderr.print "."


  # check for overlooked short command sequences:
#count = 0
#text.gsub!(/^.*\t#{Short}\t.*\n/) do |commands|
#  count = count + 1
#  $stderr.print "." if count%1000 == 0
#  if commands =~ /^\t#{Short}\t\n/ then
#    "SHORT:#{commands}"
#  else
#    "WARNING:#{commands}"
#  end
#end

$stderr.puts


#puts text.gsub(/\t/, "|")
#$stderr.puts text[0 .. 10000].gsub(/\t/, "|")
#fail

  # remove added tabs at front and end of each line:
puts text.gsub(/\t\n\t/, "\n")[1..-3]+"\n"
