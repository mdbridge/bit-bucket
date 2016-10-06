### 
### Word_format:
### 
###   Functions for converting to and from various formats.
### 
###   When used as a stand-alone utility, converts a file of word definitions 
###   into a list of (DNS format) words.
### 

import sys, re



##
## DNS's word format:
## 
##   <written>[\<spoken>]
## 
## Not a lot is known about the corner cases here; a written form of "\" 
## with a spoken form is okay, though.
## 

  # Warning: the resulting word may not be valid in DNS's eyes
def unparse_word(written, spoken=None):
    if spoken!=None and spoken!="":
        return r'%s\%s' % (written, spoken)
    else:
        return written

  # returns (written, spoken); spoken is "" if missing:
def parse_word(word):
    index = word.rfind("\\")
    if index == -1:
        return (word, "")
    else:
        return (word[0:index], word[index+1:])



## 
## Raw formatting:
## 
##   Convert to and from our Colon-delimited Field Format
## 
##   Each record takes one line, with its fields separated by :'s like so:
## 
##     field1:field2:field3
## 
##   Records may have any non-zero number of fields.
##   Comments start with an unescaped '#' and continue to the end of the line.
##   Whitespace at the end of a line (including comments) is ignored.
##   Thus, " a : b " denotes the record whose fields are " a " and " b".
## 
##   Blank lines (all whitespace) are ignored.  (The record containing one
##   empty field is denoted r'\p'.)
## 
##   Backslashes are used to escape \'s, #'s, and :'s in fields.
##   Supported backslash codes:
## 
##     \n = newline
##     \r = carriage return
##     \t = tab
## 
##     \p = the empty string  (noP)
## 
## 
## TODO: unicode escapes?  other escapes?  <<<>>>  
##       \xnn \unnnn
##       currently only handles non-Unicode strings correctly
## 

def unparse_fields(fields):
    if len(fields) == 0:
        raise "Invalid Colon-Delimited Field Format Record"

    line = ":".join([escape_specials(f) for f in fields])
    if line.rstrip()!=line or line=="":
        line = line + r'\p'

    return line

def escape_specials(string):
    return string \
           .replace("\\", "\\\\") \
           .replace(":", r'\:').replace("#", r'\#') \
           .replace("\n", r'\n').replace("\r", r'\r') \
           .replace("\t", r'\t')


  # Omits unnecessary trailing empty fields:
def unparse_fields_minimum(fields):
    while len(fields)>1 and fields[-1]=="":
        fields = fields[0:-1]

    return unparse_fields(fields)


#print unparse_fields(["a", "b:r", "cc  \t"]),"r"
#print unparse_fields(["a", "b:r  ", "  c\\c", ""])
#print unparse_fields(["#a", "b::r", "c\\:c  ", "", ""])
##print unparse_fields([])
#print unparse_fields(["a\nr\rd\te\n"])
#print unparse_fields([""])
#
#print unparse_fields_minimum(["a", "b:r", "cc  \t"]),"r"
#print unparse_fields_minimum(["a", "b:r  ", "  c\\c", ""])
#print unparse_fields_minimum(["#a", "b::r", "c\\:c  ", "", ""])
##print unparse_fields_minimum([])
#print unparse_fields_minimum([""])


escape_map = { 'n':"\n", 'r':"\r", 'p':"", 't':"\t" }

  # Returns None if no record present in line:
def parse_fields(line, minimum_fields = 0):
    line = strip_trailing_whitespace(line)
    if line=="":
        return None
        
    field = ""
    fields = []

    escaped = 0
    for c in line:
        if c == "\\" and not escaped:
            escaped = 1
            continue

        if c == ":" and not escaped:
            fields.append(field)
            field = ""
            continue
        
        if escaped and escape_map.has_key(c):
            field += escape_map[c]
        else:
            field += c

        escaped = 0
        
    fields.append(field)

    while len(fields) < minimum_fields:
        fields.append("")

    return fields

def strip_trailing_whitespace(line):
    result = ""

    escaped = 0
    for c in line:
        if c == "#" and not escaped:
            break

        result += c

        if c == "\\" and not escaped:
            escaped = 1
        else:
            escaped = 0
               
    return result.rstrip()
    


##
## Our word definition format:
## 
##     <written>:<spoken>:<properties>:<pronunciations>
## 
##   using our underlying colon-delimited format.
## 
##   All but the first field may be omitted.  Leading and trailing whitespace
##   within fields are ignored (e.g., r'a\t:\tb' == "a:b").
## 
##   The last two fields are whitespace-separated lists of strings.
## 
##   Examples:
## 
##     .gif:dot gif     :suffix keep   :dotgif
##     .gif:dot GIF     :suffix keep   :dotjEIef
## 

def unparse_word_definition(written, spoken="", properties=[], \
                            pronunciations=[]):
    fields = [written, spoken,
              " ".join(properties),
              " ".join(pronunciations)]

    return unparse_fields_minimum(fields)


  # Returns None if no word definition present in line:
def parse_word_definition(line):
    fields = parse_fields_stripped(line, 4)
    if fields == None:
        return None

    return (fields[0], fields[1],
            split_by_whitespace(fields[2]),
            split_by_whitespace(fields[3]))

def split_by_whitespace(text):
    if text == "":
        return []

    return re.split('[ \t\n\r]+', text)
    

  # Returns None if no record present in line:
def parse_fields_stripped(line, minimum_fields=0):
    fields = parse_fields(line, minimum_fields)
    if fields == None:
        return None

    return [f.strip() for f in fields]



##
## Main routine: convert a file of word definitions into a list of
##               (DNS format) words.
## 

if __name__ == '__main__':
    arguments = sys.argv[1:]
    if len(arguments) != 1:
        print "word_format.py: usage: word_format.py <filename>"
        print "    use - for <filename> to denote standard input"
        sys.exit(1)

    if arguments[0] == "-":
        input = sys.stdin
    else:
        input = open(arguments[0])

    for line in input.readlines():
        fields = parse_word_definition(line)
        if fields:
            written, spoken, properties, pronunciations = fields
            print unparse_word(written, spoken)
        
    sys.exit(0)
