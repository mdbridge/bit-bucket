###
###
### 

import sys

from word_format import parse_fields, unparse_word_definition


##
## Handling overridden symbols:
##

overridden_map = {}

def make_key(written, spoken):
    return "%s\n%s" % (written, spoken.rstrip())


def overridden(written, spoken):
    return overridden_map.has_key(make_key(written, spoken))

def override(written, spoken):
    overridden_map[make_key(written, spoken)] = True

    
def process_definition(symbol, name, properties):
    if overridden(symbol, name):
        print "#" + unparse_word_definition(symbol, name, properties) + \
              "   # OVERRIDDEN"
    else:
        print unparse_word_definition(symbol, name, properties)
    

##
##
## 

def process_symbol(left_symbol, name, right_symbol):
    print
    if right_symbol != "":
        # We have a "bracket":
        process_definition(left_symbol,  "open %s"  % name, "p")
        process_definition(right_symbol, "close %s" % name, "s")

        process_definition(left_symbol,  "left %s"  % name, ".")
        process_definition(right_symbol, "right %s" % name, ".")

        if left_symbol != right_symbol:
            return 
    
    if len(left_symbol) > 1:
        process_definition(left_symbol, "%s" % name,       "")
        process_definition(left_symbol, "tight %s" % name, ".")
        
    else:
        process_definition(left_symbol, "%s op" % name,    "")
        process_definition(left_symbol, "tight %s" % name, ".")

    if name.find(" ") == -1:
        process_definition(name, "written " + name, "")


##
## Main routine:
## 

override_section = True

for line in sys.stdin.readlines():
    line = line.rstrip()
    if override_section:
        print line
        if line == "##### SYMBOLS":
            override_section = False
        fields = parse_fields(line, 2)
        if fields != None:
            override(fields[0], fields[1])
        continue

    fields = parse_fields(line)
    if fields == None:
        continue
    
    while len(fields)<3:
        fields += [""]

    process_symbol(fields[0], fields[1], fields[2])
