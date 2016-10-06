###
### prepare_load_properties: <<<>>>
### 

import sys

from word_format     import parse_word_definition


##
## Macro functions for setting word properties:
## 

def output_header():
    print """###
### This file automatically generated via prepare_load_properties.py
### 

Word(written, spoken) := WaitForWindow("Vocabulary Editor") Wait(50)
                         {alt+w} $written Wait(50) {tab} $spoken {tab}{space};

Properties() := Wait(50) {alt+p} WaitForWindow("Word Properties");

SetSpacing(written, spoken, before, after) :=
    Word($written, $spoken) Properties()
    {alt+n}$before {alt+d}$after
    {enter};


Suffix(written, spoken) := Word($written, $spoken) Properties() {alt+n}0 {enter};

Prefix(written, spoken) := Word($written, $spoken) Properties() {alt+d}0 {enter};

Tight(written, spoken) := Word($written, $spoken) Properties() {alt+n}0 {alt+d}0 {enter};



load word properties ="""



##
## Extracting word properties:
## 

def prepare_words(input):
    for line in input.readlines():
        fields = parse_word_definition(line)
        if fields == None:
            continue
        written, spoken, properties, pronunciations = fields

        prepare_word(written, spoken, properties)


def prepare_word(written, spoken, properties):
    before = 1
    after  = 1

    for p in properties:
        if p=="-":
            return
        elif p=="." or p=="tight":
            before = after = 0
        elif p=="p" or p=="prefix":
            before = 1
            after  = 0
        elif p =="s" or p=="suffix":
            before = 0
            after  = 1
        else:
            print >> sys.stderr, "Warning: ignoring property '%s'" % (p)

    if before==1 and after==1:
        return
    
    written = written.replace("'", "''")
    print "    SetSpacing('%s', '%s', %d, %d)" % (written,spoken,before,after)

    
        

##
## Main routine:
## 

arguments = sys.argv[1:]
if len(arguments) != 1:
    print "prepare_load_properties.py: usage: prepare_load_properties.py <filename>"
    print "    use - for <filename> to denote standard input"
    sys.exit(1)
    
if arguments[0] == "-":
    input = sys.stdin
else:
    input = open(arguments[0])

output_header()
prepare_words(input)
print "    ;"

sys.exit(0)
