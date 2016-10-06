###
### Dump_words: take a list of DNS words and look them up under the current
###             dictionaries then write out word definitions for each word
###             found.
### 
### Pronunciations are omitted unless -p is used.
### 
###     Only outputs "standard" properties; e.g., not internal properties or
### properties indicating how the word was added.  However, one or more uses
### of -v can be used to display nonstandard properties via outputted comments.
### 


#
# Import NatLink:
# 
import sys
sys.path.append("c:/NatLink/NatLink/macrosystem/core")
import natlink 


from word_format     import parse_word, unparse_word_definition

from word_properties import unpack_flags, unpack_properties
from word_properties import addition_properties, internal_properties, \
                            other_properties


##
## Displaying word properties:
## 

def dump_properties(flags, verbose_level):
    if verbose_level > 2:
        dump_flags(flags & addition_properties)
    if verbose_level > 1:
        dump_flags(flags & internal_properties)
    if verbose_level > 0:
        dump_flags(flags & other_properties)

def dump_flags(flags):
    annotated_flags = unpack_flags(flags)
    if len(annotated_flags) == 0:
        return
    
    print "  #"
    for flag in annotated_flags:
        print "  # 0x%08x - %s" % (flag[0], flag[1])



##
## Dumping word(s):
## 

def dump_words(input, include_pronunciations, verbose_level):
    natlink.natConnect(0)
    try:
        for line in input.readlines():
            dump_word(line.rstrip(), include_pronunciations, verbose_level)
    finally:
        natlink.natDisconnect()

def dump_word(name, include_pronunciations, verbose_level):
    try:
        flags = natlink.getWordInfo(name, 3)
        pronunciations = natlink.getWordProns(name)
    except InvalidWord:
        print >> sys.stderr, "Error: '%s' is an invalid word" % name
        return
    if flags == None:
        print >> sys.stderr, "Error: word '%s' not found" % name
        return

    if not include_pronunciations or pronunciations==None:
        pronunciations = []


    dump_properties(flags, verbose_level)

    if verbose_level > 2:
        if find_word(name) == 1:
            print "  #"
            print "  # (in backup dictionary only)"


    written, spoken = parse_word(name)
    properties      = unpack_properties(flags & other_properties)

    print unparse_word_definition(written, spoken, properties, pronunciations)

#
# Lookup word word in NatLink's dictionaries; returns:
#
#  -1: if word is an invalid word
#   0: if word is in neither dictionary
#   1: if word is present only in the backup dictionary
#   2: if word is present in the active directory
# 
def find_word(word):
    try:
        flags = natlink.getWordInfo(word, 2)
        if flags != None:
            return 2

        flags = natlink.getWordInfo(word, 3)
        if flags != None:
            return 1

        return 0
    except InvalidWord:
        return -1



##
## Main routine:
## 

if __name__ == '__main__':
    verbose_level = 0
    include_pronunciations = False

    arguments = sys.argv[1:]
    while len(arguments)>0 and arguments[0] == "-v":
        verbose_level += 1
        arguments = arguments[1:]
    if len(arguments)>0 and arguments[0] == "-p":
        include_pronunciations = True
        arguments = arguments[1:]

    if len(arguments) != 1:
        print "dump_words.py: usage: dump_words.py [-v]* [-p] <filename>"
        print "    use - for <filename> to denote standard input"
        sys.exit(1)

    if arguments[0] == "-":
        input = sys.stdin
    else:
        input = open(arguments[0])

    dump_words(input, include_pronunciations, verbose_level)
    sys.exit(0)
