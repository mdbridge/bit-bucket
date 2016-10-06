###
### Load_words: <<<>>>
### 

#
# Import NatLink:
# 
import sys
sys.path.append("c:/Program Files/NatLink/macrosystem/core")

import natlink 


from word_format     import parse_word_definition, unparse_word
from word_properties import apply_properties, USER_ADDED

from delete_words    import delete_word



##
## Main loading routines:
## 

def load_words(input):
    natlink.natConnect(0)
    try:
        for line in input.readlines():
            fields = parse_word_definition(line)
            if fields == None:
                continue
            written, spoken, properties, pronunciations = fields
            
            load_word(written, spoken, properties)
    finally:
        natlink.natDisconnect()

def load_word(written, spoken, properties):
        name = written
        if spoken != "":
            name += "\\" + spoken
            
        if len(properties)==1 and properties[0]=="-":
            # special case: properties of "-" mean delete word
            delete_word(unparse_word(written, spoken))
        else:
            set_word(name, properties)
    
        

##
## Loading subroutines:
##

def set_word(name, properties):
    make_active_word(name, apply_properties(properties, 0))
    set_word_properties(name, properties)

    
  # throws InvalidWord
def make_active_word(name, default_word_flags):
    flags = natlink.getWordInfo(name, 0)
    if flags != None:
        return                             # already an active word

    flags = natlink.getWordInfo(name, 1)
    if flags != None:
        default_word_flags = flags         # currently in backup dictionary
        print "promoting '%s' from backup to active dictionary" % name
    else:
        default_word_flags |= USER_ADDED   # new word, added by user
        print "adding new user word '%s'" % name

    if natlink.addWord(name, default_word_flags) == 1:
        return

    currentProns = natlink.getWordProns(name)
    natlink.addWord(name, default_word_flags, currentProns[0])
    

  # requires: name is already an active word
def set_word_properties(name, properties):
    old_flags = natlink.getWordInfo(name, 0)
    if old_flags == None:
        raise natlink.UnknownName
    
    flags = apply_properties(properties, old_flags)
    if flags == old_flags:
        return

    print "changing properties of '%s' to 0x%x" % (name, flags)
    print "  from 0x%x" % old_flags
    natlink.setWordInfo(name, flags)

    if natlink.getWordInfo(name, 0) != flags:
        print "FAILED!  properties remain 0x%x" % natlink.getWordInfo(name, 0)


##
## Main routine:
## 
if __name__ == '__main__':
    arguments = sys.argv[1:]
    if len(arguments) != 1:
        print "load_words.py: usage: load_words.py <filename>"
        print "    use - for <filename> to denote standard input"
        sys.exit(1)
    
    if arguments[0] == "-":
        input = sys.stdin
    else:
        input = open(arguments[0])

    load_words(input)
    sys.exit(0)
