###
### Delete_words: remove given list of DNS words from active dictionary
### 
###   (may remain in backup dictionary)
### 

#
# Import NatLink:
# 
import sys
sys.path.append("c:/NatLink/NatLink/macrosystem/core")
import natlink 


from dump_words import find_word



def delete_words(input):
    natlink.natConnect(0)
    try:
        for line in input.readlines():
            delete_word(line.rstrip())
    finally:
        natlink.natDisconnect()


def delete_word(word):
    try:
        natlink.deleteWord(word)
        result = find_word(word)
        if result == 2:
            print >> sys.stderr, "Error: deletion of word '%s' failed" % word
        elif result == 1:
            print "demoting word '%s' to backup dictionary" % word
        else:
            print "deleting word '%s'" % word
    except natlink.UnknownName:
        pass
    except natlink.InvalidWord:
        print >> sys.stderr, "Error: '%s' is an invalid word" % name


#
# Main routine:
# 
if __name__ == '__main__':
    arguments = sys.argv[1:]
    if len(arguments) != 1:
        print "delete_words.py: usage: delete_words.py <filename>"
        print "    use - for <filename> to denote standard input"
        sys.exit(1)
    
    if arguments[0] == "-":
        input = sys.stdin
    else:
        input = open(arguments[0])

    delete_words(input)
    sys.exit(0)
