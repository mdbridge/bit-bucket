## 
## This grammar provides no commands, but instead records all
## recognized dictation/commands to a log file, along with all
## alternatives passed by DNS (i.e., what correct that would provide
## aside from capitalization changes).
## 

import string
import sys
import natlink
from   natlinkutils import *


class CatchAllGrammar(GrammarBase):

    gramSpec = """
        <start> exported = {emptyList};
    """

    def initialize(self):
        self.load(self.gramSpec,allResults=1)
        self.activateAll()

    def gotResultsObject(self,recogType,resObj):
        if recogType == 'reject':
            return

        result = resObj.getWords(0)
        
        file = open(os.getenv("HOME")+'/NatLink/alternatives.txt', 'a')
        file.write('>\t' + string.join(result, '\t')+'\n')
        i = 0
        try:
            while True:
                i = i + 1
                result = resObj.getWords(i)
                file.write('\t' + string.join(result, '\t')+'\n')
        except Exception, e:
            #file.write(repr(e)+'\n')
            pass

        file.close()
        
        #print result


## 
## Starting up and stopping our grammar:
## 
catchAllGrammar = CatchAllGrammar()
catchAllGrammar.initialize()

def unload():
    global catchAllGrammar
    if catchAllGrammar:
        catchAllGrammar.unload()
    catchAllGrammar = None
