## 
## This grammar provides no commands, but instead records all
## recognized dictation/commands to a log file.
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
        
        file = open(os.getenv("HOME")+'/NatLink/Vocola_history.txt', 'a')
        file.write(string.join(result, '\t')+'\n')
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
