### 
### Voice commands for moving files/directories to the local PC
### 

include "locale_PC.vch";
#include "import.vch";
include "import_2.vch";



## 
## Importing Vocola commands:
## 

      import commands =
	ImportInc(~/voice/my_commands/AutoHotkey, ~/AutoHotkey,     *.ahk)
	ImportInc(~/voice/my_commands/to_Vocola,  ~/NatLink/Vocola, *);

force import commands =
	ImportAll(~/voice/my_commands/AutoHotkey, ~/AutoHotkey)
	ImportAll(~/voice/my_commands/to_Vocola,  ~/NatLink/Vocola);

  # for figuring out why import is failing:
type  import commands =
    TypeImportInc(~/voice/my_commands/to_Vocola,  ~/NatLink/Vocola, *);


      import extensions =
	ImportInc(~/voice/my_commands/extensions, 
	          C:\NatLink\NatLink\Vocola\extensions, *.py);



## 
## Importing elisp:
## 

pull elisp = ImportInc(~, ~, .emacs)
     	       # need .elc's so force recompile works...
             ImportInc (~/elisp,  ~/elisp, *.elc)
             ImportInc (~/elisp,  ~/elisp, *.el);



## 
## Importing vocabulary (normal words only):
## 

LoadVocabulary(PC_pathname) :=
     HeardWord(switch, to, DragonBar) WaitForWindow("DragonBar - Professional")
     v i WaitForWindow("Add Words from Word Lists")
     {alt+p} n
     {alt+a} WaitForWindow("Add File")
     Wait(3000) $PC_pathname {enter}
     Wait(3000) {alt+n}
     ;

<source> := ( normal = UNIX(~/voice/my_vocabulary/normal_words_list.txt)
            | setup  = PC(~/setup/voice/normal_words_list.txt)
            );

load <source> vocabulary = LoadVocabulary($1);



## 
## Importing Freesr language definition:
## 

<source2> := ( normal = UNIX(~/voice/my_commands/Freesr)
             | setup  = PC(~/setup/voice)
             );

load <source2> Free SR = 
    CopyPC($1\Mark.lang, PC("~\Freesr\Voice Commands\Languages\User Defined"));




## 
## Make a local copy of the Elephant Store client:
## 
##   (duplicate of a command in old _elephant.vcl)
## 

copy client to my computer =
        ImportInc(~/deduplication/client/Windows, c:\, client.exe);



## 
## Experimental: backing up foil <<<>>>
## 

include "letters.vch";

  # from a Cygwin window:
backup foil <letter> = "scp mdl@192.168.1.2:~/../backups/now.tar $1.tar";


import webpages = ImportAll(~/http/pages, ~/scratch/pages);


## 
## Experimental
## 

import Vocola 3 commands =
    ShellExecute(
	RsyncDown(~/voice/to_Vocola_3/output_commands,
                  PCfromPC(~/Vocola3/Commands), "", "-replace"));
