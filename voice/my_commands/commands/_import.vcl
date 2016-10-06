### 
### Voice commands for moving files/directories to the local PC
### 

include "locale_PC.vch";
include "import.vch";


## 
## Importing Vocola commands:
## 

<import> := ( import="" | force import="-force" | type import="-type" );

<import> commands =
    Rsync($1, @~/voice/my_commands/AutoHotkey/*.ahk, ~/AutoHotkey)
    Rsync($1, @~/voice/my_commands/to_Vocola/,       ~/NatLink/Vocola);

please completely replace commands =
    SyncRsync(-replace-dir, @~/voice/my_commands/to_Vocola/, ~/NatLink/Vocola);

<import> extensions = Rsync($1, @~/voice/my_commands/extensions/*.py, 
                                C:\NatLink\NatLink\Vocola\extensions);

import Vocola 3 commands = 
    SyncRsync(-replace-dir, @foil:~/voice/to_Vocola_3/output_commands/,
                            ~/Vocola3/Commands);


## 
## Importing elisp:
## 

pull elisp = Rsync("", @~/.emacs, ~)
     	     Rsync("", @~/elisp/*.el,  ~/elisp)
     	     Rsync("", @~/elisp/*.elc, ~/elisp);


## 
## Importing vocabulary (normal words only):
## 

LoadVocabulary(PC_pathname) :=
     HeardWord(switch, to, DragonBar) WaitForWindow("DragonBar - Professional*")
     v i WaitForWindow("Import list of words or phrases")  # DNS 12
     {alt+p} n
     {alt+a} WaitForWindow("Add File")
     Wait(3000) $PC_pathname {enter}
     Wait(3000) {alt+n}
     ;

#<source> := ( normal = UNIX(~/voice/my_vocabulary/normal_words_list.txt)
<source> := ( normal = 
                SyncRsync(-force, @~/voice/my_vocabulary/normal_words_list.txt,
                                  ~/scratch/)
                PC(~/scratch/normal_words_list.txt)        
            | setup  = PC(~/setup/voice/normal_words_list.txt)
            );

# exit code 23 means can't find source file
load <source> vocabulary = LoadVocabulary($1);


## 
## Importing accelerator files:
## 

import accelerators = Rsync("", @foil:~/Tmp/home-accelerators.html, ~/scratch/)
     	              Rsync("", @work:~/Tmp/work-accelerators.html, ~/scratch/);
import home accelerators = 
                      Rsync("", @foil:~/Tmp/home-accelerators.html, ~/scratch/);


## 
## Importing login batch files:
## 

load login files = SyncRsync("", @~/backups/login_*.bat, ~/);


## 
## Importing Freesr language definition:
## 

<source2> := ( normal = @~/voice/my_commands/Freesr
             | setup  =  ~/setup/voice
             );

load <source2> Free SR = 
    SyncRsync(-force, $1/Mark.lang, 
              "~\Freesr\Voice Commands\Languages\User Defined");


## 
## Make a local copy of the Elephant Store client:
## 
##   (duplicate of a command in old _elephant.vcl)
## 

#copy client to my computer =
#    SyncRsync(-force, @work:~/deduplication/client/Windows/client.exe, c:\);



##
## Importing and exporting trade area:  <<<>>>
##

clear trade area = "rm -rf ~/Tmp/trade_area/*";

<location> := ( work | foil );

import [<location>] trade area = 
    SyncRsync("-force -replace-dir", @ When($1,"$1:","") ~/Tmp/trade_area, 
				     ~/scratch)
    AppBringUp("trade area", PCfromPC(~/scratch/trade_area))
    Wait(2000)
    {ctrl+r}{ctrl+g};


upload to <location> = 
    Rsync("-force -replace-dir", ~/scratch/export-area/, @$1:~/Tmp/import-area)
    ;



## 
## Backing up foil:
## 

include "letters.vch";

  # from a Cygwin window/xterm:
backup foil <letter> = 
    "rsync --progress -t -z -p -r mdl@192.168.1.2:~/../backups/now.tar $1.tar";


## 
## Experimental <<<>>>
## 

import webpages = Rsync("-force -replace-dir", @~/http/pages, ~/scratch);
