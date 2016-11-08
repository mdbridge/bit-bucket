### 
### Voice commands for moving files/directories to/from the local PC
### 

include "locale_PC.vch";
include "import.vch";
include "URLs.vch";
include "machines.vch";


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

<source> := ( normal = MakeLocal(@~/voice/my_vocabulary/normal_words_list.txt) Local()
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
## Importing and exporting via incoming and outgoing directories:
##
## Analogs for between Linux machines can be found in UNIX_shell.vcl
##

PC export [to <machine>] =
    Rsync("-force -replace-dir", ~/scratch/outgoing/,
	    When($1,"@|$1:","@") ~/Tmp/incoming);

PC import [from <machine>] =
    SyncRsync("-force -replace-dir", When($1,"@|$1:","@") ~/Tmp/outgoing/,
                ~/scratch/incoming)
    AppBringUp("incoming", PCfromPC(~/scratch/incoming))
    Wait(2000)
    {ctrl+r}{ctrl+g};



## 
## Backing up foil:
## 

include "letters.vch";

  # from a Cygwin window/xterm:
backup foil <letter> = 
    "rsync --progress -t -z -p -r mdl@192.168.1.2:~/../backups/now.tar $1.tar";
backup HP <letter> = 
    "rsync --progress -t -z -p -r hp@192.168.1.2:~/../HP-backups/now.tar.gz HP-$1.tar.gz";
SFTP backup HP <letter> = 
    "scp hp@foil.strangled.net:~/../HP-backups/now.tar.gz HP-$1.tar.gz";



## 
## Experimental <<<>>>
## 

  # Warning: synchronous and possibly slow
import webpages = MakeLocal(@~/http/pages) Lookup(Local() "/index.html");
