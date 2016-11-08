### 
### Voice commands for Windows version of gnuemacs
### 

include "locale_PC.vch";
include "gnu.vch";


## 
## Avoiding misrecognitions:
## 

close = Beep();



## 
## Commands that work only with local Emacs:
## 

start Python [(2.5=25|2.6=26|2.7=27)] shell = 
    {ctrl+u} Do(run-python) EraseToStart() 
    "c:\python" When($1,$1,27) \python {enter};


buffer Vocola history = FindFile(PC(~/NatLink/Vocola_history.txt)){enter};



## 
## Experiment: VR mode
## 

start VR mode = Elisp('(load "vr-mode")')
                Do(vr-mode)
		{ctrl+c}vB;

voice activate buffer = {ctrl+c}vB;
