### 
### Voice commands for notepad
### 

include "gnu.vch";


## 
## Commands for finishing a mutate command (see gnu_mutate.vcl):
## 

Hide() := {alt+f4}n;

transfer = {ctrl+a} {ctrl+c} Wait(1000) Hide()
           CutToRegister(ScratchRegister()) {ctrl+y};

abort transfer = {ctrl+a} {ctrl+c} {end}** Wait(1000) Hide() {left}{right};
