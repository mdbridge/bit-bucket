### 
### Voice commands for easily typing common key chords
### 

include "keys.vch";
include "letters.vch";
include "numbers.vch";
include "string.vch";



## 
## Pressing just modifier key:
## 

# press Windows

press (shift|control=ctrl|alt) = {$1};



## 
## Shorthand (avoids "press") for single base key with modifiers:
## 

# DNS 12: "press Windows [Alpha]" works even with elevated applications


big <letter>  = Upper($1);

control <prn> = Key(ctrl+,$1,"");


  # these don't seem to get used enough; alt ... is misrecognized: <<<>>>
#        <modifiers>  <prn> = {$1$2};
#	 <modifiers>  <non> = {$1$2};
#
#  # need to use SendSystemKeys so Windows sees the Windows key:
#Windows [<modifiers>] <prn> = SendSystemKeys({win+$1$2});
#Windows [<modifiers>] <non> = SendSystemKeys({win+$1$2});



## 
## Shorthand for common multiple key chords:
## 

escape <prn> = {esc} PrintablesToKeys($1);

control Charlie control Charlie = {ctrl+c}{ctrl+c};
double control Charlie		= {ctrl+c}{ctrl+c};  # <<<>>>


  # for selecting from menus:
under <letter> [<letter>] = {alt+$1} $2;

key <prn> <my0to99> = Repeat($2, PrintablesToKeys($1));  # {}_5} fails
