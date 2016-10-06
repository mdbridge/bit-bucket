###
### Voice commands for Naturally Speaking's DragonPad
###

DragonPad:


##
## Mark Lillibridge's leap3 commands:
##

include "leap3_dragonpad.vch";

include "leap3_short.vch";          # <<<>>>
include "leap3_long.vch";


## 
## Miscellaneous:
## 

save file = {ctrl+s};



## 
## Typing into a chat/IM window:
## 
##   use "set Alpha point" to set where to click to activate both the chat 
##   window and the input field.
## 

Chat() := Mouse.Click(Variable.Get(a));

chat window = Chat();

transfer = {ctrl+a}{ctrl+c} Chat() {ctrl+v}{enter} HeardWord(DragonPad, window);
