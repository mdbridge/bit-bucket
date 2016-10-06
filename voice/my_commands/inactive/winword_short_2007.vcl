### 
### Microsoft Word level short commands
### 

include "winword.vch";


## 
## Implement global short commands using Word VBA macros so the notion
## of word (movement) corresponds to Emacs's rather than Window's
## broken one.
## 

#
# Moving/selecting/killing by words:
# 
#   (These require emacs_words.bas VBA code has been loaded into Word.)
#

MoveLeftStart()    := "ctrl+shift+b,{left}";   # previous word start
MoveRightEnd()     := "ctrl+shift+e,{right}";  # next     word end
MoveRightStart()   := "ctrl+shift+s,{right}";  # next     word start

SelectLeftStart()  := "ctrl+shift+b,";
SelectRightEnd()   := "ctrl+shift+e,";
SelectRightStart() := "ctrl+shift+s,";

KillLeftStart()    := "ctrl+shift+b,{ctrl+x}";
KillRightEnd()     := "ctrl+shift+e,{ctrl+x}";
KillRightStart()   := "ctrl+shift+s,{ctrl+x}";

WordMove(action, count) := { Split($action, ",", 0) _ $count }
		             Split($action, ",", 1);

# 
# cutting/copying and pasting:
# 

Copy()  := {ctrl+c};
Cut()   := {ctrl+x};
Paste() := {ctrl+v};

# 
# Changing word capitalization:
# 

LowerWord() := l;
UpperWord() := u;
  # CapWord() in Word only capitalizes the actual start of a word; to
  # capitalize a letter in the middle of a word, use cap-a-letter
  # instead.
CapWord()   := c;  

ChangeWord(action, count) := WordMove(SelectRightEnd(), $count)
		   	     {alt+h}7 $action {right};

# 
# Copying/killing to the extreme of a compass direction:
# 
#   rest isn't quite right but is safe except at end of line.
#   Copy* may move cursor if selection is empty.
#   warning: do not use these with an active selection.
#

CopyStart() := {shift+home}            {ctrl+c} {right};
CopyRest()  := {shift+end}{shift+left} {ctrl+c} {left};

KillStart() := {shift+home}            {ctrl+x};
KillRest()  := {shift+end}{shift+left} {ctrl+x};


include "global_short.vch";



##
## Jump to location on current page whose line number on that page is equal
## to N:
##

<row> <r> = Line($2);



## 
## Leap commands:
## 

include "leap4_short.vch";
