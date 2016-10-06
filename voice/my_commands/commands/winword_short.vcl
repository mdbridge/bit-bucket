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

  # make move/edit by words work with selected dialog boxes:
IfMain(main, fallback) := 
    If(Window.Match("Open"),    $fallback,
    If(Window.Match("Save As"), $fallback,
    $main));

Begin() := IfMain(ctrl+shift+b,ctrl+shift+left);  # previous word start
End()   := IfMain(ctrl+shift+e,ctrl+shift+right); # next     word end
Start() := IfMain(ctrl+shift+s,ctrl+shift+right); # next     word start

MoveLeftStart()    := Begin() "," IfMain({left},{right}{left});
MoveRightEnd()     := End()   "," IfMain({right},{left}{right});
MoveRightStart()   := Start() "," IfMain({right},{left}{right});

SelectLeftStart()  := Begin() ",";
SelectRightEnd()   := End()   ",";
SelectRightStart() := Start() ",";

KillLeftStart()    := Begin() ",{ctrl+x}";
KillRightEnd()     := End()   ",{ctrl+x}";
KillRightStart()   := Start() ",{ctrl+x}";

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
