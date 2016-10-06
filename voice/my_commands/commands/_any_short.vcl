### 
### Global level short commands
### 
###     Implementation of global_short.vch via standard Windows keys.
### May not work in all applications.
### 
### Do NOT add commands here; add them instead to global_short.vch.
### 

include string.vch;


#
# Moving/selecting/killing by words:
#
# Warning for Emacs users:
#
#       Window's notion of "words" is werid (e.g, "." by itself is a
#   word) and unlike Emacs, move to end of word is not available.  We
#   substitute instead move to next start of word.  Thus, for example,
#   "jump" acts like "start-word" at global level.  Different
#   applications appear to differ on what they define as a word as
#   well.
#

MoveLeftStart()    := "ctrl+left,";
MoveRightStart()   := "ctrl+right,";
MoveRightEnd()     := "ctrl+right,";  # define as above since not available

SelectLeftStart()  := "ctrl+shift+left,";
SelectRightStart() := "ctrl+shift+right,";
SelectRightEnd()   := "ctrl+shift+right,";         # as above

KillLeftStart()    := "ctrl+shift+left,{ctrl+x}";
KillRightStart()   := "ctrl+shift+right,{ctrl+x}";
KillRightEnd()     := "ctrl+shift+right,{ctrl+x}"; # as above

WordMove(action, count) := { Split($action, ",", 0) _ $count }
		             Split($action, ",", 1);

# 
# Cutting/copying and pasting:
# 

Copy()  := {ctrl+c};
Cut()   := {ctrl+x};
Paste() := {ctrl+v};

# 
# Changing word capitalization:
# 

LowerWord() := l;
UpperWord() := U;
CapWord()   := C;  

include "control.vch";
include "extended_string.vch";

Alter(text, kind) := CASE7( $kind, l,Lower($text), U,Upper($text), 
                                   C,Capitalize($text) );

ChangeWord(action, count) := 
    WordMove(SelectRightEnd(), $count)              Copy()
    Clipboard.Set( Alter(Clipboard.Get(),$action) ) Paste();

# 
# Copying/killing to the extreme of a compass direction:
# 
#   rest isn't quite right but is safe except at end of line.
#   Copy* may move cursor with some applications or if selection is empty.
#   Warning: do not use these with an active selection.
#

CopyStart() := {shift+home}            {ctrl+c} {right};
CopyRest()  := {shift+end}{shift+left} {ctrl+c} {left};

KillStart() := {shift+home}            {ctrl+x};
KillRest()  := {shift+end}{shift+left} {ctrl+x};



include "global_short.vch";
