### 
### Voice commands for changing window state
### 


## 
## Changing what windows are displayed:
## 

# (hide | minimize) all windows

# note: {win+d} also minimizes all windows but doesn't remember which ones

  clear desktop = SendSystemKeys({win+m});  #({win+d});
    # restore windows minimized by above, does not restore focus:
restore desktop = SendSystemKeys({win+shift+m});

minimize rest   = SendSystemKeys({win+home});



##
## Commands for manipulating arbitrary windows, including dialog boxes
##

include "new_windows.vch";

# 
# I disable these for several reasons:
# 
#   * commonly mis-recognized, especially click close!
#   * Chuck says we should use the '<cmd> window' commands instead
# 
click (Restore | Move | Size | Minimize | Maximize | Close) = Beep();


other monitor = SendSystemKeys({win+shift+right});

<state> := ( lengthen		 = {win+shift+up}
	   | left  half 	 = {win+up}{win+left}
	   | right half		 = {win+up}{win+right}
	   );
<state> window = SendSystemKeys($1);



# 
# For most standard Windows applications and many dialog boxes,
# {alt+space} (i.e., left alt key + space; equivalent to
# {LeftAlt+space}) opens the system menu.
#
# See windows.vch for important exceptions...
# 
System(key) := SendSystemKeys({alt+space}) $key;

include "windows.vch";



# 
# Tiling experiments via AutoHotkey hotkeys in kill.ahk
# 
# Hardwired for home for now...
# 
Extend() := SendSystemKeys({win+shift+up}{win+shift+right}{win+shift+left});

left  two thirds = SendSystemKeys("{alt+ctrl+(}") Extend();
right two thirds = SendSystemKeys("{alt+ctrl+)}") Extend();
#left  half       = SendSystemKeys("{alt+ctrl+<}") Extend();
#right half       = SendSystemKeys("{alt+ctrl+>}") Extend();



<position> := ( top = "0,0"
	      | foil emacs = "1921,42"
	      );

place window at <position> = Window.Move($1);

copy window position = Clipboard.Set(Window.GetPosition());
