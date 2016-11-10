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

include "windows.vch";

# 
# I disable these for several reasons:
# 
#   * commonly mis-recognized, especially click close!
#   * Chuck says we should use the '<cmd> window' commands instead
# 
click (Restore | Move | Size | Minimize | Maximize | Close) = Beep();


other monitor = SendSystemKeys({win+shift+right});

  # this exits from snap another window state if it exists
  # does not disturb application if not in that state
EXIT() := SendSystemKeys({win}) Wait(100) SendSystemKeys({esc});

<state> := ( lengthen	  = SendSystemKeys({win+shift+up})

              # these are for  Windows 10:
	   | left  half	  = Restore() SendSystemKeys({win+left})  EXIT()
	   | right half	  = Restore() SendSystemKeys({win+right}) EXIT()

	   | top    left  = Restore() SendSystemKeys({win+left})  EXIT()
                            Wait(100) SendSystemKeys({win+up})
	   | top    right = Restore() SendSystemKeys({win+right}) EXIT()
                            Wait(100) SendSystemKeys({win+up})
	   | bottom left  = Restore() SendSystemKeys({win+left})  EXIT()
                            Wait(100) SendSystemKeys({win+down})
	   | bottom right = Restore() SendSystemKeys({win+right}) EXIT()
                            Wait(100) SendSystemKeys({win+down})
	   );

<state> window = $1;



# 
# Tiling experiments via AutoHotkey hotkeys in kill.ahk <<<>>>
# 
# Hardwired for home for now...
# 
Extend() := SendSystemKeys({win+shift+up}{win+shift+right}{win+shift+left});

left  two thirds = SendSystemKeys("{alt+ctrl+(}") Extend();
right two thirds = SendSystemKeys("{alt+ctrl+)}") Extend();



<position> := ( top = "0,0"
	      | foil emacs = "1921,42"
	      );

place window at <position> = Window.Move($1);

copy window position = Clipboard.Set(Window.GetPosition());
