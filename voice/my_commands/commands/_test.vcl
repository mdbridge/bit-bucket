# -*- encoding: windows-1252 -*-
### 
### Current test functions:
### 

include "extended_string.vch";
include "switch.vch";

############################################################

launch modified X =
       "cd /usr/src/xorg-server-1.14.3-1/build{enter_2}"
       "hw/xwin/XWin -ac -multiwindow -clipboard";#{enter}";
