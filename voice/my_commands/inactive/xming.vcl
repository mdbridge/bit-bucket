### 
### Voice commands for most Xming windows:
### 



## 
## Window manipulation commands:
## 

# 
# Don't appear to be able to capture Alt+space with Xming:
#
System(key) := SetMousePosition(1, 10, 10) ButtonClick(2, 1) Wait(100) 
               $key Wait(10);

include "windows.vch";

#(close=C | minimize=n | minimise=n | maximize=x | restore=R) [the] window 
#	= System($1);
(close=C) [the] window = System($1);
