### 
### Voice commands for putty
### 

paste that = {shift+Ins};



## 
## Window manipulation commands:
## 

#
# putty appears to ignore alt space
#
# Thus, we are instead using a right click at the top left of the window.
#
System(key) := Mouse.Click(right, window, 10, 10) Wait(100) $key Wait(10);

include "windows.vch";
