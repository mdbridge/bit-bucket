### 
### Voice commands for most Reflection windows:
### 



## 
## Window manipulation commands:
## 

#
# I have Reflection configured so it accepts only the right alt key
# (left alt is passed though so we can accept Unicode characters):
#
System(key) := {RightAlt+space} $key;

include "windows.vch";

#(close=C | minimize=n | minimise=n | maximize=x | restore=R) [the] window 
#	= System($1);
(close=C) [the] window = System($1);
