### 
### Experimental: access to current project's files via spoken names
### 

include "gnu.vch";


## 
## Opening project files via their precalculated spoken forms:
## 
## (Lists created via generators/units.rb)
## 

include "units.vch";
include "interface.vch";

unit   <units>     = FindFile(UNIX($1) {enter});
header <interface> = FindFile(UNIX($1) {enter});



## 
## Referring to project files (listed in ~/Tmp/files) using monster:
## 

Lookup(spoken, extension, info) :=
    {ctrl+u}{esc}! 'ruby ~/voice/symbols/lookup_file.rb "' 
                      When($spoken,$spoken,*) '" $extension $info'
    When($spoken, {enter}, Leap(u, *) {Del});


<type> := (unit = cpp | header = h | special = '^$' );

<type> monster <_anything> = {ctrl+x}{ctrl+f} Mark() {home}{ctrl+w}
                             Lookup($2,$1,2) When($2, {enter});

include monster <_anything> = 
    '#include ""{enter}' Leap(u,'"') Lookup($1,h,3) When($1,{down}{home});
