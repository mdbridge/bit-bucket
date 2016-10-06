### 
### Voice commands for moving the cursor in a given direction,
### optionally erasing as we go.
### 
###   Some commands use Windows key conventions so they may not work in all
###   applications.
### 
### 
### See also compass commands in global_short.vch
### 

include "numbers.vch";
include "string.vch";
include "optional.vch";



## 
## Commands for moving/selecting to the extreme of a compass direction:
## 

<simple_modifiers> := (
       shift         = shift+
     | control       = ctrl+
     | control shift = ctrl+shift+
     | shift control = ctrl+shift+
);

<long_extreme> := (
       top-of-file   = ctrl+home
     | top-of-buffer = ctrl+home

     | end-of-file   = ctrl+end
     | end-of-buffer = ctrl+end
);

[<simple_modifiers>] <long_extreme> = {$1$2};



## 
## Commands for moving a fixed distance in a compass direction:
## 

<long_direction> := (
        # moving by screenfuls:
       page up   = PgUp
     | page down = PgDn
);

[<simple_modifiers>] <long_direction> [<my1to100>] = {$1$2 When($3,_$3)};


  # need this for contexts where dictation is unavailable:
space-bar = " ";



## 
## Special case: moving orthogonal to erasing
## 

(dig={Del}|undig={space}{left}) [across] [1..20] (down|up) 1..20 = 
    Repeat($4, REPEAT($2, $1) {$3});
