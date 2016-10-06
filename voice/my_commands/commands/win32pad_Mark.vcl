### 
### Win32pad voice commands, Mark's extensions
### 

include win32pad.vch;


## 
## Long leap commands:
## 

# the leap code below is basically that for DragonPad:

include "leap_definition.vch";

_Leap(direction, set_target, times) :=
        {shift+right}             # fake selecting our target at point
        {ctrl+f}                  # bring up find dialog box
        {alt+c}- {alt+w}-         # options: not whole words, case insensitive
        {alt+$direction}          # set direction to find
        {alt+n} $set_target       # set target for find
        Repeat($times,
          {enter}                 # do a find (may produce an error dialog box)
          {alt+w}{space} {alt+w}- # dismiss error dialog box if any 
        )
        {esc}                     # dismiss find dialog box
        {left}                    # exit selection, leaving point at start of 
        ;                         #   target or original if target not found

Leap3(direction, target, count) :=
	_Leap($direction, PrintablesToKeys($target), $count)
	_AdjustLeap($direction, $target);

LeapSame(direction, count) := Vocola.Error("leap same unavailable");

Leap(direction, target) := Leap3($direction, $target, 1);

include "leap3_long.vch";



## 
## Scrolling the window:
## 

  # side effects: clobbers clipboard
lift cursor = {ctrl+g} Wait(100) {ctrl+c} Wait(100) {esc}
              {down_70} {ctrl+g} Clipboard.Get() {enter};



## 
## Additional range operations:
## 

  # return number of lines in [$top, $bottom) considered as mod 100 references:
Count(top, bottom) := Minus(CalcLine($bottom), CalcLine($top));

ApplyEach(r1, r2, action) := LineMod($r1) Repeat(Count($r1,$r2), 
                                                 {home} $action {down});

include "printables.vch";

<r> := 0..99;

prefix  <r> comma <r> with <prn> = ApplyEach($1, $2, PrintablesToKeys($3));



## 
## At one point, we were using the mini-Vocola word movement commands:
## 

#<word_compass> := (
#       flee 
#     | jump 
#
#     | kill 
#     | toast  
#);
#
#<word_compass>      = HeardWord(my, $1);
#<word_compass> 2..9 = HeardWord(my, $1, $2);
