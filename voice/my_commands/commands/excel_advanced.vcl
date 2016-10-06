###
### Advanced voice commands for Microsoft Excel 2013
###
###   commands following '#' without a definition are (sample) Dragon
###   built-in commands
###
###
### Includes commands for: auto filters, formatting cells, macros
###

include "optional.vch";

Ribbon(keys) := {f10}$keys;

Home()       := Ribbon(h);
PageLayout() := Ribbon(p);
View()       := Ribbon(w);



##
## Auto filters:
##

turn on auto filters = {alt+d}ff;
turn auto filters on = {alt+d}ff;

  #  open an auto filter list:
open  filter = SendSystemKeys({ctrl+up}{alt+down}{home});

  #  close an auto filter list:
close filter = SendSystemKeys({alt});

  #  open an auto filter list and choose custom filter:
open custom  = SendSystemKeys({ctrl+up}{alt+down}ff);

  #  select current data region of current column minus top cell:
grab data    = SendSystemKeys({ctrl+up}{down}{shift+ctrl+down});



## 
## Formatting cells:
## 

# format cell, format that

format cells = {ctrl+1};


<kind> := (
            date             = dat
          | number           = num{alt+d}0{alt+u}-
          | separated number = num{alt+d}0{alt+u}+
          | currency         = cur{alt+d}2{alt+s}{up_14}{down}{alt+n}{down_4}
          | percentage       = per{alt+d}2
          );

Number(kind) := {ctrl+1} Wait(1000) n {alt+c} $kind;

format that as <kind> [0..6] = Number($1) When($2,{alt+d} $2) Wait(100) {enter};


format that as ( normal=0 | bad=1 | good=2 | neutral=3 ) =
    {alt+h}j {home} {right_$1} {enter};



## 
## Excel Macros:
## 
## 
## Warning: various chart operations do not record well.
## 

# may want developer->use relative references turned on first...


Start(key) := View() mr "start_macro" {tab} $key{tab} {up_3}{enter} {enter};
End()      := View() mr;


  # need do this only first-time per machine if save personal workbook afterwards
start macro first time = Start(e);

start macro = Start("") WaitForWindow("Microsoft Excel") y End()
              Start(e)  WaitForWindow("Microsoft Excel") y;

end   macro = End();

run   macro [1..20 times] = REPEAT($1, {ctrl+e});
