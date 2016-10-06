###
### Advanced voice commands for Microsoft Excel 2003
###
###   commands following '#' without a definition are (sample) Dragon
###   built-in commands
###
###
### Includes commands for: charts, auto filters, formatting cells
###

include "string.vch";


## 
## Dialog accelerators (combos):
## 

size 1..40                             = HeardWord(Size) $1;

decimal places 0..3                    = HeardWord("Decimal places") $1;

(Minimum|Maximum) 0..20                = HeardWord($1) $2;
(Minimum|Maximum) 0..9 point 0..9      = HeardWord($1) $2.$3;
(Minimum|Maximum) 0..9 point 0..9 0..9 = HeardWord($1) $2.$3$4;



## 
## Commands for dealing with charts/graphs:
## 

make                    chart = {alt+i}h    {alt+f};
make (scatter=x|line=l) chart = {alt+i}h $1 {alt+f};


  # to edit most graph subparts, select that part then do:
format graph = {shift+f10}o;

format graph size 1..40 = {shift+f10}o {ctrl+tab}f {alt+s}$1{enter};


# NOTE: to reorder data series, use format graph on one of the data
# series (click on a point) and then use the series order tab...


  # make a given selected data series Black and seven point:
darken series = {shift+f10}o {ctrl+tab}p
	{alt+c}{down}{enter}
	{alt+f}{down}{enter}
	{alt+b}{down}{enter} #
	{alt+z}7
	{enter}
	{ctrl+left}  # move to next data series
	;

  # to add a linear regression trendline first select a data series then do ...
add linear   = {shift+f10}r {ctrl+tab} {alt+e} {alt+r} {enter};


# 
# The remaining chart commands require either being at a chart sheet,
# or at a normal sheet with a selected entire graph.
# 

  # Pull down Chart menu:
# ChartMenu() := {shift+f10};  # this doesn't work for chart sheets...
ChartMenu() := {alt+c}{down};

chart options     = ChartMenu() it;

<part> := (title = t | bottom = C | left = V);

      chart <part> = ChartMenu() it {alt+$1};
copy  chart <part> = ChartMenu() it {alt+$1}  {home}{shift+end}{ctrl+c}{esc};
paste chart <part> = ChartMenu() it {alt+$1}  {ctrl+v}{enter};


source data        = ChartMenu() s HeardWord(Series);
add    data	   = ChartMenu() a;


print       graph = {ctrl+p}{enter};
  # hardwired to use first printer in list: now common area color printer:
print color graph = {ctrl+p} {alt+m}{up_10}{enter} Wait(100) {enter};


# 
# These commands require being at a chart sheet:
# 

<place> := ( "Y. Axis" = {up_4} | "X. Axis" = {up_4}{right} );

<place>        = {esc}$1;
format <place> = {esc}$1 {shift+f10}o;



##
## Auto filters:
##

turn on auto filters = {alt+d}ff;
turn auto filters on = {alt+d}ff;

  #  open an auto filter list:
open  filter = SendSystemKeys("{ctrl+up}{alt+down}{home}");

  #  close an auto filter list:
close filter = SendSystemKeys("{alt+up}");

  #  open an auto filter list and choose custom filter:
open custom  = SendSystemKeys("{ctrl+up}{alt+down}{home}{down}{down}{enter}");

  #  select current data region of current column minus top cell:
grab data    = SendSystemKeys("{ctrl+up}{down}{shift+ctrl+down}");



## 
## Formatting cells:
## 

# format cell

format cells = {ctrl+1};


<kind> := (
            "date"             = "dat"
          | "number"           = "num{alt+d}0{alt+u}-"
          | "separated number" = "num{alt+d}0{alt+u}+"
          | "currency"         = cur{alt+d}2{alt+s}{up_14}{down}{alt+n}{down_4}
          | "percentage"       = "per{alt+d}2"
          );

Number(kind) := {ctrl+1} Wait(1000) n {alt+c} $kind;

format that as <kind>      = Number($1)            Wait(100) {enter};
format that as <kind> 0..6 = Number($1) {alt+d} $2 Wait(100) {enter};
