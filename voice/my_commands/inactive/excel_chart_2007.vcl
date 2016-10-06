###
### Chart voice commands for Microsoft Excel 2007
###
###   commands following '#' without a definition are (sample) Dragon
###   built-in commands
###

include "locale_PC.vch";


Ribbon(keys) := {f10}$keys;

Button()     := Ribbon(f); # office button
Home()       := Ribbon(h);
PageLayout() := Ribbon(p);

  # ribbons present only when a chart is selected:
Design()     := Ribbon(jc);
Layout()     := Ribbon(ja);
Format()     := Ribbon(jo);



### 
### Primitive functions
### 
###   (a chart or part of a chart must already be selected)
### 

   # sizes in inches
 SizeChart(x,y) := Format() W$x {enter}   Format() H$y {enter};

  # requires move_chart VBA macro bound to {ctrl+q}:
PlaceChart(x,y) := {ctrl+q} "$x,$y" {enter};


## 
## Selecting/formatting particular chart parts:
## 

FontSize(size) := Home() fs $size {enter};


  # this one is tricky for some reason; can't move directly from X title...
YTitle() := l m {enter} Layout() i v m;

<place> := ( 
             chart title   = t m 
           | X. axis title = i h m    | bottom title = i h m
           | Y. axis title = YTitle() | left   title = YTitle()
           | legend        = l m
           | X. axis       = a h m
           | Y. axis       = a v m
           | plot area     = o m
	   | entire chart  = o m {esc} {esc} Layout() m
           );

FormatPlace      (place)       := Layout() $place;
GoPlace          (place)       := FormatPlace($place) {enter};
SetPlaceFontSize (place, size) := GoPlace($place) FontSize($size);
ReplacePlace     (place, text) := GoPlace($place) Wait(100) 
		                    $text Wait(100) {enter} Wait(100);



### 
### Selecting charts:
### 

## 
## Selecting charts via selection pane:
## 

ExitSelectionPane() := SetMousePosition(1, 500, 10) ButtonClick(1,1);

Pick(n) := PageLayout() ap PageLayout() ap 
	   {up_20} {down_ Eval($n-1) } {enter}
	   ExitSelectionPane();

(show|hide) selection pane = PageLayout() ap  ExitSelectionPane();

pick    pane 1..10         = Pick($1);
destroy pane 1..10         = Pick($1) {Del};



### 
### Creating charts:
### 
###   (select data range or 1 cell in range first)
### 

  # put selected cell in the ith chart data block:
chart block 1..9 = {ctrl+g}a5{enter} Repeat($1, {ctrl+right_2}) {ctrl+left};


<make> := ( make="" 
          | make large=Wait(500) SizeChart(8,7) 
          | make medium=Wait(500) SizeChart(8,6) 
       	  | make paper=Wait(1000) SizeChart(11,7) );

<make> [(scatter=x|line=l|default=""|column="")] chart = 
    {alt+i}h When($2,$2,l) {enter} $1;



### 
### Commands dealing with entire charts:
### 

## 
## Positioning and resizing charts:
## 

place chart        (medium=580|large=580|paper=795) 1..4 = 
        PlaceChart(Eval('6 + $1 * ($2-1)'), 70);

place chart second (medium=435|large=505)           1..4 = 
        PlaceChart(Eval('6 + 580 * ($2-1)'), Eval(70 + $1));

size chart 1..20 by 1..20 = SizeChart($1, $2);


## 
## Saving charts:
## 

  # we are careful to preserve any existing file name here:
CD(pathname)   := {end} " " {ctrl+shift+home} {ctrl+c} {backspace}
		  $pathname {enter} {backspace}
		  {ctrl+v} {backspace};

save as PDF    = Button() fp;
save as figure = Button() fp WaitForWindow("Publish as PDF or XPS") Wait(2000)
     	         CD(UNIX(work:~/deduplication/tube/papers/fragmentation/figures));



### 
### Commands for editing charts:
### 

## 
## Changing chart type:
## 

change chart type = Design() c;
without markers   = Design() c {tab}{home}{down_3} {enter};  # line chart


## 
## Selecting/(generic) formatting particular chart parts:
## 
##   (a chart or part of a chart must already be selected)
## 

# 
# of current selected chart part:
# 

format (chart|graph) [part] = Layout() m;
# font size <n>   (defined in Excel.vcl)


# 
# by chart part name:
# 

go       <place>            = GoPlace($1);

format   <place>            = FormatPlace($1);
format   <place> size 1..40 = SetPlaceFontSize($1, $2);


copy     <place> = GoPlace($1) 
                   {shift+f10}{down_2}{enter} {ctrl+a}{ctrl+c}{esc};

paste    <place> = GoPlace($1) 
                   {shift+f10}{down_2}{enter} {ctrl+a} Wait(100) {ctrl+v}{esc};


  # no editing, just dictating a new version; dictate a newline at end:
replace  <place> = GoPlace($1) Wait(100) {space}{backspace};

  # really needs an "escape" after the transfer:
edit     <place> = GoPlace($1)Wait(100)
                   {shift+f10}{down_2}{enter} {ctrl+a}{ctrl+c}
		   HeardWord(show, dictation, box);


## 
## Formatting axis's:
## 

  # how often ticks occur on the x-axis:
set ticks interval 1..50 = FormatPlace(ahm) {alt+b} $1 {enter};

  # how often labels occur on the x-axis & make 1 tick per label:
set X.    interval 1..50 = FormatPlace(ahm) {alt+s}{tab} $1 {enter} Wait(100)
                           FormatPlace(ahm) {alt+b} $1 {enter};


# 
# once formatting dialog box is open:
# 

Format Axis:

  <field> := (minimum = f | maximum = i | major unit = x | minor unit = e);
  
  Set(field, value) := {alt+v}{alt+v} {alt+$field}{space} {tab} $value;
  
  set  <field>                        = Set($1,"");
  set  <field> 0..50 [(hundred=00)]   = Set($1, $2$3)    {tab};
  set  <field> 0..9 point 0..9 [0..9] = Set($1, $2.$3$4) {tab};
  auto <field>                        = Set($1, {shift+tab}{left});

:

decimal places 0..3               = HeardWord("Decimal places") $1;


## 
## Editting data series/data:
## 

  # to add a linear regression trendline first select a data series then do ...
add linear [regression] = {shift+f10}r {ctrl+tab} {alt+e} {alt+r} {enter};


# 
# to reorder data series, change source data, or add a data series,
# use "design", "select data".
# 


# 
# to make a graph all-black, it may be simplest to play with color
# themes, design chart styles
# 


## 
## Composite edits:
## 

  # sometimes must dismiss a "are you sure you want to use a complicated
  # formatting?" question:
YES() := Wait(500) y Wait(500);

  # make paper chart ready for use as a figure (idempotent):
massage chart =
    SetPlaceFontSize(YTitle(), 24)
    SetPlaceFontSize(ihm, 24)
    SetPlaceFontSize(avm, 20)
    SetPlaceFontSize(ahm, 20)
    SetPlaceFontSize(lm,  20)
      # set chart border to no line:
    FormatPlace(o m {esc} {esc} Layout() m) {down} {alt+n}	{enter}     
      # set plot area border to be a solid line:
    FormatPlace(om) {down} {alt+s} YES()
                           {alt+c}{right}{enter} {up}{enter}
    ;

  # like "massage chart" but with bigger fonts:
larger font chart =
    SetPlaceFontSize(YTitle(), 28)
    SetPlaceFontSize(ihm, 28)
    SetPlaceFontSize(avm, 28)
    SetPlaceFontSize(ahm, 28)
    SetPlaceFontSize(lm,  28)
      # set chart border to no line:
    FormatPlace(o m {esc} {esc} Layout() m) {down} {alt+n}	{enter}     
      # set plot area border to be a solid line:
    FormatPlace(om) {down} {alt+s} YES()
                           {alt+c}{right}{enter} {up}{enter}
    ;


  # make legend opaque, outlined in black:
outline legend = FormatPlace(lm)
		              {down_2} {alt+s} YES() 
                                       {alt+c} Wait(50) {right}{enter}{up}
		   {shift+tab}{up}     {alt+s} Wait(50) {alt+c}{enter}{up}
		   {shift+tab}{enter};

  # first select a data series; increases its marker and line size:
increase series = {shift+f10}f  
	 	  m{tab} o{down} {tab_2}15 {shift+tab_3}  # marker size=15
		  l{down} {tab} 3.5{enter};               # line width = 3.5

less chart junk = 
      # set plot area border to be none:
    FormatPlace(om) {down} {alt+n}{enter}
      # put horizontal grid lines in light gray:
    FormatPlace(g) hm WaitForWindow("Format Major Gridlines") Wait(100)
      {alt+s} Wait(100) {alt+c} {down_2} {enter} {esc}
    ;


# 
# Setting axis titles:
# 

<left> := ( deduplication                = "cumulative deduplication factor"
          | containers used per segment  = "containers used per segment"
          | containers used per megabyte = "containers used per MB"
          | containers read per megabyte = "containers read per MB"
          | champions per megabyte       = "champions per MB"
          | indexes per megabyte         = "container indexes per MB"
          | speed                        = "estimated MBps per RAID group"
          | theoretical speed            = "theoretical MBps per RAID group"
          | fragmentation                = "fragmentation per MB"
          | container size               = "mean container size"
          | balance                      = "max/mean dedupe size"
          );

<bottom> := ( chart          = "backup day #"
            | slice          = "cache/assembly size (KB then MB)"
            | megabyte slice = "cache/assembly size (MB)" 
            );

<bottom> shows <left> =
    ReplacePlace(YTitle(), $2)          Wait(1000)
    ReplacePlace(ihm, $1);
