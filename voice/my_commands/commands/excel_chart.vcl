###
### Chart voice commands for Microsoft Excel 2013
###
###   commands following '#' without a definition are (sample) Dragon
###   built-in commands
###

include "locale_PC.vch";


Ribbon(keys) := {f10}$keys;

File()       := Ribbon(f); 
Home()       := Ribbon(h);
Insert()     := Ribbon(n);
PageLayout() := Ribbon(p);

  # ribbons present only when a chart is selected:
Design()     := Ribbon(jc);
Format()     := Ribbon(ja);



### 
### Primitive functions
### 
###   (use first embedded chart if no chart already active)
### 

   # sizes in inches
 SizeChart(x,y) := Format() W$x {enter}   Format() H$y {enter};

PlaceChart(x,y) := Excel.MoveChart($x,$y);


## 
## Selecting/formatting particular chart parts
## 
##       These functions create a chart part if it does not already
##   exist.
## 

FontSize(size) := Home() fs $size {enter};

  # move keyboard focus to the single open task pane:
TaskPane() := {alt}{esc}{f6};

<part> := ( chart title    = title
          | X axis	   = X
          | Y axis	   = Y
          | X [axis] title = "X title"
          | Y [axis] title = "Y title"
	  | legend	   = legend
	  | plot area	   = "plot area"
	  | entire chart   = "chart area"
	  | chart area	   = "chart area"
	  );

SelectPart     (part)	    := Excel.GoChartPart($part);
  # similar but selects the text inside for titles:
GoPart	       (part)	    := SelectPart($part) {enter};

  # this opens a sidebar, which is not easy to control by voice:
FormatPart     (part)       := SelectPart($part) Format() m TaskPane();

ReplacePart    (part, text) := GoPart($part) Wait(100) 
		               $text Wait(100) {esc} Wait(100);
SetPartFontSize(part, size) := GoPart($part) FontSize($size);



### 
### Selecting charts:
### 

## 
## Selecting charts via selection pane:
## 

ExitSelectionPane() := Mouse.Click(window, 500, 10);

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


Column() := c;

<make> := ( make	= "" 
          | make large	= Wait(500)  SizeChart(8,7) 
          | make medium = Wait(500)  SizeChart(8,6) 
       	  | make paper	= Wait(1000) SizeChart(11,7) );

<type> := ( scatter=d{down} | line=n{down} | default="" | column=Column() );

<make> [<type>] chart = Insert() When($2,$2,Column()) {enter} $1;



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

  # width by height:
size chart 1..20 by 1..20 = SizeChart($1, $2);


## 
## Saving charts:
## 

  # we are careful to preserve any existing file name here:
CD(pathname)   := {end} " " {ctrl+shift+home} {ctrl+c} {backspace}
		  $pathname {enter} {backspace}
		  {ctrl+v} {backspace};

save as PDF    = {f12} {alt+t}p{alt+n};
save as figure = {f12} {alt+t}p{alt+n}
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
##       These commands create a chart part if it does not already
##   exist.
## 

# 
# of current selected chart part:
# 

format (chart|graph) [part] = Format() m TaskPane();
# font size <n>   (defined in Excel.vcl)


# 
# by chart part name:
# 

go      <part>            = GoPart($1);

format  <part>            = FormatPart($1);
format  <part> size 1..40 = SetPartFontSize($1, $2);

copy    <part> = GoPart($1) {ctrl+a}{ctrl+c}{esc};
paste   <part> = GoPart($1) {ctrl+a} Wait(100) {ctrl+v}{esc};

# dictation to a chart part does not work due to a DNS bug;
# use the following instead:

  # really needs an "escape" after the transfer:
edit    <part> = GoPart($1) {ctrl+a}{ctrl+c} HeardWord(show, dictation, box);

set     <part> to <_anything> = GoPart($1) $2 {esc};


## 
## Formatting axis's:
## 

  # how often ticks occur on the x-axis (0 = never):
set ticks  [interval] 0..50 = Excel.SetTickInterval($1);

  # how often labels occur on the x-axis & make 1 tick per label:
set labels [interval] 1..50 = Excel.SetLabelInterval($1)
                              Excel.SetTickInterval($1);

set labels [interval] auto  = Excel.SetLabelInterval(0);

      task pane = TaskPane();  # moves focus ...
close task pane = TaskPane() {ctrl+space}c;

# 
# once formatting task pane is open:
# 

<field> := (Minimum | Maximum | major [unit] = Major | minor [unit] = Minor);

Set(field, value) := TaskPane() HeardWord(Click, $field) {end}{shift+home} $value;

set  <field>                        = Set($1,"");
set  <field> 0..50 [(hundred=00)]   = Set($1, $2$3)    {tab};
set  <field> 0..9 point 0..9 [0..9] = Set($1, $2.$3$4) {tab};
  # must not be on auto already:
(auto|reset) <field>                = Set($1, {tab}{enter});


  # with an axis selected, go to Number part of formatting task pane:
GoNumber() := Format() m TaskPane() -{down} -{down} -{down} +;

number category (number=n|currency=nc) = GoNumber() {tab}$1{enter};

decimal places 0..3		       = GoNumber() {tab_2} $1{tab};


## 
## Editting data series/data:
## 

  # to add a linear regression trendline first select a data series then do ...
add linear [regression] = Excel.AddTrendline();


# 
# to reorder data series, change source data, or add a data series,
# use "click design", "click select data".
# 


# 
# to make a graph all-black, it may be simplest to play with color
# themes, design chart styles
# 


## 
## Composite edits:
## 

  # make paper chart ready for use as a figure (idempotent):
massage chart =
    SetPartFontSize("Y title", 24)
    SetPartFontSize("X title", 24)
    SetPartFontSize(Y, 20)
    SetPartFontSize(X, 20)
    SetPartFontSize(legend,  20)
    Excel.NoChartBorder()
    Excel.PlotAreaBorder(1);

  # like "massage chart" but with bigger fonts:
larger font chart =
    SetPartFontSize("Y title", 28)
    SetPartFontSize("X title", 28)
    SetPartFontSize(Y, 28)
    SetPartFontSize(X, 28)
    SetPartFontSize(legend,  28)
    Excel.NoChartBorder()
    Excel.PlotAreaBorder(1);


  # make legend opaque, outlined in black:
outline legend = Excel.OutlineLegend();

  # first select a data series; increases its marker and line size:
increase series = Excel.SetMarkerSize(15)
	 	  Excel.SetSeriesLineWidth(3.5);

less chart junk = Excel.PlotAreaBorder(0)
                    # put horizontal grid lines in light gray:
		  Excel.MajorGridlinesGrey(217);


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
    ReplacePart("Y title", $2)          Wait(1000)
    ReplacePart("X title", $1);
