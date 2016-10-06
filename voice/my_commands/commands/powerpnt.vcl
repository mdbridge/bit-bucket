### 
### Voice commands for Microsoft PowerPoint 2013
### 

include "office.vch";

include "locale_PC.vch";
include "string.vch";
include "switch.vch";


Ribbon(keys) := {f10}$keys;

Home()   := Ribbon(h);
Insert() := Ribbon(n);
View()   := Ribbon(w);

  # ribbons present only when a drawn object is selected:
  # different when Excel object (part) selected!
Format() := Ribbon(jd);

  # ribbons present only when a table (part) is selected:
Design() := Ribbon(jt);
Layout() := Ribbon(jl);


  # missing from Dragon for some reason:
Click File   = {alt+f};



### 
### Using the bamboo tablet:
### 

# needs driver installed from CD; especially for below, getting
# correct mouse button:
#
# can set to use only one monitor (absolute):
#   control panel->hardware and sound->pen tablet properties->pen->tracking
#     ->pen mode details
#   screen area->monitor; may need to set monitor number as well here
#
# can also switch to relative (mouse mode)

# 
# To increase precision, can also use zoom or snap to grid
# 



### 
### Loading/saving/switching between powerpoints:
### 

File open    = {ctrl+f12};
File save as = {f12};


  # show list of numbered documents:
window       = View()w;

  # switch to document with above number:
window 1..10 = View()w $1;



### 
### Switching between views/panes:
### 

# slideshow: start slideshow at beginning

current slideshow = SendSystemKeys({shift+f5});
# during slideshow, # <enter> to go to a slide, ^s to see title dialog of slides
# to use presentation view, first check slideshow->use presenter view

fix cursor = HeardWord(IfHome(pink,red), area)
    	     HeardWord(PowerPoint, window); # <<<>>>


# view {normal, slide sorter, notes page}


  # go to normal view, leftmost pane (usually the "slides" list):
Slides() := View()t View()l;

slides        = Slides();
the slide     = Slides() {tab};  # normal view, usually main slide
speaker notes = Slides() {shift+f6_2};

edit speaker notes = Slides() {shift+f6_2} HeardWord(edit, all); # <<<>>>


(next="" | previous="shift+") pane = {$1f6};



### 
### Navigation between slides:
### 

# first/next/previous slide
# move {forward, back} <n> slides

# slide <n>


# in slide sorter and slides list pane, can move around by
# "characters" (e.g., left 12, down 2).



### 
### Operations on entire slides:
### 

# insert slide, duplicate slide, copy slide
# [un]hide slide

# in slide sorter view: can select multiple slides using shift left 2,
# shift down, and the like.  Selected/the current slide if no
# selection can be copied or cut.  Pasting slide(s) over a slide
# places those slides *after* the current slide.  Cut and paste of
# slides does not work in the slides list pane.

# design->slide size: to change the slide size (e.g., for posters)


## Zooming:

  # so can use from anywhere:
fit to window = View() f;

# zoom {in,out} <n> [percent]: goes to that absolute zoom value -- 100=normal

# can also zoom by: ctrl + mouse wheel:
control wheel (up=""|down="-") 1..100 = 
    Keys.SendInput({ctrl_hold}) Kludge.Wheel($1 $2) Keys.SendInput({ctrl_release});


## Scrolling around a zoomed in slide:

# One of my AutoHotkey scripts enables horizontal mouse wheel to
# scroll PowerPoint horizontally

  # PowerPoint seems to need pauses:
wheel (up=""|down="-") 1..100 = Repeat($2,Kludge.Wheel($1 1) Wait(50));

please scroll (up=+|down=-) 1..100 = Repeat($2,Kludge.Wheel($1 1) Wait(50));

please scroll (left=+|right=-) 1..100 =
    Keys.SendInput({shift_hold}) 
    #Kludge.Wheel($1 $2) 
    Repeat($2,Kludge.Wheel($1 1) Wait(50))
    Keys.SendInput({shift_release});



### 
### Selecting objects:
### 

# Can drag to select a set of objects; shift-touch to add to the
# currently selected set.  Can move/copy a selected set by dragging on
# it with (/ctrl) held down.  For grouped objects, click first on the
# group and then just on a sub-object to select just that sub-object.
# Can use shift to select several sub-objects within one group, but not
# across groups.

# {next, previous} object
# object <n>

# When the focus is inside a text box (inserting -- border is dashed),
# use escape to move the focus to the text box itself (solid line).
# At that point, you can use (shift)-tab to move among text boxes,
# arrow keys to move the box, & other stuff.  When a text box is
# selected, use enter to move focus inside it (inserting).

#
# These aren't 100% reliable; some slides have object 1 be something else.
# A saved mouse point may be more reliable.
#
text box (1=one\\number|two|three) = HeardWord(object, $1) {enter}{ctrl+home};
  # ensure select inside slide first
replace title = HeardWord(object, one\\number) {enter}{ctrl+home}{ctrl+shift+end};



### 
### Drawing:
### 

## 
## Creating objects:
## 

# built-in "duplicate that" sometimes duplicates the current slide
# rather than current object by mistake; don't seem to be able to
# consistently override.

clone that [1..50 times] = {ctrl+c}{ctrl+v When($1,_$1)};
# can also duplicate objects by dragging with control held down


# holding shift key while mousing:
#   resizing objects while preserving aspect ratio
#   limits lines to 45 angle multiples or connector points.

# holding shift key while using arrow keys:
#   resizes object (group)

<shape> := ( shape            = Vocola.Abort()  # user pick
	   | line             = {down_2} 
           | arrow            = {down_2}{right} 
           | rectangle        = {down_3} 
           | oval             = {down_4}{right}
           | circle           = 
    MsgBoxConfirm("Use 'draw oval' then stroke while holding the shift key", 
		  64, "Tip")
    Vocola.Abort()
           | curved rectangle = {down_3}{right} 
	   | left  brace      = {down_7}{right_4}
	   | right brace      = {down_7}{right_5}
	   );

draw <shape> = Home() sh $1 {enter};

  # select a line or shape and then do to set default attributes:
set as default = {shift+f10}d;

  # override built-in that does not allow positioning;
  # type immediately to not lose:
insert text box = Insert() x;


## 
## Arranging objects:
## 

# bring to front
# send to back

# [un]group, regroup
(group|ungroup) that = HeardWord($1);


arrange objects = Home() g;

  # group first unless want to rotate each individually:
rotate  objects = Home() go;
rotate (right=r|left=l|horizontally=h|vertically=v) = Home() go $1;

align   objects = Home() ga;
align (left=l|center=c|right=r|top=t|middle=m|bottom=b) = Home() ga $1;
distribute (vertically=v|horizontally=h)                = Home() ga $1;


GridAndGuides() := View() x;

snap to grid    = GridAndGuides() g+;

# use arrow keys to move a selected object(s) slightly in a given direction
#   hold control to override snapping when using arrow keys; 
#   hold alt to do the same when using the mouse

# alt + arrow keys = rotate object by large increments
#   add control for  smaller increments


## 
## Generic attributes:
## 

# use these to copy the attributes (including of any text) of an object:
copy  attributes = {ctrl+shift+c};
paste attributes = {ctrl+shift+v};

  # select object then issue this then click on a new object:
format painter = Home() fp;

# note: calling fill color, format painter, etc. on a group sets all
# the objects in the group at once


## 
## Line/border attributes:
## 

ShapeFill()    := Home() sf;
ShapeOutline() := Home() so;

line (width|weight) [0..8] = ShapeOutline() w When($2,{down_$2}{enter});

<arrow> := ( none=0 | right=4 | left=5 | double=6 );

arrow type   [0..10] = ShapeOutline() r When($1,{down_$1}{enter});
arrow type   <arrow> = ShapeOutline() r {down_$1}{enter};

line dashing [0..7]  = ShapeOutline() s When($1,{down_$1}{enter});


## 
## Color attributes:
## 

# shape fill, shape outline: set color to auto

#<color> := 0..9;
<color> := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
         | custom 0   = "0,6" | HP green = "0,6"
         | custom 1   = "1,6"
         | custom 2   = "2,6"
	   # these assume new HPE template w/ custom colors row
         | standard 0 = "0,7"
         | standard 1 = "1,7" | standard red        = "1,7"
         | standard 2 = "2,7"
         | standard 3 = "3,7" | standard yellow     = "3,7"
         | standard 4 = "4,7"
         | standard 5 = "5,7" | standard green 	    = "5,7"
         | standard 6 = "6,7" | standard light blue = "6,7"
         | standard 7 = "7,7" | standard       blue = "7,7"
         | standard 8 = "8,7" | standard dark  blue = "8,7"
         | standard 9 = "9,7" | standard purple     = "9,7"

	 | none       = "0,8";  # for no fill, outline

show (colors|color key) = SwitchToApp("^color key.tif",
     		   	  	      UNIX("~/talks/templates/color key.tif"));

SetColor(color) := {down_  Split("$color,0", ",", 1) }
		   {right_ Split("$color,0", ",", 0) } {enter};

line color [<color>] = ShapeOutline() When($1,SetColor($1));
fill color [<color>] = ShapeFill()    When($1,SetColor($1));
text color [<color>] = Home() fc      When($1,SetColor($1));

# fill that with {black, white, red...}: NOT template colors


## 
## Excel chart properties:
## 

format selection = Ribbon(jo) m;


## 
## Tables:
## 

# I usually just copy the HP template's table to get the formatting right
#   UGH!  that is very ugly, with bad chart ink

# select a row's text then use right touch for insert/delete rows ...


remove (top=t|bottom=b) [cell] margin = Layout() n m {alt+$1} 0{enter};

<border> := ( no borders=n | all borders=a | outside borders=s | inside borders=i
	    | top border=p | bottom border=b | left border=l | right border=r
	    | inside horizontal borders=h | inside vertical borders=v);

toggle <border> = Design() b $1;



### 
### Text properties:
### 

# make that {black, white, red...}: NOT template colors
# see also text color [...] above

# bullet that, remove bullets

# make that [not] {italics, bold, underline}
# format that normal 
# bold that

Font() := {alt+o}f;

<font> := ( Arial | Consolas );

font <font> = Font() {alt+f} $1 {enter};

subscript   that = '{ctrl+=}';
superscript that = '{ctrl++}';

    (font|text) size       =          Home() fs;
    (font|text) size 8..60 =          Font() {alt+s}$2{enter};
all (font|text) size 8..60 = {ctrl+a} Font() {alt+s}$2{enter};

# {increase, decrease} that by <n> points
# format that <n> points

# trick: can change text size of blank lines to control spacing between lines

# think these affect spacing before the selected lines:
      line spacing = Home() k lnm{enter}{tab};
exact line spacing = Home() k lne{enter}{tab};


### 
### Navigating within a text box:
### 

<row> := ( row | line | go );

<row> 1..30 = {ctrl+home} {down_ Eval($2-1) };


### 
### Miscellaneous:
### 

paste special = Home() vs;


## 
## Experimental version of leap based on peeking:
## 
##   Currently case-insensitive.  This also works in the speaker notes.
##   These are currently *not* short commands.
## 

  # offset to start of next string or 0 if none
FwdOffset(string) := 
	EvalTemplate("max(0, %s.find(%s,1))", 
	             Lower(Replace(Clipboard.Get(), Eval("'\r'"), "")),
                     Lower($string));
  # offset to end of previous string occurrence or 0 if none
BwdOffset(string) := 
	EvalTemplate("max(0, %s[::-1].find(%s[::-1],0))", 
	             Lower(Replace(Clipboard.Get(), Eval("'\r'"), "")),
                     Lower($string));


#Leap(string) := {shift+end} {ctrl+c} Wait(100)
Leap(string) := {shift+end}{shift+ctrl+end} {ctrl+c} Wait(100)
	Repeat(EvalTemplate("1-min(1,%i)", FwdOffset($string)),
	   {left} Beep() Vocola.Abort())  # not found
	{left}{right_ FwdOffset($string) };

#Retreat(string) := {shift+home} {ctrl+c} Wait(100)
Retreat(string) := {shift+home}{shift+ctrl+home} {ctrl+c} Wait(100)
	Repeat(EvalTemplate("1-min(1,%i)", BwdOffset($string)),
	   {right} Beep() Vocola.Abort())  # not found
	{right}{left_ BwdOffset($string) }{left_ Len($string) };



include "printables.vch";

leap          <prn> = Leap($1);
leap    after <prn> = Leap($1) {right};

retreat       <prn> = Retreat($1);
retreat after <prn> = Retreat($1) {right};

advance  <_anything> = Leap($1);
fallback <_anything> = Retreat($1);
