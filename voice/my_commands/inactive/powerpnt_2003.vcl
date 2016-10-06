### 
### Voice commands for Microsoft PowerPoint 2003
### 

## 
## Loading/saving /switching between powerpoints:
## 

# file save as

# window: show list of numbered PowerPoint's

  # switch to PowerPoint with PowerPoint assigned number (see Window menu)
window 1..10 = {alt+w} $1;


## 
## Switching between views:
## 

# slideshow: start slideshow at beginning

current slideshow = {shift+f5};

# view {normal, slide sorter, notes page}


## 
## Navigation between slides:
## 

# first/next/previous slide
# move {forward, back} <n> slides

# slide <n>


## 
## Operations on entire slides:
## 

# insert slide, duplicate slide, copy slide
# zoom {in,out} <n> percent


## 
## Selecting objects:
## 

# {next, previous} object
# object <n>


## 
## Draw menu:
## 

# send to back
# bring to front

# [un]group

Draw() := {alt+u}{left_2};

draw menu       = Draw();

snap to grid    = Draw() ig;
snap to objects = Draw() iS;


## 
## Drawing toolbar:
##

DrawTool(count) := {alt+u}{esc}{right_$count};

draw line      = DrawTool(1){enter};
draw arrow     = DrawTool(2){enter};
draw rectangle = DrawTool(3){enter};

fill color     = DrawTool(11){up};
line color     = DrawTool(12){up};
text color     = DrawTool(13){up};

line width     = DrawTool(14){up};
line dashing   = DrawTool(15){up};
arrow type     = DrawTool(16){up};


<color> := ( default = {esc} | auto = ""
           | 1      = {down_1}
           | 2      = {down_2}
           | 3      = {down_3}
           | 4      = {down_4}
           | 5      = {down_5}
           | 6      = {down_6}
           | 7      = {down_7}
           | 8      = {down_8}
              # these are HP Labs color template choices:
           | white  = {down_1}
           | black  = {down_2}
           | green  = {down_6}
           | orange = {down_7}
           | red    = {down_8}
);

fill color <color>     = DrawTool(11){up}{down} $1{enter};
line color <color>     = DrawTool(12){up}{down} $1{enter};
text color <color>     = DrawTool(13){up} $1{enter};

# fill that with {black, white, red...}: NOT template colors


## 
## Text properties:
## 

# make that [not] {italics, bold, underline}
# format that normal 
# bold that

Font() := {alt+o}f;

subscript that   = '{ctrl+=}';
superscript that = '{ctrl++}';

font size 8..60 = Font() {alt+s}$1{enter};

# {increase, decrease} that by <n> points
# format that <n> points

# make that {black, white, red...}: NOT template colors
