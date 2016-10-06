### 
### Commands for creating and managing Gnuemacs windows and frames
### 
### Also see: gnu_buffer_short.vcl
### 

include "gnu.vch";


### 
### Windows:
### 

##
## The prefix "other" means execute the command in the other window
## (when there are only two windows) or the next window down (with wrap) when
## there are multiple vertically split windows.  Always refers to a
## window in the current frame.
##

other window = {ctrl+x}o Empty();

  # execute command in "other" window:
Other(command) := {ctrl+x}o $command {ctrl+u}-1{ctrl+x}o;


##
## Creating/destroying multiple windows
##

SplitWindow [vertically] = {ctrl+x}2 Empty();
SplitWindow horizontally = {ctrl+x}3 Empty();

      SingleWindow       =           {ctrl+x}1 Empty();
other SingleWindow       = {ctrl+x}o {Ctrl+x}1 Empty();


##
## Scroll current/other window:
##

  # scroll current window so that cursor is at:
<scroll> := (
      lift cursor  = {ctrl+u}0 {ctrl+l}  # top of screen
    | CenterCursor =           {ctrl+l}  # middle of screen
    | DropCursor   = {ctrl+u}-1{ctrl+l}  # bottom of screen
);

      <scroll>    =       $1;
other <scroll>    = Other($1);

  # Combo: move cursor to line <r> then lift that line:
#      <row> <r> lift cursor =       LineMod($2) {ctrl+u}0{ctrl+l};
#other <row> <r> lift cursor = Other(LineMod($2) {ctrl+u}0{ctrl+l});
      <row> <r> top         =       LineMod($2) {ctrl+u}0{ctrl+l};
other <row> <r> top         = Other(LineMod($2) {ctrl+u}0{ctrl+l});


other top of file = Other({esc}<);
other page up     = Other({PgUp});
other page down   = Other({PgDn});



## 
## Miscellaneous:
## 

  # compare the text of two windows, starting from point in each window:
compare windows = Do(compare-windows);

kill both buffers = {ctrl+x}k{enter} Other({ctrl+x}k{enter});



### 
### Frames:
### 

include "environment.vch";
include "switch.vch";


MakeFrame(parameters) := Elisp("(make-frame '( $parameters ))");

Name(name) := "(name . ""$name"") ";
Wide()     := '(width . ' IfHome(192, 165) ') ';
Position() := IfHome( "(top . 111) (left . 1955) ", 
	              "(top . 45)  (left . 1628) "   );

new                      frame = Do(make-frame);
new       <emacs_color>  frame = MakeFrame(Name($1));
new wide [<emacs_color>] frame = MakeFrame(Name(When($1,$1,"green emacs")) 
                                           Wide() Position());

name                     frame = Do(set-frame-name);
make frame <emacs_color>       = Do(set-frame-name) $1 {enter};


(other="" | next="" | previous={ctrl+u}-1) frame = $1 {ctrl+x}5o;
