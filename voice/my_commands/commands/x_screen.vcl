### 
### Voice commands for xterms running the UNIX screen command
### 

include "numbers.vch";
include "environment.vch";
include "optional.vch";


#Prefix() := {ctrl+a};
Prefix() := '{ctrl+]}';


## 
## From outside screen:
## 

screen list = "screen -list{enter}";

screen (attach=-r | steal='-d -r') = "screen $1 ";


## 
## Setting correct DISPLAY:
## 

screen set display = 
        IfHome("setenv DISPLAY ",
               "setenv DISPLAY lillibridg3.americas.hpqcorp.net:0.0"{enter});

  # this version only works if home directory is shared:
screen auto set display = 
	Prefix() z  Wait(100)
	'echo \$DISPLAY > ~/Tmp/display' {enter}
	fg{enter}
	"setenv DISPLAY `cat ~/Tmp/display`" {enter};


## 
## Creating/Navigating between screen windows:
## 

new      window = Prefix() c;

window 0..9     = Prefix() $1;

next     window = Prefix() n;
previous window = Prefix() p;

screen set window title = Prefix() A;


## 
## Miscellaneous commands:
## 

screen command = Prefix() ':';
screen detach  = Prefix() d;

include "switch.vch";

screen title <window_suffix> =
	Prefix() z  Wait(100)
	'\echo -e "\033]0;xterm $1\007"' {enter}
	fg{enter};


## 
## Scrollback/copy mode:
## 

  # enter scroll back mode; exit via escape
screen scrollback = Prefix() '[';

screen (page up={ctrl+b}|page down={ctrl+f}) [<my1to100>] = REPEAT($2, $1);

screen (up=k|down=j) [<my1to100>] = REPEAT($2, $1);
