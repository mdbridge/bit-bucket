### 
### Voice commands for Firefox (version 47.0)
### 
###      This file contains website-independent commands; see
###   *_sites.vcl for website-specific commands.
### 
###   See also Firefox_bookmark.vcl
### 

include "Firefox.vch";

FixFocus() := Address() UnAddress();


## 
## Navigation:
## 

# 
# Within a page:
# 

next     frame = {f6};
previous frame = {shift+f6};

       pane (up={PgUp}|down={PgDn}|top={ctrl+home}) = FixFocus() $1;
second pane (up={PgUp}|down={PgDn}|top={ctrl+home}) = Address() {shift+f6} $1;


#
# Other browser components:
#

  # built-in broken in Firefox 6.0:
(show|view) [page] source = FixFocus() {ctrl+u};

  # push options button of noscript:
options button = {alt+o} WaitForWindow("NoScript Options") 
	       	 {esc} Wait(100) {enter};

show inspector			    = {ctrl+shift+c};
[(open|show|close)] developer tools = {ctrl+shift+c};



## 
## Clicking links/gizmos:
## 

# 
# These commands require the mouseless browsing extension.
# 
# Plug-in configuration:
# 
#   General->not execute automatically without pressing enter
#   ID-types->all modifiers   = ctrl+alt
#   Keys->blur active element = ctrl+DIVIDE  (press control num pad slash)
#

Blur()   := {ctrl+NumKey/};
Toggle() := {NumKey.};
D(n)     := When($n,{alt+ctrl+$n});
HintFocus(operation, d_1, d_2, d_3, d_4) :=
    Blur() D($d_1) D($d_2) D($d_3) D($d_4) {shift} $operation;


blur me		= Blur();

show    numbers = Blur() Toggle();
refresh numbers = Blur() Toggle() Toggle();

<pick> 0..9 [0..9 [0..9 [0..9]]] = HintFocus($1, $2, $3, $4, $5);

<pick> := (        pick = {enter} 
          | proceed     = {enter} # better recognized than just pick by itself
          | go     pick = ""
          | push   pick = {ctrl+enter}    # stay but open new tab w/ link
          | tab    pick = {ctrl+shift+enter}
          | window pick = {shift+enter}

          | menu   pick = {shift+f10}
          | save   pick = {shift+f10} Wait(100) k
          | copy   pick = {shift+f10} Wait(100) a # copy URL of link

          | drop   pick = {enter}{alt+down}
	  | HP     pick =  Wait(500) mark.lillibridge@hpe.com{tab}


          | hit    pick = {enter}
	     # not available with mouseless browsing:
          | hover  pick = Beep() Vocola.Abort()
          );


# 
# These commands use the built-in quick find link:
# 

       link <_anything> = Blur() "'$1" {enter};
new    link <_anything> = Blur() "'$1" {ctrl+enter};
window link <_anything> = Blur() "'$1" {shift+enter};



##
## Miscellaneous:
##

# {increase, decrease} font

zoom in 0..10  = {ctrl+0} Repeat($1, {ctrl++});

force refresh = {ctrl+shift+r};



## 
## Experimental:
## 

include "string.vch";

label <_anything> = Blur() Replace("/$1", " :", ":") Wait(100) {tab};

caret browsing = {f7}; # toggle
