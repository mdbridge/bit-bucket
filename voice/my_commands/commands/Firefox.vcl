### 
### Voice commands for Firefox (version 47.0)
### 
###      This file contains website-independent commands; see
###   *_sites.vcl for website-specific commands.
### 
###   See also Firefox_bookmark.vcl
### 

include "locale_PC.vch";
include "Firefox.vch";

FixFocus() := Address() UnAddress();


## 
## Window manipulation commands:
## 

#System(key) := Mouse.Click(right, window, 10, 10) Wait(100) $key Wait(10);
#
#include "windows.vch";


## 
## Navigation:
## 

# 
# Within a page:
# 

next     frame = {f6};
previous frame = {shift+f6};


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
# Also, remove Dragon's use of DIVIDE
# 

Blur()   := {ctrl+NumKey/};
Toggle() := {NumKey.};
D(n)     := When($n,{alt+ctrl+$n});
HintFocus(operation, d_1, d_2, d_3, d_4) :=
    Blur() D($d_1) D($d_2) D($d_3) D($d_4) {shift} $operation;

show    numbers = Blur() Toggle();
refresh numbers = Blur() Toggle() Toggle();
blur me		= Blur();

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
          );


#
# These commands require the VimFx extension.
#
# hints chars: 012345678 9
# blacklist: *
#

OverlayBlur()   := {shift+f1}{esc} Wait(100);
OverlayHintFocus(operation, d_1, d_2, d_3, d_4) :=
    $d_1 $d_2 $d_3 $d_4 Wait(100) $operation;

overlay blur me = OverlayBlur();

[show]	  hints = OverlayBlur() {shift+f1}zf;
  # use with go pick for multple link opens:
multi	  hints = OverlayBlur() {shift+f1}af; 
browser	  hints = OverlayBlur() {shift+f1}zF;
  # use go pick to position caret and enter caret mode:
caret	  hints = OverlayBlur() {shift+f1}v; 
clickable hints = OverlayBlur() {shift+f1}f;
all	  hints = OverlayBlur() {shift+f1}zf Wait(100) {ctrl+enter};

new next     = {shift+f1} ']';
new previous = {shift+f1} '[';
up a level   = {shift+f1} gu;

overlay <opick> 0..9 [0..9 [0..9 [0..9]]] = OverlayHintFocus($1, $2, $3, $4, $5);

<opick> := (        pick = {enter} 
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
           | radio  pick = {space} Wait(500) {shift+f1}zf
           | multi  pick = {ctrl+enter} Wait(500) {shift+f1}zf
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

  # built-in broken in Firefox 6.0:
(show|view) [page] source = FixFocus() {ctrl+u};

pane (up={PgUp}|down={PgDn}|top={ctrl+home}) = FixFocus() $1;
second pane (up={PgUp}|down={PgDn}|top={ctrl+home}) = Address() {shift+f6} $1;

force refresh = {ctrl+shift+r};

  # push options button of noscript:
options button = {alt+o} WaitForWindow("NoScript Options") {esc} Wait(100) {enter};

show inspector = {ctrl+shift+c};


## 
## Experimental:
## 

include "string.vch";

label <_anything> = Blur() Replace("/$1", " :", ":") Wait(100) {tab};

caret browsing = {f7}; # toggle
