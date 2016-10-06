### 
### Voice commands for Firefox (version 46.0)
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

#System(key) := SetMousePosition(1, 10, 10) ButtonClick(2, 1) Wait(100) 
#               $key Wait(10);
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

show    numbers = Blur() Toggle();
refresh numbers = Blur() Toggle() Toggle();
blur me         = Blur();

<pick> 0..9 [0..9 [0..9 [0..9]]] = Blur() D($2) D($3) D($4) D($5) $1;

<pick> := (        pick = {shift}{enter} 
          | proceed     = {shift}{enter} # <<<>>>
          | go     pick = {shift}
          | push   pick = {shift}{ctrl+enter}    # stay but open new tab w/ link
          | tab    pick = {shift}{ctrl+shift+enter}
          | window pick = {shift}{shift+enter}

          | menu   pick = {shift}{shift+f10}
          | save   pick = {shift}{shift+f10} Wait(100) k
          | copy   pick = {shift}{shift+f10} Wait(100) a # copy URL of link

          | drop   pick = {shift}{enter}{alt+down}
	  | HP     pick = {shift} Wait(500) mark.lillibridge@hpe.com{tab}
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

  # built-in broken in Firefox 6.0:
(show|view) [page] source = FixFocus() {ctrl+u};

pane (up={PgUp}|down={PgDn}|top={ctrl+home}) = FixFocus() $1;
second pane (up={PgUp}|down={PgDn}|top={ctrl+home}) = Address() {shift+f6} $1;

force refresh = {ctrl+shift+r};

  # push options button of noscript:
options button = {alt+o} WaitForWindow("NoScript Options") {esc} Wait(100) {enter};

show inspector = {ctrl+shift+c};
zoom in 0..10  = {ctrl+0} Repeat($1, {ctrl++});


## 
## Experimental:
## 

include "string.vch";

label <_anything> = Blur() Replace("/$1", " :", ":") Wait(100) {tab};

caret browsing = {f7}; # toggle
