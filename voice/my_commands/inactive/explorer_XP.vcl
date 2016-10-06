### 
### Voice commands for Windows Explorer:
### 
###   see also _explorer.vcl
### 

include "locale_PC.vch";


## 
## Commands for navigating around filesystems
## 

UP(count)      := Repeat($count, {alt+v}ou);

DOWN(pathname) := {alt+d} Wait(500) {tab}
                  Replace($pathname, "/", "{enter}") {enter}
             	  {right}{left}{down}{up};

CD(pathname)   := {alt+d} Wait(500) $pathname {enter};

include "navigate.vch";

  # enter currently selected directory:
down directory               = DOWN("");


## 
## Mapping network drives:
## 

map network drive = {alt+t}n;

map Z. drive	  = {alt+t}n Wait(1000) {shift+tab}z{tab}
		    {backspace} \\spica.labs.hpl.hp.com\mdl {enter};

map alternative Z. drive	  = {alt+t}n Wait(1000) {shift+tab}z{tab}
		    {backspace} \\ts-rhel5.labs.hpl.hp.com\mdl {enter};


## 
## Adjusting the view:
## 

  # allow reload command to work like with Internet Explorer:
reload = {ctrl+r};

as (thumbnails=h|tiles=s|icons=n|list=l|details=d) = {alt+v} $1;

arrange by (name=n|size=s|type=t|date modified=m|modified=m|groups=g) =
	{alt+v}i $1; 


## 
## Operations on selected files:
## 

include "letters.vch";

open that with           = {alt+f}h;
open that with <letter>  = {alt+f}h $1;

  # requires a shortcut called "emacs" in SendTo folder:
open that with emacs     = {alt+f}ne;

show properties = {alt+f}r;

  # selected file must not already have an extension
make that [a] text file = {f2} {end} .txt {enter};



## 
## Other operations:
## 

new folder = {alt+f} w f;
