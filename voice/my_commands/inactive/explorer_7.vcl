### 
### Voice commands for Windows Explorer (Windows 7):
### 
###   see also _explorer.vcl
### 

include "locale_PC.vch";


## 
## Commands for navigating around filesystems
## 

UP(count)      := Repeat($count, {alt+up} Wait(100));

DOWN(pathname) := {alt+d} Wait(500) 
                  Replace($pathname, "/", "\") {enter};

CD(pathname)   := {alt+d} Wait(500) $pathname {enter};

include "navigate.vch";


## 
## Open command prompt here...
## 

  # so it shows the open command prompt here option:
[click] File = {alt+shift+f};

[open] command prompt here = {alt+shift+f} Wait(100)
                             HeardWord("Open command window here");


## 
## Mapping network drives:
## 

map network drive = {alt+t}n;

map W drive	  = {alt+t}n Wait(1000) {shift+tab}w{tab}
		    {backspace} \\spica.labs.hpecorp.net\mdl {enter}
		    #WaitForWindow(Windows) y
		    WaitForWindow("Connect to spica.labs.hpecorp.net")
		    Americas\LillibridgeM{tab};

map alternative W drive = {alt+t}n Wait(1000) {shift+tab}z{tab}
                          {backspace} \\ts-rhel7.labs.hpecorp.net\mdl {enter};


## 
## Adjusting the view:
## 

  # allow reload command to work like with Internet Explorer:
(refresh|reload) = {ctrl+r};

as (extra large icons=x | large icons=r{enter} | medium icons=m |
    small icons=n | icons=r{enter} | list=l | details=d | tiles=s ) = {alt+v} $1;


(arrange|sort) by (name=n|size=s|type=t
                   |date modified={down}{enter}|modified={down}{enter}) =
	{alt+v}o $2; 


## 
## Operations on selected files:
## 

include "letters.vch";

open that with [<letter>] = {alt+f}h $1;

  # requires a shortcut called "emacs" in SendTo folder:
open that with emacs      = {alt+f}n Wait(100) e;

open [that] in new window = {shift+f10}e;


show properties    = {alt+f}r;

copy pathname      = {shift+f10}a;


  # selected file must not already have an extension
make that [a] text file = {f2} {end} .txt {enter};

please delete = {Del} WaitForWindow("Delete F*") HeardWord(click, Yes);



## 
## Other operations:
## 

new folder = {alt+f} w f;

open first = {end}{home}{enter};

  # "address" of current directory:
go to address = {alt+d};
copy address  = {alt+d}{ctrl+c} {shift+tab};
