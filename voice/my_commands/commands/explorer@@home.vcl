### 
### Voice commands for Windows Explorer (Windows 10):
### 
###   see also _explorer.vcl
### 

include "locale_PC.vch";

Ribbon(key) := {alt+$key};

File()      := Ribbon(f);
Home()      := Ribbon(h);
Share()     := Ribbon(s);
View()      := Ribbon(v);

  # missing from Dragon for some reason:
Click File   = File();


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

[open] command prompt here = File() p;
[open] admin   prompt here = File() m a;


## 
## Mapping network drives:
## 

Computer() := {alt+d} "This PC"{enter} Wait(400) {alt+c};
Map()	   := Computer() n{enter} WaitForWindow("Map Network Drive");

map network drive = Map();

map W drive	  = Map() {shift+tab}w{tab}
		    {backspace} \\spica.labs.hpecorp.net\mdl {enter}
		    #WaitForWindow(Windows) y
		    #WaitForWindow("Connect to spica.labs.hpecorp.net")
		    WaitForWindow("Windows Security")
		    Americas\LillibridgeM{tab};

map alternative W drive = Map() {shift+tab}z{tab}
                          {backspace} \\ts-rhel7.labs.hpecorp.net\mdl {enter}
			  WaitForWindow("Windows Security")
			  Americas\LillibridgeM{tab};


## 
## Adjusting the view:
## 

  # allow reload command to work like with Internet Explorer:
(refresh|reload) = {ctrl+r};

<view> := ( extra large icons = ""
       	  | large icons	      = {right}
	  | medium icons      = {right_2}
	  | small icons	      = {down}
	  | icons	      = {right}           # same as large icons
	  | list	      = {right}{down}
	  | details	      = {right_2}{down}
	  | tiles	      = {down_2}
	  | content	      = {right}{down_2}
	  );

as <view> = View() l {home} $1{enter};


(arrange|sort) by (name=""|size={down_3}|type={down_2}
                  |date modified={down_1}|modified={down_1}) =
	View()o $2{enter}; 


## 
## Operations on selected files:
## 

include "letters.vch";

open that with [<letter>] = {shift+f10}h{enter} $1;

  # requires a shortcut called "emacs" in SendTo folder:
open that with emacs      = {shift+f10}n Wait(100) e;

open [that] in new window = {shift+f10}e;


show properties    = {alt+enter};

copy pathname      = {shift+f10}a;


  # selected file must not already have an extension
make that [a] text file = {f2} {end} .txt {enter};

please delete = {Del} WaitForWindow("Delete F*") HeardWord(click, Yes);



## 
## Other operations:
## 

new folder = {ctrl+shift+n};

open first = {end}{home}{enter};

  # "address" of current directory:
go to address = {alt+d};
copy address  = {alt+d}{ctrl+c} {shift+tab};
