### 
### Voice commands for Chrome (version 48)
### 
###      This file contains website-independent commands; see
###   *_sites.vcl for website-specific commands.
### 

include "locale_PC.vch";
include "chrome.vch";

FixFocus() := Address() UnAddress();

  # this is the triple bar menu at the upper right:
(click file | open menu) = {alt+f};

# switching between frames using the keyboard does not appear to be supported



## 
## Clicking links/gizmos:
## 

Blur() := "{ctrl+shift+,}" Wait(100);


Vim(command) := @ $command;

O() := Variable.Get(chrome:state, "");
H() := Variable.Get(chrome:hint,  "");

SetO(value) := Variable.Set(chrome:state, $value);
SetH(value) := Variable.Set(chrome:hint,  $value);


Action(cbv, vim) := 
    When(O(), 
        When($vim,{esc} Vim($vim) Wait(300),"") H() SetO(""), 
#        {ctrl+shift+space} Wait(1000) H() :$cbv Wait(100) {enter}
        {ctrl+shift+space} Wait(500) H() :$cbv Wait(100) {enter}
    );

Focus() := Action(f, G);


blur me	   = Blur();

show numbers = {ctrl+shift+space} Wait(500) :+{enter};
hide numbers = {ctrl+shift+space} Wait(500) :-{enter};

show hints = Blur() Vim(f) SetO(f);
escape	   = {esc} SetO("");


<pick> 0..9 [0..9 [0..9 [0..9]]] = SetH($2$3$4$5) $1;

<pick> := (        pick = Action("", "")
          | proceed     = Action("", "")

          | push   pick = Action(b, F)     # stay but open new tab w/ link
          | tab    pick = Action(t, T)
          | window pick = Action(w, W)

          | go     pick = Focus()
          | menu   pick = Focus() Wait(200) {shift+f10}
	  | HP     pick = Focus() Wait(500) mark.lillibridge@hpe.com{tab}



          | hover  pick = Action(?, q)
          | unhover  pick = Action(?, Q)
);





<pick_old> := (        pick = ""

          | save   pick = {shift}{shift+f10} Wait(100) k
          | copy   pick = {shift}{shift+f10} Wait(100) a # copy URL of link

          | drop   pick = {shift}{enter}{alt+down}
          );



link <_anything> = Click($1);
#new    link <_anything> = Blur() "'$1" {ctrl+enter};
#window link <_anything> = Blur() "'$1" {shift+enter};



##
## Miscellaneous:
##

open new window = {ctrl+n};

(increase=+ | decrease=-) font = {ctrl+$1};
restore font size = {ctrl+0};
zoom in 0..10  = {ctrl+0} Repeat($1, {ctrl++});

(show|view) [page] source = FixFocus() {ctrl+u};

pane (up={PgUp}|down={PgDn}|top={ctrl+home}) = FixFocus() $1;

force refresh = {ctrl+shift+r};

  # <<<>>>
go    notification = {alt+n};
allow notification = {alt+shift+a};
deny  notification = {alt+shift+d};

# caret browsing requires an extension


(open|show|close) developer tools = {f12};
show help = {f1};
show history = {ctrl+h};
show downloads = {ctrl+j};
focus toolbar = {alt+shift+t};


# <<<>>>
web chrome extensions = OpenURL(chrome://extensions);
