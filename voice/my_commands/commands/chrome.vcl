### 
### Voice commands for Chrome (version 54)
### 
###      This file contains website-independent commands; see
###   *_sites.vcl for website-specific commands.
### 
###   See also chrome_bookmark.vcl
###   See also chrome_developer.vcl
###   See also chrome_cvim.vcl
### 

include "chrome.vch";
include "string.vch"; # <<<>>>

FixFocus() := Address() UnAddress();


  # this is the triple bar menu at the upper right:
(click file | open menu) = {alt+f};



## 
## Navigation:
## 

# 
# Within a page:
# 

# switching between frames using the keyboard does not appear to be supported

       pane (up={PgUp}|down={PgDn}|top={ctrl+home}) = FixFocus() $1;
second pane (up={PgUp}|down={PgDn}|top={ctrl+home}) = Address() {shift+f6} $1;


#
# Other browser components:
#

show help	= {f1};
show history	= {ctrl+h};
show downloads	= {ctrl+j};
show extensions = OpenURL(chrome://extensions);

focus toolbar	= {alt+shift+t};


(show|view) [page] source = FixFocus() {ctrl+u};

[(open|show|close)] developer tools = {f12};



## 
## Clicking links/gizmos:
## 
##   Done via Click-by-Voice Chrome Extension
## 

  # CbV functionality:
Blur()		   := "{ctrl+shift+,}" Wait(100);
CbV(hint, command) :=  {ctrl+shift+space} Wait(500) 
	  	       $hint ":" Wait(100) $command {enter};


SetH(value)  := Variable.Set(chrome:hint,  $value);
H()	     := Variable.Get(chrome:hint,  "");
Action(code) := CbV(H(), $code);

Focus() := Action(f);
Show(parameters) := CbV("", + $parameters);

  # <<<>>>
MoveMouse() := Clipboard.Set("xyzzy") Action("X")  Clipboard.WaitForNew("xyzzy",5)
    Mouse.Go(window,  Split(Clipboard.Get(),',',0), Split(Clipboard.Get(),',',1));
	      

blur me	   = Blur();


<show> <kind> numbers = Show(CE3 $2 $1);
<show>  numbers	      = Show(       $1);

<show> := ( show="" | show contrast="c" | show all="+c" );

<kind> := (
      image	     = oc\$img
    | tooltip	     = 'oc\$[title]'

    | inline	     = i
    | overlay	     = o
    | hybrid	     = h

    | start overlay  = os
    | start hybrid   = hs

       # dealing with overflowing text:
    | before  hybrid = h
    | overlay hybrid = h>
    | risky   hybrid = h.

    | old	     = '#'



);

New() := C oE3?;
Old() := C oE3;

<show> <kind2> numbers = Show($2 $1);

<kind2> := (
      test	     = New()
    | control	     = Old()
    | Delta	     = New() Vocola.Abort()

        # hack for Facebook comments; must hit, not go pick this
    | comment = ':+oc$.UFIInputContainer > div[class!=UFICommentAttachmentButtons]'

    | re-edit = ":+i\$a.title, .next-button a, .prev-button a, a.comments"
);


inspect events = SetH(2) Action(INSPECT);




# refresh numbers?

hide numbers = CbV("", "-");


<pick> 0..9 [0..9 [0..9 [0..9]]] = SetH($2$3$4$5) $1;

             # default action:
<pick> := (        pick = Action("") 
          | proceed     = Action("") # better recognized than just pick 

	      # clicking:
          | hit    pick = Action("c")

	      # a href links only:
          | push   pick = Action(b)     # stay but open new tab w/ link
          | tab    pick = Action(t)
          | window pick = Action(w)
          | copy   pick = Action(k)     # copy URL of link

	      # focusing:
          | go     pick = Focus()

              # hovering:
          | hover  pick = Action("h")

              # derived actions:
          | drop   pick = Focus() Wait(500) {alt+down}
          | menu   pick = Focus() Wait(200) {shift+f10}
	  | HP     pick = Focus() Wait(500) mark.lillibridge@hpe.com{tab}


  # experiments <<<>>>
          | old pick    = Action("C")
          | new pick    = Action("CC")
          | debug pick    = Action("D")

          | move pick   = MoveMouse() 
          | force pick  = MoveMouse() Mouse.Click() 
	                  Wait(300)  Mouse.Click(interior,610,50)
          | staying pick  = MoveMouse() Mouse.Click() 
);

  # to be implemented...  <<<>>>
<pick_old> := (
            save   pick = {shift}{shift+f10} Wait(100) k
          );

  # experimental <<<>>>
facebook drag 0..9 [0..9 [0..9 [0..9]]] through
              0..9 [0..9 [0..9 [0..9]]] 
    = SetH($1$2$3$4) Action(>) Wait(200)
      SetH($5$6$7$8) Action(<);


link <_anything> = Click($1);
#new    link <_anything> = Blur() "'$1" {ctrl+enter};
#window link <_anything> = Blur() "'$1" {shift+enter};



##
## Miscellaneous:
##

open new window = {ctrl+n};

(increase=+ | decrease=-) font [size] = {ctrl+$1};
(restore|normal) font size = {ctrl+0};
zoom in 0..10  = {ctrl+0} Repeat($1, {ctrl++});

force refresh = {ctrl+shift+r};


  # notifications that occur from the system tray <<<>>>
go    notification = {alt+n};
allow notification = {alt+shift+a};
deny  notification = {alt+shift+d};


# caret browsing requires an extension
