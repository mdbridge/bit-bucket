### 
### Voice commands for Kindle PC reader
### 

include "numbers.vch";
include "string.vch";


  # menus are not accessible even with accessibility plug-in:
(file=f|view=v|go=g|tools=t|help=h) = {alt+$1};

library = {alt+ctrl+l};



## 
## Library shortcuts:
## 

# use arrow keys to select a book

open (that|book) = {ctrl+o};

sort by (most recent=r | date=r | title=t | author=u) = {alt+ctrl+$1};



## 
## Navigating within a book:
## 

# back: move to previous location in book

close book = {ctrl+w};

table of contents = {alt+g}t;


Location(n) := {ctrl+g} {shift+tab}{down} {tab}{down_5} $n{enter};

Number(hundreds, units) := Eval(When($hundreds,$hundreds,0)*100 
		                + When($units,$units,0));

location                    <my0to99>  = Location($1);
location <my0to99> hundred [<my0to99>] = Location(Number($1, $2));


Page(n) := {ctrl+g} $n{enter};

page                    <my0to99>  = Page($1);
page <my0to99> hundred [<my0to99>] = Page(Number($1, $2));


## 
## Viewing a book:
## 

full screen = {f11};

  # toggle:
multiple columns = {shift+tab_3} {space} {tab_3};

(increase=+|decrease=-) font [size] = {ctrl+$1};
