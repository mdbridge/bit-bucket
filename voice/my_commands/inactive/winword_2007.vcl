###
### Voice commands for Microsoft Word 2007
###

include "string.vch";
include "letters.vch";
include "numbers.vch";

include "winword.vch";


# office button

File save as = {alt+f}a;



### 
### Navigation:
### 

## 
## Switch to a currently open document: 
## 

  # show list of numbered documents:
window = {alt+w}w;

  # switch to document with above number:
window 1..10 = {esc_4} {alt+w}w $1;


##
##  jump to absolute page number in current Word document:
##

# page #: try this built in!  adequate replacement?  <<<>>>


GoPage(page) := {ctrl+g} WaitForWindow("Find and Replace")
	     	{alt+o}p {alt+e} $page {enter}{esc};

go to page <my0to99> [hundred <my0to99>] = GoPage($1 When($2,Right("00$2",2)));
(clear|cut) page <my0to99> = Beep(); # prevent common misrecognition of go to page


[go to] page <my0to99>           <row> <r> = GoPage($1) Line($3);




## 
## Bookmarks:
## 

  # uses 2003 compatibility mode keys:
SetBookmark(n) := SendSystemKeys({Alt+i})  Wait(100) k Wait(100) 
		  natlink_$n  {Alt+a} ;
 GoBookmark(n) := SendSystemKeys({Alt+i})  Wait(100) k Wait(100) 
		  natlink_$n  {Alt+g}{esc} ;


drop crumb        = SetBookmark(crumb);
find crumb        =  GoBookmark(crumb);

set bookmark 1..9 = SetBookmark($1);
 go bookmark 1..9 =  GoBookmark($1);


##
## Leap commands:
##

include "leap4_long.vch";



##
## Changing case:
##

  # Unlike the Emacs version, this does not skip non-letters
  # before capitalizing:
cap-a-letter = {shift+right} {alt+h}7 U {right};



### 
### Other:
### 

# select line
# select {previous,next} # lines         # starts with current line


## 
## Showing line numbers:
## 
##    (only visible with print layout)
## 

(show=r|hide=n) line numbers = {alt+p}ln $1;



## 
## Changing paragraphs (just need to be in paragraph):
## 

# {indent,unindent,out dent} paragraph 

pick bullet = {alt+h}u;



## 
## Changing text properties:
## 

font size 5..70 = {alt+h}fs $1{enter};

# make that {italics,normal,red}

   # these are toggles:
make that subscript   = '{ctrl+=}';
make that superscript = {ctrl++};



## 
## Non-ASCII characters:
## 

insert medium dash = {ctrl+NumKey-};
insert long   dash = {ctrl+alt+NumKey-};


## 
## Tracking changes:
## 

show (final=ff| final showing markup=f|markup=f) = {alt+r} td o$1 {enter};


## 
## Miscellaneous:
## 

search and replace = {ctrl+f} WaitForWindow("Find and Replace") {alt+p};

Sub(normal, subscript) := $normal $subscript {shift+left_ Len( $subscript) } 
	    	          '{ctrl+=}' {right} '{ctrl+=}';

[lower] <letter> sub 0..9 = Sub($1,         $2);
upper   <letter> sub 0..9 = Sub({shift+$1}, $2);
