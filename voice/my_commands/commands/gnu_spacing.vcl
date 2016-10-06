### 
### Spacing and capitalization commands for Gnu emacs
### 
### Also has a few special commands for fixing up doubled/dropped characters
### 

include "gnu.vch";
include "string.vch";



### 
### Inserting/removing blank lines:
### 

# 
# these insert N new lines before the current one, leaving the
# cursor on the first new line:
# 
open line [2..20]     = {ctrl+a} Rep($1){ctrl+o};
open line [2..20] tab = {ctrl+a} Rep($1){ctrl+o} {tab};

drop rest [2..20]     =          Rep($1){ctrl+o};


## 
## inserting blank lines at a given point:
## 

<row> <r> open        [2..20] = LineMod($2) Rep($3){ctrl+o};
<row> <r> open wide           = LineMod($2) {ctrl+u}3 {ctrl+o} {down};

<row> <r> open      tab       = LineMod($2) {ctrl+o}                 {tab};
<row> <r> open wide tab       = LineMod($2) {ctrl+u}3 {ctrl+o} {down} {tab};


## 
## Removing blank lines:
## 

  # On blank line, delete all surrounding blank lines, leaving just one.
  # On isolated blank line, delete that one.
  # On nonblank line, delete any immediately following blank lines.
squeeze lines = {ctrl+x}{ctrl+o};



### 
### Fixing/adjusting spacing:
### 

Past(pattern, command) := SaveExcursion(Leap(u, $pattern) $command);


  # useful for my mother's email:
soften hard spaces = {esc}% {ctrl+q}240{enter}{enter} " "{enter};


## 
## Adjusting spaces after periods
## 

double space periods = Do(query-replace-regexp) 
    '\. \([^ {ctrl+q}{ctrl+j}]\)' {enter} '.  \1' {enter};

single space periods = Do(query-replace) '.  ' {enter} '. ' {enter};


## 
## Adding a space:
## 

  # before characters:
separate <prn> [<prn>]	    = Past($1$2, " ");
separate [word] <_anything> = Past(Lower($1), " ");


##
## Simple alignment:
##

PushTo(excursion) := SaveExcursion($excursion
		                   Elisp('(setq mdl-column (current-column))'))
                     Elisp('(indent-to-column mdl-column)');

push under 	<prn> = PushTo({up}	    Leap(D, $1));
push above 	<prn> = PushTo({down}	    Leap(D, $1));

push under last <prn> = PushTo({home}       Leap(u, $1));
push above last <prn> = PushTo({home}{down} Leap(d, $1));


## 
## Removing spaces:
## 

  # kill last N spaces before cursor:
remove space [1..20] = {ctrl+u} When($1,$1,1) {esc}{ctrl+k};

  # same, but after cursor:
remove next space  = SaveExcursion(Leap(D, ' ') {Del});

  # remove all spaces around cursor then add one space before cursor:
squeeze     spaces =          {esc}{space};
  # remove all spaces around cursor:
squeeze all spaces = {ctrl+u}0{esc}{space};

squeeze region     = ActivateMark() {esc}% " "{enter} {enter} !;


  # remove extra space after left delimiters and before right delimiters
  # on current line; also remove extra space before comma or dot.
  # also fixes up simily/frowny faces after filling.
fix spaces         = Do(mdl-fix-spaces);



### 
### Fixing capitalization:
### 

cap-a-letter = LeapRegex(D, '[a-z]') Mark() {right} Do(capitalize-region);

lift up [word] <_anything> = Past(Lower($1), {esc}c);


  # find end of previous sentence (must be on current line before point, prefer
  # ". " falling back to ".") and fix spacing and capitalization:
fix sentence = Do(mdl-fix-sentence);



###
### Fixing up doubled/dropped characters:
###

headless [word] <_anything> = Past(Mid($1, 1), Left($1, 1) Wait(300));
#doubled  [word] <_anything> = Past(Left($1,1) $1, {Del});
