### 
### Global commands for operating on ranges of lines
### 
###    A pale imitation of gnu_range.vcl using Windows key shortcuts.
### 

include "string.vch";
include "numbers.vch";


## 
## The operators available for operating on ranges.
##
##    These operators are run after the given range has been selected (i.e., 
## the range is the current selection); the character '%' denotes that the 
## selection should be exited towards the original cursor position.
## (Unlike the Emacs version, all of these commands move the cursor.)
## 

<op> := (
       highlight    = ""              # select range only
     | copy         = {ctrl+c} %
     | destroy      = {ctrl+x}        # equivalent to cut
);


## 
## Commands for applying operators to different kinds of ranges:
## 

Command(operator, selector, exit) :=
	$selector
	Replace($operator, "%", $exit);

# 
# Basic ranges:
# 

<op>       entire buffer =
     Command    ($1, {ctrl+home}{shift+ctrl+end}, {right});

# 
# ranges relative to the current line:
# 

  # current line (including newline and before point) plus next N-1 lines:
<op> line     [<my0to99>] = 
    Command($1, {home}       {shift+down When($2,_$2)}, {left});

  # next N lines after current line:
<op> next     [<my0to99>] = 
    Command($1, {home}{down} {shift+down When($2,_$2)}, {left});

  # previous N lines before current line:
<op> previous [<my0to99>] = 
    Command($1, {home}       {shift+up   When($2,_$2)}, {right});
