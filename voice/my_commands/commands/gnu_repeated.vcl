### 
### Gnuemacs commands for repeatedly editing text
### 
###   includes keyboard macros, search and replace
### 

include "letters.vch";

include "gnu.vch";



## 
## Search and replace, with querying each replacement
## 
##     Note you can restrict search and replace to a region by using
## an active region (see "highlight <range>") or by using "narrow"
## first then "widen" afterwards.
## 
##     You can also edit the results of an occur buffer (e.g.,
## teleport results) in version 24+ via 'e'.
## 

Save() := SavePoint(ScratchRegister());

      search-and-replace = Save() {esc}%;

  # use \& in replacement for entire match, \N for Nth \(...\) group:
regex search-and-replace = Save() Do(query-replace-regexp);

  # answers to should I do this replacement question:
replace all              = !;

  # exit with point at start of search and replace:
done replacing           = {enter} RestorePoint(ScratchRegister());


# 
# Shortcuts:
# 

replace <prn> with nothing     = Save() {esc}% $1{enter}          {enter};
replace <prn> with <prn>       = Save() {esc}% $1{enter}        $2{enter};

  # these start search from start of current line, not point so can
  # copy text then replace it:
replace clipboard              = Save() {home} {esc}% {ctrl+y}{enter};
replace clipboard with nothing = Save() {home} {esc}% {ctrl+y}{enter} {enter};


# 
# Regex search and replace across multiple files:
# 

  # do a regexp search and replace on all files in tags table:
tags  search-and-replace = Do(tags-query-replace);

  # do a regexp search and replace on all marked files in dired mode:
Mondo search-and-replace      = Q Empty();
Mondo search-and-replace word = Q \<\> {left_2} Empty();

  # 'next match' can be be used to resume a search and replace



##
## Keyboard macros:
##

# 
# Basic macro creation and running:
# 

  # defining a new keyboard macro:
start macro = "{ctrl+x}(";
end   macro = "{ctrl+x})";

  # run last/being defined keyboard macro:
run   macro [<my0to99> times] = When($1,{ctrl+u}$1) {ctrl+x}e;
  # repeating a macro 0 times means repeat until error:
run   macro forever           =          {ctrl+u}0  {ctrl+x}e;

# also run macro <range>, but it does *not* end macro first...


# 
# Macro counters:
# 

  # use the first time:
macro counter       = {ctrl+x}{ctrl+k}{ctrl+i}; # increments afterward...
#  # use the second time:
#  # same as last counter inserted (not current value); does not increment
#macro counter again = {ctrl+u}{ctrl+x}{ctrl+k}{ctrl+i};

  # the counter starts with 0 by deflaut:
macro set counter   = {ctrl+x}{ctrl+k}{ctrl+c};

#  # set during macro or before it's defined; default: %d
#  # use %02d for two digit padded numbers (e.g., 03)
#macro set format    = {ctrl+x}{ctrl+k}{ctrl+f};


# 
# Allowing user control of each iteration:
# 

#  # let user skip iteration or do a recursive edit:
#macro query               =           {ctrl+x}q;
#  # go straight to a recursive edit:
#macro      recursive edit = {ctrl+u}1 {ctrl+x}q;

(end|exit) recursive edit = {esc}{ctrl+c};


# 
# Naming, saving, and restoring macros:
# 

name macro [<letter>] = Do(name-last-kbd-macro) When($1,mdl-macro-$1 {enter});

run macro <letter>    = Do(mdl-macro-$1);

  # these produce elisp code:
insert macro <letter> here = Do(insert-kbd-macro)     mdl-macro-$1 {enter};
insert named macro    here = Do(insert-kbd-macro);

#edit macro <letter>        = Do(edit-named-kbd-macro) mdl-macro-$1 {enter};
#edit named macro           = Do(edit-named-kbd-macro);



## 
## Perform the same edits on each line in a range paradigm:
## 

#
# run last defined macro once per line in current region, starting from
# the beginning of each line:
#
#   run macro <range specification>
#
# defined in gnu_ranges.vcl
#


# 
# Conciser version:
#   edit row <n> [last]
#     {edit that line using commands that will work on each line}
#   same edit <r>,<r'>     or   same edit next <n> 
#   [repeat above as needed]
# 

edit <row> <r>           = LineMod($2) "{ctrl+x}("      ;
edit <row> <r> last      = LineMod($2) "{ctrl+x}(" {end};

same edit next <my0to99> =
	"{ctrl+x})" 
	{down}{home} Mark() {ctrl+n_$1}
	Do(apply-macro-to-region-lines)
  	{up};  # allow repeating this command with no gaps

same edit <r> comma <r>  =
        "{ctrl+x})" 
        LineMod($1) Mark() LineMod($2)
        Do(apply-macro-to-region-lines);



## 
## Using this paradigm as an implementation trick for voice commands:
## 
##   each command created using this requires from < to
## 

  #
  # run $command once from the start of each line in ranges of lines
  # [$from, $to] where line $from is *before* line $to where lines are
  # denoted as usual mod 100.
  #
  # side effect: defines a keyboard macro  (avoid via macro kill ring?) <<<>>>
  #
EachLine(command, from, to) :=
	SaveExcursion(
          LineMod($from) 
  	  "{ctrl+x}(" $command "{ctrl+x})"
  	  {down}{home} Mark()
  	  LineMod($to) 
  	  Do(apply-macro-to-region-lines)
	);



## 
## Deleting lines matching a regular expression:
## 

  # affects active region or point onwards
flush lines = Do(flush-lines);



## 
## Registers:
## 

  # save text to a register:
name region    <letter> =                 CopyToRegister($1) 
                          Elisp('(deactivate-mark)');
name clipboard <letter> = Elisp( {ctrl+y} CopyToRegister($1) {ctrl+w}  t);

insert         <letter> = YankFromRegister($1);
