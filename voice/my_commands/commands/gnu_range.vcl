### 
### Gnuemacs commands for operating on ranges of lines
### 

include "gnu.vch";

include "switch.vch";
include "optional.vch";


## 
## The operators available for operating on ranges.
##
##    These operators are run after the given range has been selected
## (i.e., range is between cursor and mark); the command Ret() denotes
## that the cursor should be returned to where the command was issued.
## Thus, {esc}w Ret() {ctrl+y} copies the contents of the range
## specified to the point where the command was issued.
## 

Ret() := Variable.Get(_range_exit);

  # only align exp's where preceeded by at least 1 space:
LooseAlign(exp) := 
	{ctrl+u} Do(align-regexp) EraseToStart() '\(\s-+\)' $exp {enter}
	{enter_2} n;

Join(replacement) := ActivateMark() Do(replace-string) 
                        {ctrl+q}{ctrl+j} {enter} $replacement {enter};

<op> := (
   #
   # primitive operators
   #
         # select range only, activating the region (moves point)
       highlight          = ActivateMark()
     | copy               = {esc}w   Ret()
     | destroy            = {ctrl+w} Ret()  # equivalent to cut

   #
   # yanking: copying range to (before line containing) point
   #
     | yank               = {esc}w   Ret() {home}         {ctrl+y}

   #
   # grabbing: moving range to before line containing point
   #
     | grab               = {ctrl+w} Ret() {home}         {ctrl+y}

   # 
   # replacing: replace range with a blank line (moves point)
   # 
     | replace            = {ctrl+w}       {home} {ctrl+o}

   #
   # yanking/grabbing to crumb (advances crumb):
   #
     | crumb yank         = {esc}w   RestorePoint(0) {ctrl+y} SavePoint(0) Ret()
     | crumb grab         = {ctrl+w} RestorePoint(0) {ctrl+y} SavePoint(0) Ret()

   #
   # paste range to blue area window via {shift+Ins}: <<<>>>
   #
     | blue area paste    = {esc}w Ret() GoArea(blue) {shift+Ins}
     | blue area dump     = {esc}w Ret() GoArea(blue) {shift+Ins} OldWindow()

   #
   # aligning:
   #
     | align              = Do(align-regexp)             
     | complex align      = {ctrl+u} Do(align-regexp)             
     | align equals       = LooseAlign('=')			 Ret()
     | align tight equals = Do(align-regexp) '='   {enter}	 Ret()
     | align bar          = Do(align-regexp) '|'   {enter}	 Ret()
     | align pound        = Do(align-regexp) '#'   {enter}	 Ret()
     | align colon        = Do(align-regexp) ':'   {enter}	 Ret()
     | align underscore   = LooseAlign("_")			 Ret()
     | align slash        = Do(align-regexp) '/'   {enter}	 Ret()

   #
   # indenting:
   #
     | indent             = {esc}{ctrl+\}                        Ret()
     | quad indent        = Do(mdl-insert-string) "    " {enter} Ret()
     | message indent     = Do(mdl-insert-string) "> "   {enter} Ret() 

   #
   # joining lines:
   #
     | join               = Join(" ")                            Ret()
     | make list          = Join(", ")                           Ret()

   # 
   # other operators:
   # 
     | justify            = Do(fill-region)                      Ret()
     | comment            = Do(comment-region)                   Ret()
     | uncomment          = Do(uncomment-region)                 Ret()
     | run macro          = Do(apply-macro-to-region-lines)      Ret()
     | evaluate           = Do(eval-region)                      Ret()
     | shell command      =         '{esc}|'
     | shell transform    = '{ctrl+u}{esc}|'  # example: sort
     | sort               = Do(sort-lines)

   # experiment: <<<>>>
     | toggle             = ToggleRegion() Ret()
     | vortex             = {esc}w HeardWord(vortex, load, clipboard)
);


## 
## Commands for applying operators to different kinds of ranges:
## 
##   "other" denotes a range in the "other" window; this only produces
##   coherent results when there are only two windows.
##

Command     (operator, selector) :=
	SavePoint(ScratchRegister2())
	Variable.Set(_range_exit, RestorePoint(ScratchRegister2()))
	$selector $operator;

CommandOther(operator, selector) :=
	{ctrl+x}o SavePoint(ScratchRegister2())
	Variable.Set(_range_exit, RestorePoint(ScratchRegister2())
	           	          {ctrl+u}-1{ctrl+x}o)
	$selector $operator;


# 
# Basic ranges:
# 

  # [start row, end row)
<op>       <r> comma <r> =
    Command     ($1, LineMod($2) Mark()  LineMod($3));
<op> other <r> comma <r> = 
    CommandOther($1, LineMod($2) Mark()  LineMod($3));


  # [start row, end of buffer]
<op>       <r> onwards  = 
    Command     ($1, LineMod($2) {esc}>);
<op> other <r> onwards  = 
    CommandOther($1, LineMod($2) {esc}>);

  # [start of buffer, start row)
<op>       <r> backwards  = 
    Command     ($1, LineMod($2) {esc}< {ctrl+x}{ctrl+x});
<op> other <r> backwards  = 
    CommandOther($1, LineMod($2) {esc}< {ctrl+x}{ctrl+x});


<op>       entire buffer =
     Command    ($1, {esc}<      {esc}>);


# 
# Singleton ranges:
# 

<op>        single  <r> = Command     ($1, LineMod($2) Mark() {ctrl+n});
<op> other [single] <r> = CommandOther($1, LineMod($2) Mark() {ctrl+n});


# 
# ranges relative to the current line:
# 

  # current line (including newline and before point) plus next N-1 lines:
<op> line     [<my0to99>] = Command($1, {home}       Mark() Rep($2){ctrl+n});
  # next N lines after current line:
<op> next     [<my0to99>] = Command($1, {home}{down} Mark() Rep($2){ctrl+n});
  # previous N lines before current line:
<op> previous [<my0to99>] = Command($1, {home}       Mark() Rep($2){ctrl+p});

# 
# ranges too big to fit on the screen:
# 

  # point to previously dropped crumb:  
<op> to crumb = Command($1, Mark() RestorePoint(0));


# 
# Other ranges:
# 

  # treat currently selected region (e.g., from point to mark) as a range:
<op> region             = Command($1, "");


# 
# Experimental ranges:  <<<>>>
# 

  # avoid confusion with indent paragraph, flee paragraph...
<op> current paragraph = Command($1,             {esc}h {ctrl+x}{ctrl+x});
<op> paragraph <r>     = Command($1, LineMod($2) {esc}h {ctrl+x}{ctrl+x});



## 
## These commands can be used to manually limit things like run macro
## forever to a region:
## 

narrow (region|buffer) = Do(narrow-to-region);
widen  (region|buffer) = Do(widen);



## 
## These operations are hardcoded to a limited set of ranges because
## they require parameters:
## 

  # [start row, end row)
CommandBasic  (operator, from, to) :=
    Command     ($operator, LineMod($from) Mark()  LineMod($to));
  # [start row, end of buffer]
CommandOnwards(operator, from) :=
    Command     ($operator, LineMod($from) {esc}>);


# 
# Inserting text at the beginning of lines:
# 

Insert(string) := Do(mdl-insert-string) $string {enter} Ret();

indent <r> comma <r> by 1..20   = CommandBasic  (Insert(Repeat($3," ")), $1, $2);
indent <r> onwards   by 1..20   = CommandOnwards(Insert(Repeat($2," ")), $1);


prefix <r> comma <r> with <prn> = CommandBasic  (Insert($3), $1, $2);
prefix <r> onwards   with <prn> = CommandOnwards(Insert($2), $1);


# 
# Removing a fixed number of characters from line beginnings:
# 

  # These are incremented via delete-rectangle; first line must have at least
  # the number of characters to delete; other lines need not.
  # first command below must be top line then bottomline:
outdent <r> comma <r> by 1..20 = CommandBasic  ({up} Exchange() 
                                                {right_$3} {ctrl+x}rd, $1, $2);
outdent <r> onwards   by 1..20 = CommandOnwards(     Exchange()
						{right_$2} {ctrl+x}rd, $1);


## 
## Degenerate cases:
## 

# 
# Duplicating the current line [N times]:
# 

Duplicate(count) := {home} Mark() {end}{esc}w 
                    REPEAT($count, {down}{home}{ctrl+o} {ctrl+y}{home});

duplicate line [2..20] = Duplicate($1);


# 
# Swapping lines:
# 

  # swaps line containing point with line preceding it then goes to
  # next line:
(swap|transpose) lines = {ctrl+x}{ctrl+t};

# other direction is "grab next"
