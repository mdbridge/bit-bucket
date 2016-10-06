###
### Basic voice commands for Microsoft Excel 2013
###
###   commands following '#' without a definition are (sample) Dragon
###   built-in commands
###

include "office.vch";

include "numbers.vch";
include "string.vch";
include "optional.vch";


Ribbon(key) := {alt+$key};

File()      := Ribbon(f);
Home()      := Ribbon(h);
Insert()    := Ribbon(n);
View()      := Ribbon(w);


  # missing from Dragon for some reason:
Click File   = File();



## 
## Switch to a currently open workbook: 
## 

next     (workbook|window) = {ctrl+tab};
previous (workbook|window) = {ctrl+shift+tab};

  # show list of numbered workbooks:
window       = {alt+w}w;

  # switch to workbook with above number:
window 1..10 = {alt+w}w $1;



##
## Entire workbook commands:
##

# new   workbook
# close workbook

save file    = {ctrl+s};

File open    = {ctrl+f12};
File save as = {f12};

  # from save as dialog:
use workbook format = {alt+t} {alt+down}{home}{enter} {alt+n};

# use shift tab 2 to move to file selection pane



## 
## Switching between worksheets:
## 

# next sheet, previous sheet
previous sheet 2..10 = {ctrl+PgUp_$1};
next     sheet 2..10 = {ctrl+PgDn_$1};

  # assuming at max 20 sheets:
first sheet       = {ctrl+PgUp_20};
last  sheet       = {ctrl+PgDn_20};
sheet       1..10 = {ctrl+PgUp_20} "{ctrl+PgDn " Eval($1 - 1) };
sheet minus 1..10 = {ctrl+PgDn_20} "{ctrl+PgUp " Eval($1 - 1) };



## 
## Worksheet commands:
## 

# {rename,insert,delete,copy} sheet


AtName(keys) := HeardWord(rename, sheet) Wait(100) $keys;

copy sheet (name="" | and name=HeardWord(copy, sheet)) =
	AtName({ctrl+c}{esc}) 
	Variable.Set(Excel:name, Clipboard.Get()) $1;

paste sheet (name="" | and name={ctrl+v} {right}{left}) =
	AtName(Variable.Get(Excel:name) {enter}) $1;


move   sheet      = {alt+h}o m;
move   sheet last = {alt+h}o m Wait(100) {down_20}{enter};

insert sheet last = HeardWord(insert, sheet) Wait(100)
                    {alt+h}o m Wait(100) {down_20}{enter};


Freeze() := View()f Wait(100) {enter};
   freeze panes = Freeze();            # built-in broken in DNS 11
re-freeze panes = Freeze() Freeze();



## 
## Navigation within worksheets:
## 

top    left  = {ctrl+g}a1{enter};
bottom right = {ctrl+end};


# top of column, beginning of row
# row 14, column c, cell c 14

# [go to] cell a 104
# [go to] cell foxtrot Zulu 11       (need code for two letter cell names)

# page [up/down/left/right]



##
## Selecting cells:
##

# select cell c 4
# select row 13
# select column k

# select cell a 1 through cell c 4
# select row 5 through row 6
# select column a through column b

# select sheet

# above commands also work with "copy", & "cut" instead of "select"


<rect> := ( rectangle="" | here rectangle=Mouse.Click() );
<d>    := ( right | left | up | soar=up | down );
<ed>   := ( right=*right | left=*left | up=*up | soar=*up | down=*down 
          | column=ctrl+space | row=shift+space);
<op>   := ( copy={ctrl+c} );

  # without count, select by "group"; with count, select by individual cells:
Dir(ed,count) := { When($count, Replace($ed,"*",shift+)      _$count,
	                        Replace($ed,"*",ctrl+shift+)          ) };


<rect>                                       [<op>] = $1 {ctrl+shift+*} $2;
<rect> <ed> [<my1to100>] [<ed> [<my1to100>]] [<op>] = 
    $1 Dir($2,$3) When($4,Dir($4,$5)) $6;

# can immediately repeat one of these commands to extend selection


cell <d> = {ctrl+$1};  # move to rectangle edge without selecting



##
## Pasting commands:
##

here copy  =     Mouse.Click() {ctrl+c};
here paste =     Mouse.Click() {ctrl+v};

stub that  = '=' Mouse.Click() {enter};


paste (values=v | values and [number] formatting=a | stubs=n
      |values transposed=s v e{enter}) = Home()v $1;

  #  paste multiple times, moving down in between:
paste 2..10 times       = Repeat($1, {ctrl+v}{down});



## 
## Filling:
## 

  # if selection height > 1, fill all but first row of selection with
  # first row of selection; otherwise, fill using row before selection:
fill down  selection =                  {ctrl+d} {down};
fill down  1..50     = {shift+down_$1}  {ctrl+d} {down_$1};

  # similar but with width and columns:
fill right selection =                  {ctrl+r} {right};
fill right 1..50     = {shift+right_$1} {ctrl+r} {right_$1};



## 
## Editing cells:
## 

# 
# There are at least two modes, edit and enter (see lower left corner
# for mode name).  In enter mode for formulas, arrow keys select a
# cell (range if shifted), whereas in edit mode, they move the cursor
# inside the formula.  For non-formulas, in enter mode, the arrows
# finished entering the current cell and move to another cell.
# 
# "formula bar" and "edit cell" initially switch to edit mode then
# toggle thereafter for the given cell.  Typing initially puts you in
# enter mode.
#

# edit cell: always edits directly in cell (blue background)

  #
  # this has the effect of pressing F2 without DNS around.
  #
  # Want "edit directly in cell" option turned off:
  #   Office button->advanced->!allow editing directly in cells
  # (Dragon handles edit in cell directly badly)
  #
  # First time, edits current cell via formula bar.  Unlike with
  # editing directly in a cell, this highlights ranges and cells used.
  #
formula bar = SetMicrophone(0) Wait(100) {F2} SetMicrophone(1);


append clipboard = {f2} Wait(100) {ctrl+v} {f2} {right};



##
## Creating formulas:
##

# average cell A1 through cell C2
# add     cell A1 through cell D2


# 
# follow with things like 'touch key /' then 'touch slap' or
# 'left 10 key plus left 9'
#
# use "formula bar" to edit normally (e.g., insert $'s)
#
formula = '=';



##
## Inserting/deleting/(un)hiding rows/columns:
##

# insert three columns

insert [1..20] rows    = REPEAT($1, {alt+i}r);
insert [1..20] columns = REPEAT($1, {alt+i}c);

# delete row

# [un]hide [that] {column|row}, unhide row two



## 
## Printing sheets or charts:
## 

include "printers.vch";

<thing> := (workbook={down_1} | sheet={down_0} | selection={down_2}
           |chart="" | that={down_1});

Want(thing) := {alt+p}a {PgUp} $thing{enter};


  # need press page down or leap down to move in result:
preview <thing>			     = {ctrl+p} Wait(1000) Want($1) {alt+p}v;
print   <thing> [<printer_position>] = {ctrl+p} Wait(1000) Want($1) When($2,SetPrinter($2))
                              	       {alt+p}p;


  # from preview display:
show margins dialog = {alt+p}ma;

  # from dialog box produced by above: not actually useful for charts :-(
kill margins        = {alt+t}0 {alt+l}0 {alt+r}0 {alt+b}0;



## 
## Misc.:
## 

  # current selection:
auto fit that =          {alt+o}ca;
  # current rectangle or so:
auto fit      = {ctrl+a} {alt+o}ca {right}{left};

show formulas = {ctrl+`};  # toggle formulas vs. values in all cells



## 
## Changing text properties:
## 

  # set size to <n> built-in fails for chart parts;
  # this only works on entire text part (e.g., not part of a word)
font size 1..40 = {f10}h fs $1 {enter};



## 
## Specific task macros:
## 

#do it 1..9 times = Repeat($1, {f2} {home} "through " {f2} {down});
#do it 1..9 times = Repeat($1, {f2} {end} " [I]" {f2} {down});

  # for my accounting spreadsheet; copy some cells in previous year then...
convey that = {right}{ctrl+v}{left} {ctrl+down_2} {ctrl+shift+down}{ctrl+c};
