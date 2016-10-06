###
### Basic voice commands for Microsoft Excel 2003
###
###   commands following '#' without a definition are (sample) Dragon
###   built-in commands
###

include "numbers.vch";
include "string.vch";


##
## Workbook commands:
##

# new   workbook
# close workbook

save file    = {ctrl+s};
File save as = {alt+f}a;


<file> := (all = out_all | 128 = out_128 | hook = out_hook 
          | out | foo | bar | data | summary);

open <file> = {alt+f}o $1.csv {enter};


next     workbook = {ctrl+tab};
previous workbook = {ctrl+shift+tab};

# window: show list of numbered workbooks

  # switch to workbook with Excel assigned number (see Window menu)
window 1..10 = {alt+w} $1;



## 
## Worksheet commands:
## 

# next sheet, previous sheet
previous sheet 2..10 = {ctrl+PgUp_$1};
next     sheet 2..10 = {ctrl+PgDn_$1};

  # kludge <<<>>>
first sheet = {ctrl+PgUp_20};
last  sheet = {ctrl+PgDn_20};
sheet 1..10 = {ctrl+PgUp_20} "{ctrl+PgDn " Eval($1 - 1) };


# {rename,insert,delete} sheet

# freeze panes
re-freeze panes = {alt+w}f {alt+w}f;



## 
## Navigation within worksheets:
## 

#top    left  = {ctrl+home};
top    left  = {ctrl+g}a1{enter};
bottom right = {ctrl+end};

# top of column, beginning of row
# row 14, column c, cell c 14

# go to cell a 104
# go to cell foxtrot Zulu 11       (need code for two letter cell names)

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

# select sheet, copy sheet


current (region|area) = {ctrl+shift+*};
copy    (region|area) = {ctrl+shift+*} {ctrl+c};



  # select a X [by Y] rectangle of cells from top left corner:
  # does not work for (1, 1)
Rectangle(X, Y) := "{shift+right " Eval($X - 1) }
                   "{shift+down "  Eval($Y - 1) };


rectangle <my1to100>                    = Rectangle($1, 1 );
rectangle <my1to100> by <my1to100>      = Rectangle($1, $2);

rectangle <my1to100>               copy = Rectangle($1, 1 ) {ctrl+c};
rectangle <my1to100> by <my1to100> copy = Rectangle($1, $2) {ctrl+c};


<edge> := ( right = {ctrl+shift+right} | lower = {ctrl+shift+down}
          | lower right = {ctrl+shift+right}{ctrl+shift+down});

rectangle <edge> edge      = $1;
rectangle <edge> edge copy = $1 {ctrl+c};



##
## Pasting commands:
##

paste values            = {alt+e}s v{enter};
paste values transposed = {alt+e}s v e{enter};

paste stubs             = {alt+e}s l;

  #  paste multiple times, moving down in between:
paste 2..10 times       = Repeat($1, {ctrl+v}{down});



## 
## Filling:
## 

  # if selection height > 1, fill all but first row of selection with
  # first row of selection; otherwise, fill using row before selection:
fill down  selection =                  {ctrl+d} {down};
fill down  1..10     = {shift+down_$1}  {ctrl+d} {down};

  # similar but with width and columns:
fill right selection =                  {ctrl+r} {right};
fill right 1..10     = {shift+right_$1} {ctrl+r} {right};



##
## Creating formulas:
##

# average cell A1 through cell C2
# add cell A1 through cell D2



##
## Inserting/deleting/(un)hiding rows/columns:
##

insert row          = {alt+i}r;
insert 1..20 rows   = Repeat($1, {alt+i}r);

# insert three columns
insert column          = {alt+i}c;
insert 1..20 columns   = Repeat($1, {alt+i}c);


# delete row

# hide that {column|row}



## 
## Misc.:
## 

auto fit that = {alt+o}ca;
auto fit      = {ctrl+a} {alt+o}ca {right}{left};


# edit cell



## 
## Specific task macros:
## 

ready import = {ctrl+a} {alt+o}ca {right}{left}  # auto fit
               {ctrl+shift+*} {ctrl+c}           # copy region
               {alt+w};                          # Window

#do it 1..9 times = Repeat($1, {f2} {home} "through " {f2} {down});
do it 1..9 times = Repeat($1, {f2} {end} " segment size" {f2} {right});

hit me = {ctrl+home} {ctrl+a} {alt+o}ca {alt+d}ff {right};
