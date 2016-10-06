###
### Voice commands for Microsoft Word 2003
###

include "string.vch";
include "numbers.vch";


File save as = {alt+f}a;

# window: show list of numbered documents

  # switch to workbook with Word assigned number (see Window menu)
window 1..10 = {alt+w} $1;



#
#  jump to absolute page number in current Word document:
#
GoPage(page) := {ctrl+g} {alt+o}p {alt+e} $page {enter}{esc};

go to page <my0to99>                   = GoPage($1);
go to page <my0to99> hundred <my0to99> = GoPage($1 Right("00$2",2));


really subscript = {alt+o}f{alt+b}{enter};

force subscript  = {alt+o}f{alt+b}{enter} {left}{Del};



##
## Mark Lillibridge's leap commands:
##

include "leap_word.vch";
include "leap.vch";




set bookmark 1..9 = SendSystemKeys("{Alt+i}")  Wait(100) k Wait(100) 
		     "natlink_$1"  {Alt+a} ;

go to bookmark 1..9 = SendSystemKeys("{Alt+i}")  Wait(100) k Wait(100) 
		     "natlink_$1"  {Alt+g}{esc} ;



Row(row) := {ctrl+g} {alt+o}p {alt+e} +0             {enter} {esc}
            {ctrl+g} {alt+o}l {alt+e} + Eval($row-1) {enter} {esc};

my row <my0to99> = Row($1);

my top = {ctrl+g} {alt+o}p {alt+e} +0             {enter} {esc};
my advance <my0to99> = {ctrl+g} {alt+o}l {alt+e} + Eval($1-1) {enter} {esc};
