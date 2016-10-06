### 
### Voice commands for Acrobat Pro
### 

#include "printers.vch";
#
#SetPrinter(printer) := WaitForWindow(Print) {alt+n}{alt+up} $printer {enter};
#
#  # override of global command in _dialogue.vcl:
#set print [<printer_name>]     = SetPrinter(When($1,$1,Vocola.Abort()));
#
#print that [on <printer_name>] = {alt+f}p When($1, SetPrinter($1));
#
#
## 
## Deal with stupid reading order dialog box:
## 
##leap down       = SendSystemKeys({alt+c}) Wait(100) 
##		  SendSystemKeys({alt+c}) {PgDn}   ;
##leap down 1..20 = SendSystemKeys({alt+c}) {alt+c} {PgDn_$1};
#
#

Page() := {alt+v}np;  # {ctrl+shift+n} not working for some reason

[go to] page               1..99  = Page() $1 {enter};
[go to] page 1..9 hundred [0..99] = Page() Eval($1*100 + When($2,$2,0)) {enter};

fit (page={ctrl+0} | size={ctrl+1} | width={ctrl+2}) = $1;

fit page 1..99 = {ctrl+0} Page() $1 {enter};


#(close|hide) bookmarks [pane] = {alt+v} Wait(100) {down_7}{right} nb;
#
#rotate = {ctrl++};
