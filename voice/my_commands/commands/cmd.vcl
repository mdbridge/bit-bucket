###
### Commands for Windows command shell
###

include "locale_PC.vch";
include "string.vch";


## 
## General shell commands:
## 

Empty() := "";

include "any_shell.vch";


## 
## Commands for navigating around filesystems
## 

UP(count)      := 'cd .' Repeat($count, UNIX(/..)) {enter};

DOWN(pathname) := 'cd "$pathname"{enter}';

CD(pathname)   := DOWN($pathname)   
                  Left($pathname, 2) {enter};   # change drive

include "navigate.vch";


(Explorer | folder) here = "start .{enter}";

TYPE(pathname) := $pathname;

type directory [<D>] <UNIX> [/ <COM>] = TYPE(UNIX($1$2 When($3,/$3)));
type directory       <PC>   [/ <COM>] = TYPE(PC(    $1 When($2,/$2)));



##
## Cutting and pasting  (requires special menu options):
##

# works best if QuickEdit mode is turned on (then left mouse select works)

copy  that        = {alt+space}ey;
(paste that|yank) = {alt+space}ep;

  # copy then paste:
#command prompt yank = {alt+space}ey {alt+space}ep;

  # after, select text using (shift) arrows then enter or copy that to copy:
keyboard select = {alt+space}ek;


## 
## Scrolling:
## 

page (up=PgUp|down=PgDn) [2..20] = {alt+space}el {$1 When($2,_$2)} {enter};


##
## Directory listing commands
##

show directory = "dir{enter}";


## 
## Simple process control:
## 

  # stop currently running batch job:
stop batch job = {ctrl+c} Wait(1000) y{enter};


## 
## Command history:
## 

# F8: find command from history starting with text before cursor

  # after, exit with escape or arrows then enter to pick a command
show history = {f7};


## 
## Macros for miscellaneous commands:
## 

PSFTP(machine) := '"' PF32() '\PuTTY\psftp" $machine{enter}';

secure upload [to <machine>] = PSFTP(When($1,$1,PWork()));


  # {alt+f4} doesn't work for command prompt windows:
Close()    := Window.Close()
	      If(Window.Success(), "", SendSystemKeys({alt+space}c));

  # override global command in _any_windows.vcl/windows.vch:
close [the] window = Close();



## 
## Cygwin window (SEE ALSO mintty.vcl):
## 

  # fallback method in case "new Cygwin xterm" isn't working:  <<<>>>
create Cygwin xterm = "tcsh{enter} setenv DISPLAY localhost:0.0{enter}"
       "xterm "
       "-fa "DejaVu Sans Mono" -fs 10 "
       "-title 'Cygwin xterm' -n 'Cygwin xterm' "
       "-e '" Cygwin() "/bin/tcsh -l'{enter}"
       WaitForWindow("Cygwin xterm")
    ;
