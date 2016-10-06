###
### Commands to manipulate dialogue windows (e.g., "open/save file" windows)
###

include "locale_PC.vch";


### 
### Commands for "open/save file" dialog boxes:
### 

## 
## Commands for navigating around filesystems
## 
## This is a direct copy of navigate.vch grammar with renamed command prefixes.
## 

include "directories.vch"; 

  # we are careful to preserve any existing file name here:
CD(pathname)   := {end} " " {ctrl+shift+home} {ctrl+c} {backspace}
		  $pathname {enter} {backspace}
		  {ctrl+v} {backspace};

UP(count)      := CD(. Repeat($count, \..) );
DOWN(pathname) := CD($pathname);


#
# Change to given absolute directory:
#

dialogue [<D>] <UNIX> [/ <COM>] = CD(UNIX($1$2 When($3,/$3)));
dialogue       <PC>   [/ <COM>] = CD(PC  (  $1 When($2,/$2)));


#
# Go down one or more directory components:
#

dialogue down <COM> [/ <COM>] = DOWN(UNIX($1 When($2,/$2)));


#
# Go up one or more directories:
#

dialogue up [1..10] = UP(When($1,$1,1));


## 
## Fallback for weird dialog boxes (e.g., Firefox's where to save dialog box)
## 

PREFIX(pathname) := {home} $pathname\ {end};

dialogue prefix [with] [<D>] <UNIX> [/ <COM>] = PREFIX(UNIX($1$2 When($3,/$3)));
dialogue prefix [with]       <PC>   [/ <COM>] = PREFIX(PC  (  $1 When($2,/$2)));


## 
## Bump version number in filenames like <name>_<\d+>\..{3-4}
## 

include "numbers.vch";

last version was <my0to99> = 
    Clipboard.Set("") {end} {shift+left_4}{ctrl+c} Wait(0)
    If(EQ(Left(Clipboard.Get(),1),"."),
       {end}{backspace_4},
       {shift+left}{ctrl+c} Wait(0)
       If(EQ(Left(Clipboard.Get(),1),"."),
            {end}{backspace_5},
	    {end} Beep() Vocola.Abort()
       )
    )
    Repeat(Len($1), {backspace}) Eval($1 + 1);



### 
### Commands for print dialog boxes:
### 
### Overridden in:
### 
###   office.vch, UNIX_shell.vcl, acrord32.vcl
### 

include "printers.vch";

SetPrinter(printer) := HeardWord(Name) {alt+down} $printer {enter};


  # this works for some programs but not WordPad, Win32Pad:
set print <printer_name> = SetPrinter($1);

  # WordPad, Win32Pad: <<<>>>
alternative set print <printer_name> = $1;
