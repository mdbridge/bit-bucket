### 
### Commands to pull up Windows Explorer windows from anywhere
### 

include "locale_PC.vch";


##
## Switch to (new) Explorer window viewing given directory:
##

# "folder window" for an existing folder window via _switch.vcl


include "letters.vch";
include "directories.vch"; 

CD(pathname) := AppBringUp("explorer@" $pathname, "$pathname");

folder [<D>] <UNIX> [/ <COM>] = CD(UNIX($1$2 When($3,/$3)));
folder       <PC>   [/ <COM>] = CD(PC  (  $1 When($2,/$2)));

folder drive <letter>         = CD(PC  (  $1:/));

# "open my computer" for multiple drive view in pre-Windows 10;
# use "folder This PC" in Windows 10

  # "open settings" is captured by FreeSR
open computer settings = Keys.SendInput({win+i});


## 
## Closing multiple Explorer windows:
## 

include "switch.vch";

  # <<<>>>
AwaitChange(actions) :=
    Variable.Set(:target, Window.ID())
    $actions
    Repeat(50, 
        If(Window.Match(ID> Variable.Get(:target)), Wait(100)));

close all (folder|Explorer) windows =
    Repeat(10, 
        SwitchTo2(FolderPattern(), Vocola.Abort()) 
	AwaitChange({alt+f4}))
    Beep();
