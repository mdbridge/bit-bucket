### 
### Voice commands for switching to a different application than the
### current one and possibly executing some commands.
### 
### Special cases are in:
### 
###   _acrord32.vcl
###   _explorer.vcl
### 

include "locale_PC.vch";
include "switch.vch";
include "letters.vch";


### 
### Generic access methods:
### 

## via FreeSR:

# [Freesr] show window #
# [Freesr] Taskbar #
# [Freesr] Right Click Taskbar #


## via phrase in window title:

window with <_anything> = SwitchTo($1);


## via DNS list dialog box:

choose window   = HeardWord(list, all, Windows);
choose fire     = HeardWord(list, Windows, for, Firefox);
choose explorer = HeardWord(list, windows, for, Windows, Explorer);


## via searching from start menu:

Start() := SendSystemKeys({ctrl+esc});

start menu <letter> [<letter>] = Start() $1$2;
start menu phrase <_anything>  = Start() $1;


## via alt-tab:

  # use arrow keys then enter/escape:
      alt tab select = SendSystemKeys({ctrl+alt+tab});

## via win-tab:

  # use arrow keys/tab then enter/escape:
      window tab select = SendSystemKeys({win+tab});


## Switch to the program that last displayed a message in the
## notification area: <<<>>>

notification window = SendSystemKeys({win+ctrl+b});



### 
### Shell windows:
### 

# start Cygwin terminal

  # need /bin/run to avoid extra console window hanging around, but
  # /bin/run can't seem to handle arguments with (quoted) spaces so
  # indirect via a batch file:
  #  (Using Async doesn't work any better)
new Cygwin xterm = Subprocess.System(PC(Cygwin() \run) " -p /usr/bin "
                                       PC(~/bin/start_Cygwin.bat));


Do(command) :=  {esc}x $command {enter};
# Specialize Save() to save the current buffer if it is visiting a file:
Save() := If(Window.Match("emacs"), Do(mdl-save-visiting-buffer));

include "shell_windows.vch";

# also see blue area {paste|dump} <range>



### 
### Controlling non-parameterized applications:
### 

## 
## Creating/switching to application [windows]:
## 

# open run dialog
# open control panel

include "applications.vch";

<application> window              = $1;
  # can't bounce back here because of possible confirmation dialog boxes:
<application> window close window = NoCreate($1) {alt+f4};


## 
## Remote controlling applications/dialog boxes that don't have focus:
## 

  # Outlook reminder dialog box:
reminder window (Dismiss|Snooze|Dismiss All|Open Item|Join Online) =
    OutlookReminder() HeardWord(click, $1);

iTunes        [window] (space={space} | skip={ctrl+right}) =
    SwitchTo("^iTunes$") $1 Wait(200)           {alt+space}n OldWindow();

media player  [window] (pause|play) =   
    SwitchTo("^Windows Media Player$") {ctrl+p} {alt+space}n OldWindow();


  # RSIGuard rest break dialog window:
RestBreak() := SwitchTo("(Taking a rest break)|(^Exceeded)|(^Keyboard exposure)|(^Mouse exposure)");

postpone (two=2|ten=1) minutes = RestBreak() $1;
skip break                     = RestBreak() s;


## 
## Miscellaneous cases:
## 

invoke login  = AppBringUp(login, PC(~/login_ IfHome(home,work) .bat));



### 
### Launching background processes:
### 

launch X = Subprocess.Async(PC(Cygwin() \XWin.exe), XWin.exe,
                            -ac, -multiwindow, -clipboard);
