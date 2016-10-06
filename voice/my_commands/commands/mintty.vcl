### 
### Voice commands for Cygwin terminal
### 

include "environment.vch";


paste that = {shift+Ins};



## 
## Window manipulation commands:
## 

System(key) := {alt+space};

include "windows.vch";



## 
## Cygwin window:
## 

  # fallback method in case "new Cygwin xterm" isn't working: <<<>>>
create Cygwin xterm = "tcsh{enter} setenv DISPLAY localhost:0.0{enter}"
       "xterm "
       "-fa "DejaVu Sans Mono" -fs 10 "
       "-title 'Cygwin xterm' -n 'Cygwin xterm' "
       "-e '" Cygwin() "/tcsh -l'{enter}"
       WaitForWindow("Cygwin xterm")
    ;
