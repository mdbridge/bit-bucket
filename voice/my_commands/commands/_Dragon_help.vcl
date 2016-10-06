###
### Global voice commands for displaying parts of the Dragon help pages 
### 

include "URLs.vch";


## 
## The DNS command "open help" will open up your help documentation if
## you are looking for information not provided by the below shortcuts.
## 
## {alt+space}j will show you the URL for the current help topic.
## 


## 
## Help for procedures available through Vocola
## 

# 
# Dragon calls for which Dragon documentation is available:
# 
<command> := ( 
    Active Control Pick = activecontrolpick |
    Active Menu Pick    = activemenupick    |
    App Bring Up        = appbringup        |
    App Swap With       = appswapwith       |
    Beep                = beep              |
    Button Click        = buttonclick       |
    Clear Desktop       = cleardesktop      |
    Control Pick        = controlpick       |
    DDE Execute         = ddeexecute        |
    DDE Poke            = ddepoke           |
    DLL Call            = dllcall           |
    Drag To Point       = dragtopoint       |
    Go To Sleep         = gotosleep         |
    Heard Word          = heardword         |
    HTML Help           = htmlhelp          |
    Menu Cancel         = menucancel        |
    Menu Pick           = menupick          |
    Mouse Grid          = mousegrid         |
    message Box Confirm = msgboxconfirm     |
    Play Sound          = playsound         |
    Remember Point      = rememberpoint     |
    Run Script File     = runscriptfile     |
    Send Keys           = sendkeys          | # equivalent to SendDragonKeys
    Send Dragon Keys    = sendkeys          | # !?!
    Send System Keys    = sendsystemkeys    |
    Set Microphone      = setmicrophone     |
    Set Mouse Position  = setmouseposition  |
    Set Natural Text    = setnaturaltext    |
    Shell Execute       = shellexecute      |
    TTS Play String     = ttsplaystring     |
    Wait                = wait              |
    Wait For Window     = waitforwindow     |
    Wake Up             = wakeup            |
    Win Help            = winhelp           |

      # Dragon scripting language extensions quick reference:
    scripting           = scripting_language_quickref |
    key names           = key_names
);

show help for <command> = 
    Lookup("http://www.nuance.com/products/help/dragon/dragon-for-pc/scriptref/Content/scrptref/$1.htm");


<command2> := ( 
    Eval                = Eval              |
    Eval Template       = EvalTemplate      |
    If                                      |
    Repeat              = Repeat            |
    Shift Key           = ShiftKey          | # undocumented Dragon call
    Unimacro                                |
    When
);

show help for <command2> = 
        AppBringUp("lookup", "http://vocola.net/v2/BuiltinFunctions.asp#$1");


  # stuff usable in advanced scripting commands:
show help for advanced scripting = 
    Lookup(http://www.nuance.com/products/help/dragon/dragon-for-pc/scriptref/Content/vbs/speechlinks_basic_language.htm);

  # overall starting page:
show Dragon web help =
    Lookup(http://www.nuance.com/products/help/dragon/dragon-for-pc/scriptref/Content/GetStart1.htm);
