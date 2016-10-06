###
### Global voice commands for displaying parts of the Dragon help pages 
### 
###     These commands require that you have Dragon NaturallySpeaking
### Professional or another version that supports advanced scripting
### commands.
### 

## 
##     These macros need to be told where the relevant Dragon HTML
## help file lives.  Unfortunately, the location of this file appears
## to vary between versions of Dragon.  The most likely locations are
## included below; uncomment out only the version that appears to
## apply to you.  If that fails, you may have to hunt around to find a
## version of dragon_enx.chm (there may be several) that has a top
## level topic about creating commands with the MyCommand Editor.
## 

include "locale_PC.vch";

#HelpLocation() := PF32() "\Nuance\NaturallySpeaking10\Help\enx\power\";
#HelpLocation() := PF32() "\Nuance\NaturallySpeaking11\Help\enx\professional\";
HelpLocation() := PF32() "\Nuance\NaturallySpeaking12\Help\enx\professional\";
  # this isn't enough as they have changed the help to HTML:<<<>>> DPI 14
#HelpLocation() := PF32() "\Nuance\NaturallySpeaking14\Help\enx\professional\";



Help(topic) := HTMLHelp(HelpLocation() dragon_enx.chm,
			"HH_DISPLAY_TOPIC", $topic);


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

    key names           = key_names
);

show help for <command> = Help("scrptref/$1.htm");


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
