### 
### Miscellaneous global voice commands
### 
###   These are not targeted at the current application.
### 

## 
## Controlling system sound:
## 

Mute() := Keys.SendInput({VolumeMute});  # toggles all sound on/off

mute sound           = Mute();

loudness 0..100 = Keys.SendInput({VolumeDown_50}{VolumeUp_ Eval($1/2)});

loudness (Up|Down) 0..100 = Keys.SendInput({Volume$1_ Eval($2/2)});

#toggle sound = ShellExecute("C:\Windows\System32\SndVol.exe")
#               WaitForWindow("Volume Mixer") {tab}{space} {alt+f4};


(top=up|bottom=down) sound = ShellExecute("mmsys.cpl") WaitForWindow("Sound") 
		             {down} {$1_6} {alt+s} {enter};

media ( next [track]=NextTrack | previous [track]=previousTrack
      | play=PlayPause | pause=PlayPause | stop=Stop ) =
    Keys.SendInput({Media$1});



## 
## Informational questions:
## 

tell me the time       = TTSPlayString( Date.Now("the time is %I:%M %p") );
tell me the (day|date) = TTSPlayString( Date.Now() );



## 
## Interacting with other people:
## 

start listening = Mute() HeardWord(go, to, sleep);

Tell (Laura|cat|Will|visitor) <_anything> = Beep();

      pause now = SetMicrophone(0) Wait( 5000) TTSPlayString(ready) SetMicrophone(1);
short pause now = SetMicrophone(0) Wait(10000) TTSPlayString(ready) SetMicrophone(1);

bad computer = HeardWord(scratch, that) TTSPlayString(sorry);



## 
## Other:
## 

Windows search = SendSystemKeys({win+s});  # search for a file or folder


include "switch.vch";

  # puts current window title in clipboard, use ^c to copy everything:
show window titles =  AutoHotkeyAsync0(window_info);



## 
## Experiments:
## 

include "letters.vch";

set shortcut <letter> = Variable.Set("shortcut:$1", Clipboard.Get());
    shortcut <letter> = Variable.Get("shortcut:$1");
