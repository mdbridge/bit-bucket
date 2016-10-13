###
### Global voice commands not yet assigned to a file
###

#include "letters.vch";
#include "numbers.vch";
include "control.vch";
include "extended_string.vch";

#################################################################

  # Unfortunately, cannot adjust volume without losing utterance buffer:
media playback = 
    Keys.SendInput({MediaPlayPause})
    HeardWord(play, that, back) Wait(300) 
    Keys.SendInput({MediaPlayPause});


  # attempt to fix misrecognized DNS command:
exit Dragon = exit{enter};


# <<<>>>
include "machines.vch";
include "switch.vch";

new <machine> putty = SwitchToApp("new " ShortMachine($1) ".* PuTTY", 
    PCfromPC(~pf32/PuTTy/putty.exe) " -load no-password -l " User($1) " $1");
new <machine> putty = SwitchTo("new "ShortMachine($1) ".* PuTTY");
