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
