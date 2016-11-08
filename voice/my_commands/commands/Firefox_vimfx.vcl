###
### Voice commands for Firefox using the VimFx Firefox extension
###
###
### Requires VimFx Firefox extension
###
### Configure via:
###
###    hints chars: 012345678 9
###    blacklist: *
###

## 
## Clicking links/gizmos:
## 

OverlayBlur()   := {shift+f1}{esc} Wait(100);
OverlayHintFocus(operation, d_1, d_2, d_3, d_4) :=
    $d_1 $d_2 $d_3 $d_4 Wait(100) $operation;


overlay blur me = OverlayBlur();

[show]	  hints = OverlayBlur() {shift+f1}zf;
  # use with go pick for multple link opens:
multi	  hints = OverlayBlur() {shift+f1}af; 
browser	  hints = OverlayBlur() {shift+f1}zF;
  # use go pick to position caret and enter caret mode:
caret	  hints = OverlayBlur() {shift+f1}v; 
clickable hints = OverlayBlur() {shift+f1}f;
all	  hints = OverlayBlur() {shift+f1}zf Wait(100) {ctrl+enter};

new next     = {shift+f1} ']';
new previous = {shift+f1} '[';
up a level   = {shift+f1} gu;

overlay <opick> 0..9 [0..9 [0..9 [0..9]]] = OverlayHintFocus($1, $2, $3, $4, $5);

<opick> := (        pick = {enter} 
           | proceed     = {enter} # better recognized than just pick by itself
           | go     pick = ""
           | push   pick = {ctrl+enter}    # stay but open new tab w/ link
           | tab    pick = {ctrl+shift+enter}
           | window pick = {shift+enter}

           | menu   pick = {shift+f10}
           | save   pick = {shift+f10} Wait(100) k
           | copy   pick = {shift+f10} Wait(100) a # copy URL of link

           | drop   pick = {enter}{alt+down}
 
	   | HP     pick =  Wait(500) mark.lillibridge@hpe.com{tab}
           | radio  pick = {space} Wait(500) {shift+f1}zf
           | multi  pick = {ctrl+enter} Wait(500) {shift+f1}zf
           );
